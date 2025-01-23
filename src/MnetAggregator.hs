{-|
Module         : MnetAggregator
Description    : Most of the application's logic resides here. This module is
                 responsible for connecting the services.
Copyright      : (c) Aleksi Tarvainen, 2025
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module MnetAggregator (scrapeReport, sendReport) where

import qualified Amazonka
import           Configs
import           Control.Concurrent       ()
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception
import           Control.Monad
import           Data.Bifunctor
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy           as LT
import           Data.Time
import           Database
import           GHC.Stack                (HasCallStack)
import           HTMLRenderer             (errorSectionToHtml, sectionToHtml,
                                           sectionsToHtml)
import           Listing                  (Listing (listingId))
import           ListingScraper           (scrapeListings)
import           Mailer                   (sendReportMail)
import           Report
import           ScrapingOptions

newtype MnetAggregatorException = NoSections String deriving Show

instance Exception MnetAggregatorException

-- | Listings whose ids are in the set of seen ids are filtered out
filterOutSeenListings
    :: [ListingId] -- ^ List of seen listing ids
    -> [Listing] -- ^ Listings to filter
    -> [Listing] -- ^ Return only listings yet to be seen
filterOutSeenListings seenIds =
    filter (not . (`Set.member` Set.fromList seenIds) . listingId)

-- | Scrape a single listing section
scrapeSection :: HasCallStack
    => SectionParams
    -> IO (T.Text, Either SomeException [Listing])
scrapeSection SectionParams {..} =
    (sectionTitle, ) <$> scrapeListings (T.unpack sectionUrl)

-- | Scrape given sections, generate a report out of them and send it
scrapeReport :: HasCallStack
    => ScrapingOptions
    -> IO Report
scrapeReport opts@ScrapingOptions {..} = do
    when (null sections) (throw $ NoSections "No sections to scrape")
    scrapedSections <- mapConcurrently scrapeSection sections
    let reportSections = fmap
            (\(title, sects) -> ReportSection title (first (T.pack . show) sects))
            scrapedSections
    pure $ Report
        { reportSections = reportSections
        , reportRecipientName = recipientName
        , reportRecipientEmail = recipientEmail
        }

sendReport :: HasCallStack
    => Amazonka.Env
    -> AppConfig
    -> Report
    -> IO ()
sendReport aws config report = do
    sectionsHtml    <- forM (Report.reportSections report) $ \sect ->
        let sectionTitle = title sect
        in case listings sect of
            Left err -> do
                T.putStrLn $ "Failed to scrape section '"
                    <> sectionTitle
                    <> "': "
                    <> err
                pure $ errorSectionToHtml
                    sectionTitle
                    err
            Right listings -> do
                newListings <- diffListingsWithSeen sectionTitle listings
                pure $ sectionToHtml sectionTitle newListings
    -- Generate and send report of new listings
    time <- formatTime defaultTimeLocale "%-d.%-m.%Y %-R" <$> getZonedTime
    let mailTitle = "Raportti " <> time
        mailHtml  = sectionsToHtml mailTitle (concat sectionsHtml)
    sendReportMail aws (mailConfig config) report (LT.pack mailHtml)
  where
    -- Remove seen listings (which are stored in a database) from the list.
    -- Listings are stored to database during this process.
    diffListingsWithSeen title listings = do
        storedIds <- storeListings
            aws
            (dynamoDBTableName config)
            (reportRecipientEmail report)
            title
            listings
        pure $ filterOutSeenListings storedIds listings
