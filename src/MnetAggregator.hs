{-|
Module         : MnetAggregator
Description    : Most of the application's logic resides here. This module is
                 responsible for connecting the services.
Copyright      : (c) Aleksi Tarvainen, 2020
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module MnetAggregator (scrapeAndReport) where

import qualified Amazonka
import           Configs
import           Control.Concurrent       ()
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception
import           Control.Monad
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import           Data.Time
import           Database
import           HTMLRenderer             (errorSectionToHtml, sectionToHtml,
                                           sectionsToHtml)
import           Listing                  (Listing (listingId))
import           ListingScraper           (scrapeListings)
import           Mailer                   (sendListingMail)
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
scrapeSection :: Section -> IO (T.Text, Either SomeException [Listing])
scrapeSection Section {..} =
    (sectionTitle, ) <$> scrapeListings (T.unpack sectionUrl)

-- | Scrape given sections, generate a report out of them and send it
scrapeAndReport :: Amazonka.Env -> AppConfig -> ScrapingOptions -> IO ()
scrapeAndReport awsEnv AppConfig {..} opts@ScrapingOptions {..} = do
    when (null sections) (throw $ NoSections "No sections to scrape")
    scrapedSections <- mapConcurrently scrapeSection sections
    sectionsHtml    <- forM scrapedSections $ \(sectionTitle, scrapeResult) ->
        case scrapeResult of
            Left err -> do
                putStrLn $ "Failed to scrape section '"
                    <> T.unpack sectionTitle
                    <> "': "
                    <> show err
                pure $ errorSectionToHtml
                    sectionTitle
                    (T.pack $ displayException err)
            Right listings -> do
                newListings <- diffListingsWithSeen sectionTitle listings
                pure $ sectionToHtml sectionTitle newListings
    -- Generate and send report of new listings
    time <- formatTime defaultTimeLocale "%-d.%-m.%Y %-R" <$> getZonedTime
    let mailTitle = "Raportti " <> time
        mailHtml  = sectionsToHtml mailTitle (concat sectionsHtml)
    sendListingMail mailConfig opts (LT.pack mailHtml)
  where
    -- Remove seen listings (which are stored in a database) from the list.
    -- Listings are stored to database during this process.
    diffListingsWithSeen title listings = do
        storedIds <- storeListings awsEnv dynamoDBTableName recipientEmail title listings
        pure $ filterOutSeenListings storedIds listings
