{-|
Module         : MnetAggregator
Description    : Most of the application's logic resides here. This module is
                 responsible for connecting the services.
Copyright      : (c) Aleksi Tarvainen, 2020
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module MnetAggregator (scrapeAndReport) where

import           Listing
import           ListingScraper
import           Configs
import           Control.Monad
import           Control.Concurrent
import           Data.Time
import           Database
import           HTMLRenderer
import           Mailer
import           ScrapingOptions
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import           Data.Either                    ( fromLeft
                                                , isLeft
                                                )

-- | Listings whose ids are in the set of seen ids are filtered out
filterOutSeenListings
    :: [ListingId] -- ^ List of seen listing ids
    -> [Listing] -- ^ Listings to filter
    -> [Listing] -- ^ Return only listings yet to be seen
filterOutSeenListings seenIds =
    filter (not . flip Set.member (Set.fromList seenIds) . listingId)

-- | Scrape a single listing section
scrapeSection :: Section -> IO (T.Text, Maybe [Listing])
scrapeSection Section {..} =
    (,) sectionTitle <$> scrapeListings (T.unpack sectionUrl)

-- | Scrape given sections, generate a report out of them and send it
scrapeAndReport :: AppConfig -> ScrapingOptions -> IO ()
scrapeAndReport conf opts = unless (null $ sections opts) $ do
    -- Report is generated only if there are sections to scrape
    conn            <- connect (databaseConfig conf)
    scrapedSections <- mapM
        (if False -- rtsSupportsBoundThreads
            then runInBoundThread . scrapeSection
            else scrapeSection
        )
        (sections opts)
    -- Filter out all empty and failed scraped sections and generate HTML from 
    -- them
    let sectionsWithContent =
            [ (t, a) | (t, Just a) <- scrapedSections, (not . null) a ]
    sectionsHtml <- forM sectionsWithContent $ \(title, announcements) -> do
        resultSeenIds <- getUsersSeenListings conn (recipientEmail opts)
        seenIds       <- case resultSeenIds of
            Right ids -> pure ids
            Left  err -> fail . T.unpack $ err
        let newListings = filterOutSeenListings seenIds announcements
        stored <- storeSeenListings conn
                                    (recipientEmail opts)
                                    (fmap listingId newListings)
        -- We check that `stored` is `Left`, so empty string here is unreachable
        when (isLeft stored) $ fail . T.unpack $ fromLeft "" stored
        pure $ sectionToHtml title newListings
    -- Generate and send report of new announcements
    time <- formatTime defaultTimeLocale "%-d.%-m.%Y %-R" <$> getZonedTime
    let mailTitle = "Raportti " <> time
        mailHtml  = sectionsToHtml mailTitle (concat sectionsHtml)
    sendListingMail (mailConfig conf) opts (LT.pack mailHtml)
    disconnect conn
