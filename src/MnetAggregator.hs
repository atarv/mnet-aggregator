{-|
Module         : MnetAggregator
Description    : Most of the application's logic resides here. This module is
                 responsible for connecting the services.
Copyright      : (c) Aleksi Tarvainen, 2020
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings
  , ScopedTypeVariables
  , RecordWildCards
  , TupleSections 
  , LambdaCase
#-}
module MnetAggregator (scrapeAndReport) where

import           Listing                        ( Listing(listingId) )
import           ListingScraper                 ( scrapeListings )
import           Configs
import           Control.Monad
import           Control.Concurrent             ( )
import           Control.Concurrent.Async       ( mapConcurrently )
import           Control.Exception
import           Data.Time
import           Database
import           HTMLRenderer                   ( sectionToHtml
                                                , sectionsToHtml
                                                , errorSectionToHtml
                                                )
import           Mailer                         ( sendListingMail )
import           ScrapingOptions
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT


data MnetAggregatorException = NoSections String deriving Show

instance Exception MnetAggregatorException

-- | Listings whose ids are in the set of seen ids are filtered out
filterOutSeenListings
    :: [ListingId] -- ^ List of seen listing ids
    -> [Listing] -- ^ Listings to filter
    -> [Listing] -- ^ Return only listings yet to be seen
filterOutSeenListings seenIds =
    filter (not . flip Set.member (Set.fromList seenIds) . listingId)

-- | Scrape a single listing section
scrapeSection :: Section -> IO (T.Text, Either SomeException [Listing])
scrapeSection Section {..} =
    (sectionTitle, ) <$> scrapeListings (T.unpack sectionUrl)

-- | Scrape given sections, generate a report out of them and send it
scrapeAndReport :: AppConfig -> ScrapingOptions -> IO ()
scrapeAndReport AppConfig {..} opts@ScrapingOptions {..} = do
    when (null sections) (throw $ NoSections "No sections to scrape")
    bracket (connect databaseConfig) disconnect $ \conn -> do
        scrapedSections <- mapConcurrently scrapeSection sections
        sectionsHtml    <- forM scrapedSections $ \(title, scrapeResult) ->
            case scrapeResult of
                Left err -> pure $ errorSectionToHtml
                    title
                    (T.pack $ displayException err)
                Right listings -> do
                    newListings <- diffListingsWithSeen conn listings
                    void $ storeListingIds conn newListings
                    pure $ sectionToHtml title newListings
        -- Generate and send report of new listings
        time <- formatTime defaultTimeLocale "%-d.%-m.%Y %-R" <$> getZonedTime
        let mailTitle = "Raportti " <> time
            mailHtml  = sectionsToHtml mailTitle (concat sectionsHtml)
        sendListingMail mailConfig opts (LT.pack mailHtml)
  where
    -- Remove seen listings (which are stored in a database)
    diffListingsWithSeen conn listings =
        getUsersSeenListings conn recipientEmail >>= \case
            Left  err  -> fail $ T.unpack err
            Right seen -> pure $ filterOutSeenListings seen listings
    -- Store given listings' ids to database
    storeListingIds conn listings =
        storeSeenListings conn recipientEmail (listingId <$> listings) >>= \case
            Left  err         -> fail . T.unpack $ err
            Right storedCount -> pure storedCount
