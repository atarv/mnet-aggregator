{-|
Module         : MnetAggregator
Description    : Most of the application's logic resides here. This module is
                 responsible for connecting the services in a meaningful way.
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

-- | Listings whose ids are in the set of seen ids are filtered out
filterOutSeenListings
    :: [T.Text] -- ^ List of seen listing ids
    -> [Listing] -- ^ Listings to filter
    -> [Listing] -- ^ Return only listings yet to be seen
filterOutSeenListings seenIds =
    filter (not . flip Set.member (Set.fromList seenIds) . listingId)

-- | Scrape a single listing section
scrapeSection :: Section -> IO (T.Text, Maybe [Listing])
scrapeSection Section {..} =
    (,) sectionTitle <$> scrapeListings (T.unpack sectionUrl)

-- | Pass the result (value in Right) or fail with text in 'Left err'.
passResultOrFail :: Either T.Text a -> IO a
passResultOrFail (Right val) = pure val
passResultOrFail (Left  err) = fail (T.unpack err)

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
    sectionsHtmls <- forM sectionsWithContent $ \(title, announcements) ->
        getUsersSeenListings conn (recipientEmail opts)
            >>= passResultOrFail
            >>= \seenIds -> do
                    let newListings =
                            filterOutSeenListings seenIds announcements
                    _numStored <- -- ignored
                        storeSeenListings conn
                                          (recipientEmail opts)
                                          (fmap listingId newListings)
                            >>= passResultOrFail
                    pure $ sectionToHtml title newListings
    -- Generate and send report of new announcements
    time <- formatTime defaultTimeLocale "%-d.%-m.%Y %-R" <$> getZonedTime
    let mailTitle = "Raportti " <> time
        mailHtml  = sectionsToHtml mailTitle (concat sectionsHtmls)
    sendListingMail (mailConfig conf) opts (LT.pack mailHtml)
    disconnect conn
