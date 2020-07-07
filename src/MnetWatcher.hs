{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module MnetWatcher (scrapeAndReport) where

import           Announcement
import           AnnouncementScraper
import           Configs
import           Control.Monad
import           Data.Time
import           Database
import           HTMLRenderer
import           Mailer
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import           ScrapingOptions

filterOutSeenAnnouncements :: [T.Text] -> [Announcement] -> [Announcement]
filterOutSeenAnnouncements seenIds =
    filter (not . flip Set.member (Set.fromList seenIds) . announcementId)

scrapeSection :: Section -> IO (T.Text, Maybe [Announcement])
scrapeSection Section {..} =
    (,) sectionTitle <$> scrapeAnnouncements (T.unpack sectionUrl)

passResultOrFail :: Either T.Text a -> IO a
passResultOrFail (Right val) = pure val
passResultOrFail (Left  err) = fail (T.unpack err)

scrapeAndReport :: AppConfig -> ScrapingOptions -> IO ()
scrapeAndReport conf opts = unless (null $ sections opts) $ do
    -- Report is generated only if there are sections to scrape
    conn            <- connect (databaseConfig conf)
    scrapedSections <- mapM scrapeSection (sections opts)
    -- Filter out all empty and failed scraped sections and generate HTML from 
    -- them
    let sectionsWithContent =
            [ (t, a) | (t, Just a) <- scrapedSections, (not . null) a ]
    sectionsHtmls <- forM sectionsWithContent $ \(title, announcements) ->
        getUsersSeenAnnouncements conn (recipientEmail opts)
            >>= passResultOrFail
            >>= \seenIds -> do
                    let newAnnouncements =
                            filterOutSeenAnnouncements seenIds announcements
                    _numStored <-
                        storeSeenAnnouncements
                                conn
                                (recipientEmail opts)
                                (fmap announcementId newAnnouncements)
                            >>= passResultOrFail
                    pure $ sectionToHtml title newAnnouncements
    -- Generate and send report of new announcements
    time <- formatTime defaultTimeLocale "%-d.%-m.%Y %-R" <$> getZonedTime
    let mailTitle = "Raportti " <> time
        mailHtml  = sectionsToHtml mailTitle (concat sectionsHtmls)
    sendAnnouncementMail (mailConfig conf) opts (LT.pack mailHtml)
    disconnect conn
