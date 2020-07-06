{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module MnetWatcher (runApp) where

import           Announcement
import           AnnouncementScraper
import           Configs
import           Control.Monad
import           Data.Maybe
import           Data.Time
import           Database
import           HTMLRenderer
import           Mailer
import           System.Exit
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT

filterSeenAnnouncements :: [T.Text] -> [Announcement] -> [Announcement]
filterSeenAnnouncements seenIds =
    filter (not . flip Set.member (Set.fromList seenIds) . announcementId)

scrapeSection :: Section -> IO (T.Text, Maybe [Announcement])
scrapeSection Section {..} =
    (,) sectionTitle <$> scrapeAnnouncements (T.unpack sectionUrl)

passResultOrFail :: Either T.Text a -> IO a
passResultOrFail (Right val) = pure val
passResultOrFail (Left  err) = fail (T.unpack err)

runApp :: IO ()
runApp = do
    conf <- loadConfig Nothing
    when (null $ sectionsToScrape conf) $ do
        putStrLn "No sections to scrape"
        exitSuccess
    conn <- connect (databaseConfig conf)
    scrapeAndReport conf conn
  where
    scrapeAndReport conf conn = do
        scrapedSections <- mapM scrapeSection (sectionsToScrape conf)
        -- Filter out all empty and failed scraped sections
        let sectionsWithContent = filter (\(_, xs) -> not $ null xs)
                $ map (fmap $ fromMaybe []) scrapedSections
        sectionsHtmls <- forM sectionsWithContent $ \(title, announcements) ->
            getUsersSeenAnnouncements conn (recipientEmail conf)
                >>= passResultOrFail
                >>= \seenIds -> do
                        let newAnnouncements =
                                filterSeenAnnouncements seenIds announcements
                        storeSeenAnnouncements
                                conn
                                (recipientEmail conf)
                                (fmap announcementId newAnnouncements)
                            >>= passResultOrFail
                            >>= \n ->
                                    putStrLn
                                        $ show n
                                        <> " announcements stored from section: "
                                        <> T.unpack title
                        pure $ sectionToHtml title newAnnouncements
        -- Generate and send report of new announcements
        time <- formatTime defaultTimeLocale "%-d.%-m.%Y %-R" <$> getZonedTime
        let emailTitle = "Raportti " <> time
            emailHtml  = sectionsToHtml emailTitle (concat sectionsHtmls)
        sendAnnouncementMail conf (LT.pack emailHtml)
        disconnect conn
