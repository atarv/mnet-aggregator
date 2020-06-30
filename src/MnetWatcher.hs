{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module MnetWatcher (runApp) where

import           Announcement
import           AnnouncementScraper
import           Configs
import           Control.Applicative
import           Control.Exception
import           Data.Time
import           HTMLRenderer
import           Mailer
import           Text.HTML.Scalpel
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.IO                  as T
import qualified Text.Regex.TDFA               as RegexTDFA

announcementFile :: FilePath
announcementFile = "seenAnnouncements.txt"

saveAnnouncementIds :: [Announcement] -> IO ()
saveAnnouncementIds =
    T.appendFile announcementFile . T.unlines . map announcementId

loadAnnouncementIds :: IO (Set.Set T.Text)
loadAnnouncementIds = catch
    (Set.fromList . T.lines <$> T.readFile announcementFile)
    (\(_ :: IOException) -> return Set.empty)

filterSeenAnnouncements :: Set.Set T.Text -> [Announcement] -> [Announcement]
filterSeenAnnouncements seen =
    filter (not . flip Set.member seen . announcementId)

runApp :: IO ()
runApp = do
    conf <- loadConfig Nothing
    -- TODO: Scrape multiple URLs at a time and send them combined as email
    -- User could also have section titles for each URL that would show up in 
    -- in mail
    res <- scrapeAnnouncements (T.unpack $ head $ scrapeUrls conf)
    case res of
        Just []   -> putStrLn "No announcements scraped"
        Just anns -> do
            seen <- loadAnnouncementIds
            let newAnnouncements = filterSeenAnnouncements seen anns
            if null newAnnouncements
                then putStrLn "No new announcements"
                else do
                    time <-
                        formatTime defaultTimeLocale "%-d.%-m.%Y %-R"
                            <$> getCurrentTime
                    let title    = "Uusia ilmoituksia " <> time
                    let annsHtml = announcementsToHtml title newAnnouncements
                    sendAnnouncementMail conf (LT.pack annsHtml)
                    saveAnnouncementIds newAnnouncements
        Nothing -> error "Error: failed to scrape"
