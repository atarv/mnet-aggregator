{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module MnetWatcher (runApp) where

import           Announcement
import           AnnouncementScraper
import           Configs                        ( baseUrl )
import           Control.Applicative
import           Control.Exception
import           Data.Time
import           HTMLRenderer
import           Text.HTML.Scalpel
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Text.Regex.TDFA               as RegexTDFA

announcementFile :: FilePath
announcementFile = "seenAnnouncements.txt"

saveSeenAnnouncementIds :: [Announcement] -> IO ()
saveSeenAnnouncementIds =
    T.appendFile announcementFile . T.unlines . map announcementId

loadSeenAnnouncementIds :: IO (Set.Set T.Text)
loadSeenAnnouncementIds = catch
    (Set.fromList . T.lines <$> T.readFile announcementFile)
    (\(_ :: IOException) -> return Set.empty)

filterSeenAnnouncements :: Set.Set T.Text -> [Announcement] -> [Announcement]
filterSeenAnnouncements seen =
    filter (not . flip Set.member seen . announcementId)

runApp :: IO ()
runApp = do
    res <- scrapeAnnouncements
        (baseUrl <> "/tori/?type=sell&province=Keski-Suomi&category=20")
    time <- formatTime defaultTimeLocale "%-d.%-m.%Y %-R" <$> getCurrentTime
    let title = "Uusia ilmoituksia " <> time
    case res of
        Just anns -> do
            seen <- loadSeenAnnouncementIds
            let newAnnouncements = filterSeenAnnouncements seen anns
            writeFile "out.html" $ announcementsToHtml title newAnnouncements
            saveSeenAnnouncementIds newAnnouncements
        Nothing -> error "Error: failed to scrape"
