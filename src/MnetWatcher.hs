{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module MnetWatcher (runApp) where

import           Announcement
import           AnnouncementScraper
import           Configs
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Bifunctor
import           Data.Maybe
import           Data.Time
import           HTMLRenderer
import           Mailer
import           System.Exit
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
    -- This happens when file is not found. We can continue like we didn't read
    -- any old announcements
    (\(_ :: IOException) -> return Set.empty)

filterSeenAnnouncements :: Set.Set T.Text -> [Announcement] -> [Announcement]
filterSeenAnnouncements seen =
    filter (not . flip Set.member seen . announcementId)

scrapeSection Section {..} =
    (,) sectionTitle <$> scrapeAnnouncements (T.unpack sectionUrl)

runApp :: IO ()
runApp = do
    conf <- loadConfig Nothing
    when (null $ sectionsToScrape conf) $ do
        print "No sections to scrape"
        exitSuccess
    res <- mapM scrapeSection (sectionsToScrape conf)
    -- Filter out all empty and failed scraped sections
    let sectionsWithContent = filter (\(_, xs) -> not $ null xs)
            $ map (second $ fromMaybe []) res
    sectionsHtmls <- forM sectionsWithContent $ \(title, announcements) -> do
        seen <- loadAnnouncementIds
        let newAnnouncements = filterSeenAnnouncements seen announcements
        saveAnnouncementIds newAnnouncements
        pure $ sectionToHtml title newAnnouncements
    time <- formatTime defaultTimeLocale "%-d.%-m.%Y %-R" <$> getZonedTime
    let emailTitle = "Raportti " <> time
        emailHtml  = sectionsToHtml emailTitle (concat sectionsHtmls)
    sendAnnouncementMail conf (LT.pack emailHtml)
