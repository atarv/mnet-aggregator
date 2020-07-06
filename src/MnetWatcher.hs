{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module MnetWatcher (runApp) where

import           Announcement
import           AnnouncementScraper
import           Configs
import           Control.Monad
import           Data.Bifunctor
import           Data.Maybe
import           Data.Time
import           Database
import           HTMLRenderer
import           Mailer
import           System.Exit
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.IO                  as TIO

filterSeenAnnouncements :: [T.Text] -> [Announcement] -> [Announcement]
filterSeenAnnouncements seenIds =
    filter (not . flip Set.member (Set.fromList seenIds) . announcementId)

scrapeSection :: Section -> IO (T.Text, Maybe [Announcement])
scrapeSection Section {..} =
    (,) sectionTitle <$> scrapeAnnouncements (T.unpack sectionUrl)

passResultOrExitFailure :: Either T.Text a -> IO a
passResultOrExitFailure (Right val) = pure val
passResultOrExitFailure (Left  err) = do
    TIO.putStrLn err
    -- FIXME: use exceptions instead of exiting, could fail straight from db 
     -- calls
    exitFailure

runApp :: IO ()
runApp = do
    conf <- loadConfig Nothing
    conn <- connect (databaseConfig conf)
    when (null $ sectionsToScrape conf) $ do
        putStrLn "No sections to scrape"
        exitSuccess

    scrapedSections <- mapM scrapeSection (sectionsToScrape conf)
    -- Filter out all empty and failed scraped sections
    let sectionsWithContent = filter (\(_, xs) -> not $ null xs)
            $ map (second $ fromMaybe []) scrapedSections
    sectionsHtmls <- forM sectionsWithContent $ \(title, announcements) -> do
        res <- getUsersSeenAnnouncements conn (recipientEmail conf)
        passResultOrExitFailure res >>= \seenIds -> do
            let newAnnouncements =
                    filterSeenAnnouncements seenIds announcements
            stored <- storeSeenAnnouncements
                conn
                (recipientEmail conf)
                (fmap announcementId newAnnouncements)
            passResultOrExitFailure stored >>= \n ->
                putStrLn
                    $  show n
                    <> " announcements stored from section: "
                    <> T.unpack title
            pure $ sectionToHtml title newAnnouncements

    time <- formatTime defaultTimeLocale "%-d.%-m.%Y %-R" <$> getZonedTime
    let emailTitle = "Raportti " <> time
        emailHtml  = sectionsToHtml emailTitle (concat sectionsHtmls)
    sendAnnouncementMail conf (LT.pack emailHtml)
