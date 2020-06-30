{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module AnnouncementScraper (scrapeAnnouncements) where
import           Announcement
import           Control.Applicative
import           Text.HTML.Scalpel
import qualified Data.Text                     as T
import qualified Text.Regex.TDFA               as RegexTDFA

re :: String -> RegexTDFA.Regex
re = RegexTDFA.makeRegex

announcementScraper :: Scraper T.Text Announcement
announcementScraper = do
    (title, announcementId) <-
        chroot ("td" @: [hasClass "tori_title"] // "a")
        $   (,)
        <$> text (tagSelector "a")
        <*> attr "href" (tagSelector "a")
    description        <- text $ "font" @: [hasClass "msg"]
    (author, authorId) <-
        chroot ("a" @: ["href" @=~ re "/jasenet.*"]) $ (,) <$> text "a" <*> attr
            "href"
            "a"
    -- FIXME: Price doesn't get scraped properly
    price      <- T.concat <$> innerHTMLs "p"
    thumbnails <- attrs "src" $ "img" @: [hasClass "border"]
    dates      <- text $ "small" @: [hasClass "light"]
    return $ Announcement { .. }

announcements :: Scraper T.Text [Announcement]
announcements = chroots ("table" @: ["cellpadding" @= "2"]) announcementScraper

scrapeAnnouncements :: URL -> IO (Maybe [Announcement])
scrapeAnnouncements url = scrapeURL url announcements
