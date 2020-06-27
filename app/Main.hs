{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import           Text.HTML.Scalpel
import           Control.Applicative
import qualified Text.Regex.TDFA               as RegexTDFA
import qualified Data.Text                     as T

re :: String -> RegexTDFA.Regex
re = RegexTDFA.makeRegex

baseUrl = "https://muusikoiden.net"

data Announcement =
    Announcement { title :: T.Text, announcementId :: T.Text
                 , description :: T.Text, author :: T.Text, authorId :: T.Text
                 , price :: T.Text, thumbnails :: [T.Text], dates :: T.Text
                 }
    deriving (Show)

instance Eq Announcement where
    (==) a b = announcementId a == announcementId b

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

main :: IO ()
main = do
    res <- scrapeAnnouncements
        (baseUrl <> "/tori/?type=sell&province=Keski-Suomi&category=20")
    case res of
        Just anns -> mapM_ print anns
        Nothing   -> print "Error: failed to scrape"
