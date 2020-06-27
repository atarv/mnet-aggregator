{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import           Text.HTML.Scalpel
import           Control.Applicative
import qualified Text.Regex.TDFA               as RegexTDFA

re :: String -> RegexTDFA.Regex
re = RegexTDFA.makeRegex

baseUrl = "https://muusikoiden.net"

data Announcement =
    Announcement { title :: String, announcementId :: String
                 , description :: String, author :: String, authorId :: String
                 , price :: String, thumbnails :: [String], dates :: String
                 }
    deriving (Show)

instance Eq Announcement where
    (==) a b = announcementId a == announcementId b

announcements :: Scraper String [Announcement]
announcements = chroots ("table" @: ["cellpadding" @= "2"]) announcementScraper

announcementScraper :: Scraper String Announcement
announcementScraper = do
    (title, announcementId) <-
        chroot ("td" @: [hasClass "tori_title"] // "a")
        $   (,)
        <$> (text $ tagSelector "a")
        <*> (attr "href" $ tagSelector "a")
    description        <- text $ "font" @: [hasClass "msg"]
    (author, authorId) <-
        chroot ("a" @: ["href" @=~ (re "/jasenet.*")])
        $   (,)
        <$> (text "a")
        <*> (attr "href" $ "a")
    -- FIXME: Price doesn't get scraped properly
    price      <- (innerHTMLs $ "p") >>= return . concat
    thumbnails <- attrs "src" $ "img" @: [hasClass "border"]
    dates      <- text $ "small" @: [hasClass "light"]
    return $ Announcement { .. }

scrapeAnnouncements :: URL -> IO (Maybe [Announcement])
scrapeAnnouncements url = scrapeURL url announcements

main :: IO ()
main = do
    res <- scrapeAnnouncements
        (baseUrl <> "/tori/?type=sell&province=Keski-Suomi&category=20")
    case res of
        Just anns -> mapM_ print anns
        Nothing   -> print "Error: something went wrong"
