{-# LANGUAGE OverloadedStrings #-}

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
    (t, l) <-
        chroot ("td" @: [hasClass "tori_title"] // "a")
        $   (,)
        <$> (text $ tagSelector "a")
        <*> (attr "href" $ tagSelector "a")
    desc           <- text $ "font" @: [hasClass "msg"]
    (auth, authId) <-
        chroot ("a" @: ["href" @=~ (re "/jasenet.*")])
        $   (,)
        <$> (text "a")
        <*> (attr "href" $ "a")
    p    <- innerHTMLs $ "p" -- FIXME: Price doesn't get scraped properly
    pics <- attrs "src" $ "img" @: [hasClass "border"]
    dat  <- text $ "small" @: [hasClass "light"]
    return $ Announcement { title          = t
                          , announcementId = baseUrl <> l
                          , description    = desc
                          , author         = auth
                          , authorId       = authId
                          , price          = concat p
                          , thumbnails     = pics
                          , dates          = dat
                          }

scrapeAnnouncements :: URL -> IO (Maybe [Announcement])
scrapeAnnouncements url = scrapeURL url announcements

main :: IO ()
main = do
    res <- scrapeAnnouncements
        (baseUrl <> "/tori/?type=sell&province=Keski-Suomi&category=20")
    case res of
        Just anns -> mapM_ print anns
        Nothing   -> print "Error: something went wrong"
