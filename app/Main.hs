{-# LANGUAGE OverloadedStrings #-}

import           Text.HTML.Scalpel
import           Control.Applicative
import qualified Text.Regex.TDFA               as RegexTDFA

re :: String -> RegexTDFA.Regex
re = RegexTDFA.makeRegex

baseUrl = "https://muusikoiden.net"

data Announcement =
    Announcement { title :: String, link :: String, description :: String
                 , author :: String, authorId :: String, price :: String
                 , thumbnails :: [String], dates :: String }
    deriving (Show, Eq)

announcements :: Scraper String [Announcement]
announcements = chroots "table" announcement

announcement :: Scraper String Announcement
announcement = do
    (t, l) <- chroot ("td" @: [hasClass "tori_title"] // "a") $ do
        t <- text $ tagSelector "a"
        l <- attr "href" $ tagSelector "a"
        return (t, l)
    desc           <- text $ "font" @: [hasClass "msg"]
    (auth, authId) <- chroot ("a" @: ["href" @=~ (re "/jasenet.*")]) $ do
        auth   <- text "a"
        authId <- attr "href" $ "a"
        return (auth, authId)
    p    <- texts "p" -- FIXME: Price doesn't get scraped properly
    pics <- attrs "src" $ "img" @: [hasClass "border"]
    dat  <- text $ "small" @: [hasClass "light"]
    return $ Announcement { title       = t
                          , link        = baseUrl <> l
                          , description = desc
                          , author      = auth
                          , authorId    = authId
                          , price       = concat p
                          , thumbnails  = pics
                          , dates       = dat
                          }

main :: IO ()
main = do
    scrapeURL
            (baseUrl <> "/tori/?type=sell&province=Keski-Suomi&category=20")
            announcements
        >>= print
