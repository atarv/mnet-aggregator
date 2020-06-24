{-# LANGUAGE OverloadedStrings #-}

import           Text.HTML.Scalpel
import           Control.Applicative

baseUrl = "https://muusikoiden.net"

data Announcement =
    Announcement { title :: String, link :: String }
    deriving (Show, Eq)

main :: IO ()
main = do
    content <- readFile "/home/aleksi/setit-utf.html" :: IO String
    scrapeURL
            (baseUrl <> "/tori/?type=sell&province=Keski-Suomi&category=20")
            comments
        >>= print
  where
    comments :: Scraper String [Announcement]
    comments = chroots ("td" @: [hasClass "tori_title"]) announcement

    announcement :: Scraper String Announcement
    announcement = do
        t <- text $ tagSelector "a"
        l <- attr "href" $ tagSelector "a"
        return $ Announcement { title = t, link = baseUrl <> l }
