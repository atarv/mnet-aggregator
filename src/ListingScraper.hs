{-|
Module         : ListingScraper
Description    : Web scraping listings.
Copyright      : (c) Aleksi Tarvainen, 2020
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module ListingScraper (scrapeListings) where
import           Listing
import           Text.HTML.Scalpel
import qualified Data.Text                     as T
import qualified Text.Regex.TDFA               as RegexTDFA

re :: String -> RegexTDFA.Regex
re = RegexTDFA.makeRegex

-- | Scrape a single listing
listingScraper :: Scraper T.Text Listing
listingScraper = do
    (listingTitle, listingId) <-
        chroot ("td" @: [hasClass "tori_title"] // "a")
        $   (,)
        <$> text (tagSelector "a")
        <*> attr "href" (tagSelector "a")
    description        <- html $ "td" @: ["colspan" @= "2", "valign" @= "top"]
    (author, authorId) <-
        chroot ("a" @: ["href" @=~ re "/jasenet.*"]) $ (,) <$> text "a" <*> attr
            "href"
            "a"
    thumbnails <- attrs "src" $ "img" @: [hasClass "border"]
    dates      <- text $ "small" @: [hasClass "light"]
    return $ Listing { .. }

-- | Scrape all listings
listingsScraper :: Scraper T.Text [Listing]
listingsScraper =
    chroots ("table" @: ["cellpadding" @= "2"]) listingScraper

-- | Scrape all listings from given section URL
scrapeListings :: URL -> IO (Maybe [Listing])
scrapeListings url = scrapeURL url listingsScraper
