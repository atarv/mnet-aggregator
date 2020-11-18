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
import           Control.Applicative
import           Text.HTML.Scalpel
import qualified Data.Text                     as T
import qualified Text.Regex.TDFA               as RegexTDFA

-- | Shorthand for constructing regexes
regex :: String -> RegexTDFA.Regex
regex = RegexTDFA.makeRegex

-- | Scrape a single listing for relevant information, including thumbnail
-- images.
listingScraper :: Scraper T.Text Listing
listingScraper = do
    (listingTitle, listingId) <-
        chroot ("td" @: [hasClass "tori_title"] // "a") $ liftA2
            (,)
            (text $ tagSelector "a")
            (attr "href" $ tagSelector "a")
    description        <- html $ "td" @: ["colspan" @= "2", "valign" @= "top"]
    (author, authorId) <- chroot ("a" @: ["href" @=~ regex "/jasenet.*"])
        $ liftA2 (,) (text "a") (attr "href" "a")
    thumbnails <- attrs "src" $ "img" @: [hasClass "border"]
    dates      <- text $ "small" @: [hasClass "light"]
    return $ Listing { .. }

-- | Scrape all listings
listingsScraper :: Scraper T.Text [Listing]
listingsScraper = chroots ("table" @: ["cellpadding" @= "2"]) listingScraper

-- | Scrape all listings from given section URL. May throw errors from
-- http-client!
scrapeListings :: URL -> IO (Maybe [Listing])
scrapeListings url = scrapeURL url listingsScraper
