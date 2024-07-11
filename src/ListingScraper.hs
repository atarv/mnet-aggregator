{-|
Module         : ListingScraper
Description    : Web scraping listings.
Copyright      : (c) Aleksi Tarvainen, 2020
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module ListingScraper (scrapeListings, ScrapingException) where
import           Control.Applicative
import           Control.Exception
import qualified Data.Text           as T
import           Data.Text.Encoding
import           Listing
import           Network.HTTP.Simple
import           Text.HTML.Scalpel
import qualified Text.Regex.TDFA     as RegexTDFA

-- | `scalpel` library doesn't give much information about why scraping failed
-- so this is pretty basic.
data ScrapingException = ScrapingException deriving (Show)

instance Exception ScrapingException

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
    (author, authorId) <-
        chroot ("a" @: ["href" @=~ regex "/jasenet.*"])
            $ liftA2 (,) (text "a") (attr "href" "a")
    thumbnails <- attrs "src" $ "img" @: [hasClass "border"]
    dates      <- text $ "small" @: [hasClass "light"]
    return $ Listing{..}

-- | Scrape all listings
listingsScraper :: Scraper T.Text [Listing]
listingsScraper = chroots ("table" @: ["cellpadding" @= "2"]) listingScraper

-- | Scrape all listings from given section URL. May throw errors from
-- http-client!
scrapeListings :: URL -> IO (Either SomeException [Listing])
scrapeListings url = try $ do
    request  <- parseRequestThrow $ "GET " <> url
    response <- httpBS request
    -- Mnet uses ISO8859-15, but this might yield close enough results
    let body         = decodeLatin1 $ getResponseBody response
        scrapeResult = scrapeStringLike body listingsScraper
    case scrapeResult of
        Nothing       -> throw ScrapingException
        Just listings -> pure listings
