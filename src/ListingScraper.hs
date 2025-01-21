{-|
Module         : ListingScraper
Description    : Web scraping listings.
Copyright      : (c) Aleksi Tarvainen, 2025
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications   #-}
module ListingScraper (scrapeListings, scrapeListingsText, ScrapingException, captureLocationAndCounty) where
import           Control.Applicative
import           Control.Exception
import           Data.Maybe
import qualified Data.Text           as T
import           Data.Text.Encoding
import           Debug.Trace
import           Dhall.Core          (Expr (Double))
import           Listing
import           Network.HTTP.Simple
import           Text.HTML.Scalpel
import           Text.Read           (readMaybe)
import qualified Text.Regex.TDFA     as RegexTDFA

-- | `scalpel` library doesn't give much information about why scraping failed
-- so this is pretty basic.
data ScrapingException = ScrapingException deriving (Show)

instance Exception ScrapingException

-- | Shorthand for constructing regexes
regex :: String -> RegexTDFA.Regex
regex = RegexTDFA.makeRegex

captureDates :: T.Text -> (T.Text, T.Text)
captureDates txt = let
    dateFormat =  "([0-9]{2})\\.([0-9]{2})\\.([0-9]{4})" :: String
    matches = RegexTDFA.getAllTextMatches $ txt RegexTDFA.=~ dateFormat
    in case matches of
        start : end : _ -> (start, end)
        _               -> ("", "")

captureLocationAndCounty :: T.Text -> Maybe (T.Text, T.Text)
captureLocationAndCounty input = let
    locationAndCounty = "Paikkakunta: *([åöäÅÖÄa-zA-Z]+) *\\(([ åöäÅÖÄa-zA-Z-]+)\\)" :: String
    matches = RegexTDFA.getAllTextSubmatches $ input RegexTDFA.=~ locationAndCounty :: [T.Text]
    in case -- trace (T.unpack input <> " matches: " <> show matches) 
        matches of
        _fullMatch : location : county : _ -> pure (location, county)
        _fullMatch : location : _          -> pure (location, "")
        _                                  -> Nothing

-- | Scrape a single listing for relevant information, including thumbnail
-- images.
listingScraper :: Scraper T.Text Listing
listingScraper = do
    (listingTitle, listingId) <-
        chroot ("td" // "a") $ liftA2
            (,)
            (text "a")
            (attr "href" "a")
    (description, price)        <- chroot ("td" @: ["colspan" @= "2"]) $ do
            desc <- html "font"
            priceText <- mconcat <$> texts (textSelector `atDepth` 1)
            let commaToDot c = if c == ',' then '.' else c
            let price = readMaybe @Double
                    $ filter (`elem` ("0123456789." :: String))
                    $ map commaToDot (T.unpack priceText) 
            pure (desc <> "<br><br>Hinta: " <> priceText, price)
    (author, authorId) <-
        chroot ("a" @: ["href" @=~ regex "/jasenet.*"])
            $ liftA2 (,) (text "a") (attr "href" "a")
    thumbnails <- attrs "src" $ "img" @: [hasClass "tori-thumb"]
    metaText <- (!! 2) <$> texts "td"
    let (start, end) = captureDates metaText
        dates = "Lisätty: " <> start <> " Voimassa: " <> end
    (location, county) <- fmap (fromMaybe ("", "") . listToMaybe)
        $ chroots "td"
        $ do
            txt <- mconcat <$> texts textSelector
            maybe empty pure (captureLocationAndCounty txt)
    return $ Listing{..}

-- | Scrape all listings
listingsScraper :: Scraper T.Text [Listing]
listingsScraper = chroots ("table" @: [hasClass "tori-advert"]) listingScraper

-- | Scrape all listings from given section URL. May throw errors from
-- http-client!
scrapeListings :: URL -> IO (Either SomeException [Listing])
scrapeListings url = try $ do
    request  <- parseRequestThrow $ "GET " <> url
    response <- httpBS request
    -- Mnet uses ISO8859-15, but this might yield close enough results
    let body         = decodeLatin1 $ getResponseBody response
        scrapeResult = scrapeListingsText body
    case scrapeResult of
        Nothing       -> throw ScrapingException
        Just listings -> pure listings

scrapeListingsText :: T.Text -> Maybe [Listing]
scrapeListingsText body = scrapeStringLike body listingsScraper
