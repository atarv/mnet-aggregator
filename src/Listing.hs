{-|
Module         : Listing
Description    : Defining listing's relevant content and transofmations.
Copyright      : (c) Aleksi Tarvainen, 2020
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Listing (Listing(..), toModel, fromModel) where

import           Amazonka.DynamoDB.Types     as DB
import           Amazonka.Prelude            hiding (div)
import           Configs
import           Control.Exception
import           Control.Lens
import           Data.String
import qualified Data.Text                   as T
import           Prelude                     hiding (div, span)
import           Text.Blaze.Html
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A
import           Text.Read

data Listing = Listing
    { listingId    :: !T.Text
    , author       :: !T.Text
    , authorId     :: !T.Text
    , dates        :: !T.Text
    , description  :: !T.Text
    , thumbnails   :: ![T.Text]
    , listingTitle :: !T.Text
    , location     :: !T.Text
    , county       :: !T.Text
    , price        :: !(Maybe Double)
    } deriving (Show)

instance Eq Listing where
    (==) x y = listingId x == listingId y

instance ToMarkup Listing where
    toMarkup Listing {..} = div ! class_ "announcement" $ do
        h3 $ a ! href (toValue $ T.pack baseUrl <> listingId) $ text
            listingTitle
        div $ do
            H.span $ do
                text "Ilmoittaja: "
                a ! href (toValue $ T.pack baseUrl <> authorId) $ text author
            mapM_
                (\thumbnail ->
                    img ! src (toValue $ T.pack baseUrl <> thumbnail)
                        ! alt
                        "announcement image"
                )
                thumbnails
            p $ preEscapedText description
            p $ do
                text dates
                H.br
                text $ "Paikkakunta: " <> location <> " (" <> county <> ")"
        hr

toModel :: T.Text -> T.Text -> Listing -> HashMap Text DB.AttributeValue
toModel userId sectionTitle Listing{..} = fromList
    [ ("PK", S userId)
    , ("SK", S $ "Listing#" <> listingId)
    , ("listingId", S listingId)
    , ("author", S author)
    , ("authorId", S authorId)
    , ("dates", S dates)
    , ("description", S description)
    , ("thumbnails", if null thumbnails then NULL else SS $ fromList thumbnails)
    , ("title", S listingTitle)
    , ("sectionTitle", S sectionTitle)
    , ("location", S location)
    , ("county", S county)
    , ("price", case price of
            Nothing  -> NULL
            Just num -> N $ T.pack $ show num)
    ]

fromModel :: HashMap Text DB.AttributeValue -> Listing
fromModel model = Listing
    { listingId = fromS model "listingId"
    , author = fromS model "author"
    , authorId = fromS model "authorId"
    , dates = fromS model "dates"
    , thumbnails = fromSS model "thumbnails"
    , listingTitle = fromS model "title"
    , description = fromS model "description"
    , location = fromSDefault model "location"
    , county = fromSDefault model "county"
    , price = fromNum model "price"
    }
  where
    fromS model fieldName =
        case model ^. at (T.pack fieldName) of
            Just (S str) -> str
            val -> Prelude.error
                $ "Could not parse record from model. Invalid value in field '"
                <> fieldName
                <> "': "
                <> show val
    fromSDefault model fieldName =
        case model ^. at (T.pack fieldName) of
            Just (S str) -> str
            _            -> ""
    fromNum model fieldName =
        case model ^. at (T.pack fieldName) of
            Just (N str) -> readMaybe (T.unpack str)
            _            -> Nothing
    fromSS model fieldName =
        case model ^. at (T.pack fieldName) of
            Just (SS strings) -> Amazonka.Prelude.toList strings
            Just NULL -> []
            val -> Prelude.error
                $ "Could not convert to record from model. Invalid value in field '"
                <> fieldName
                <> "': "
                <> show val
