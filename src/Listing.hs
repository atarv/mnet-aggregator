{-|
Module         : Listing
Description    : Defining listing's relevant content and transofmations.
Copyright      : (c) Aleksi Tarvainen, 2020
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, StrictData #-}
module Listing (Listing(..)) where
import           Prelude                 hiding ( div
                                                , span
                                                )
import           Configs
import           Text.Blaze.Html
import           Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes   as A
import qualified Data.Text                     as T


data Listing = Listing
    { listingId :: !T.Text
    , author :: !T.Text
    , authorId :: !T.Text
    , dates :: !T.Text
    , description :: !T.Text
    , thumbnails :: ![T.Text]
    , listingTitle :: !T.Text
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
                    img ! src (toValue $ T.pack baseUrl <> thumbnail) ! alt
                        "announcement image"
                )
                thumbnails
            p $ preEscapedText description
            H.span $ text dates
        hr
