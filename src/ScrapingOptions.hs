{-|
Module         : ScrapingOptions
Description    : Options for scraping sections
Copyright      : (c) Aleksi Tarvainen, 2020
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric     #-}

module ScrapingOptions where
import           Data.Aeson
import           GHC.Generics
import qualified Data.Text                     as T

data ScrapingOptions = ScrapingOptions { recipientEmail :: !T.Text
                                       , recipientName :: !T.Text
                                       , sections :: ![Section]
                                       }
    deriving (Show, Generic, FromJSON, ToJSON)


data Section = Section { sectionTitle :: !T.Text
                       , sectionUrl :: !T.Text
                       }
    deriving (Generic, Show, FromJSON, ToJSON)
