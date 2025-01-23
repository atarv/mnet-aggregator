{-|
Module         : ScrapingOptions
Description    : Options for scraping sections
Copyright      : (c) Aleksi Tarvainen, 2025
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StrictData     #-}

module ScrapingOptions where
import           Data.Aeson   (FromJSON, ToJSON)
import qualified Data.Text    as T
import           GHC.Generics (Generic)

data ScrapingOptions = ScrapingOptions
    { recipientEmail :: T.Text
    , recipientName  :: T.Text
    , sections       :: [SectionParams]
    , postReportUrl  :: String
    , apiKey         :: T.Text
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data SectionParams = SectionParams
    { sectionTitle :: T.Text
    , sectionUrl   :: T.Text
    }
    deriving (Generic, Show, FromJSON, ToJSON)
