{-|
Module         : Report
Description    : Representation for scraped report content
Copyright      : (c) Aleksi Tarvainen, 2025
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StrictData     #-}
module Report where

import           Data.Aeson
import qualified Data.Text    as T
import           GHC.Generics (Generic)
import           Listing

data Report = Report
    { reportSections       :: [ReportSection]
    , reportRecipientName  :: T.Text
    , reportRecipientEmail :: T.Text
    }
    deriving (Generic, FromJSON, ToJSON, Show)

data ReportSection = ReportSection
    { title    :: T.Text
    , listings :: Either T.Text [Listing]
    } deriving (Generic, FromJSON, ToJSON, Show)
