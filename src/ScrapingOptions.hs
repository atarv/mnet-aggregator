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
