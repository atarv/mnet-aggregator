{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric     #-}

module QueryOptions where
import           Data.Aeson
import           GHC.Generics
import qualified Data.Text                     as T

data QueryOptions = QueryOptions { recipientEmail :: !T.Text
                                 , recipientName :: !T.Text
                                 , sections :: ![Section]
                                 }
    deriving (Show, Generic, FromJSON, ToJSON)


data Section = Section { sectionTitle :: !T.Text
                       , sectionUrl :: !T.Text
                       }
    deriving (Generic, Show, FromJSON, ToJSON)
