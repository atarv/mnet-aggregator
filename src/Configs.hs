{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Configs where
import           Dhall

baseUrl :: String
baseUrl = "https://muusikoiden.net"

data Section = Section { sectionTitle :: !Text
                       , sectionUrl :: !Text
                       }
    deriving (Generic, Show)
instance FromDhall Section

data DatabaseConfiguration =
    DatabaseConfiguration { hostname :: !Text
                          , portNumber :: !Natural
                          , password :: !Text
                          }
    deriving (Generic, Show)
instance FromDhall DatabaseConfiguration

data Config = Config { databaseConfig :: !DatabaseConfiguration
                     , senderEmail :: !Text
                     , senderName :: !Text
                     , recipientEmail :: !Text
                     , recipientName :: !Text
                     , sectionsToScrape :: ![Section]
                     , smtpPassword :: !Text
                     , smtpUsername :: !Text
                     , smtpPort :: !Natural
                     , smtpHostname :: !Text
                     }
    deriving (Generic, Show)
instance FromDhall Config

loadConfig :: Maybe Text -> IO Config
loadConfig Nothing     = input auto "./config.dhall"
loadConfig (Just path) = input auto path
