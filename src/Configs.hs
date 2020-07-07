{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Configs where
import           Dhall

baseUrl :: String
baseUrl = "https://muusikoiden.net"

data DatabaseConfiguration =
    DatabaseConfiguration { hostname :: !Text
                          , databasePort :: !Natural
                          , password :: !Text
                          }
    deriving (Generic, Show)
instance FromDhall DatabaseConfiguration

data MailConfig =
    MailConfig { senderEmail :: !Text
               , senderName :: !Text
               , smtpPassword :: !Text
               , smtpUsername :: !Text
               , smtpPort :: !Natural
               , smtpHostname :: !Text
               }
    deriving (Show, Generic)
instance FromDhall MailConfig

data AppConfig = AppConfig { databaseConfig :: !DatabaseConfiguration
                     , mailConfig :: !MailConfig
                     , serverPort :: !Natural
                     }
    deriving (Generic, Show)
instance FromDhall AppConfig

loadConfig :: Maybe Text -> IO AppConfig
loadConfig Nothing     = input auto "./config.dhall"
loadConfig (Just path) = input auto path
