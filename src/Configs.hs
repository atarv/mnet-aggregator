{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Configs (Config(..), loadConfig, baseUrl) where
import           Dhall

baseUrl :: String
baseUrl = "https://muusikoiden.net"

data Config = Config { senderEmail :: Text
                     , senderName :: Text
                     , recipientEmail :: Text
                     , recipientName :: Text
                     , scrapeUrls :: [Text]
                     , smtpPassword :: Text
                     , smtpUsername :: Text
                     , smtpPort :: Natural
                     , hostname :: Text
                     }
    deriving (Generic, Show)

instance FromDhall Config

loadConfig :: Maybe Text -> IO Config
loadConfig Nothing     = input auto "./config.dhall"
loadConfig (Just path) = input auto path
