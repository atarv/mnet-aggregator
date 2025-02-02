{-|
Module         : Configs
Description    : Configuration definitions for the app.
Copyright      : (c) Aleksi Tarvainen, 2025
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Configs where
import           Dhall

-- | Base URL of Muusikoiden.net's services
baseUrl :: String
baseUrl = "https://muusikoiden.net"

data MailConfig = MailConfig
    { senderEmail  :: !Text
    , senderName   :: !Text
    } deriving (Show, Generic)

instance FromDhall MailConfig

-- | This includes all the necessary configurations for app
data AppConfig = AppConfig
    { mailConfig        :: !MailConfig
    , dynamoDBTableName :: !Text
    } deriving (Generic, Show)

instance FromDhall AppConfig

-- | Loads configuration from file. If no path is given, configuration is loaded
-- from "./config.dhall". Dhall checks that the given configuration has the
-- types specified in this module.
loadConfig :: Maybe Text -> IO AppConfig
loadConfig Nothing     = input auto "./config.dhall"
loadConfig (Just path) = input auto path
