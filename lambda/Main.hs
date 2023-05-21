{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aws.Lambda
import Configs
import Control.Exception
import Data.IORef
import MnetAggregator
import ScrapingOptions
import System.Environment (getEnv)

import qualified Data.Text as T

main :: IO ()
main =
    runLambdaHaskellRuntime
        defaultDispatcherOptions
        appConfigEnv
        id
        (addStandaloneLambdaHandler "handler" handler)

handler :: ScrapingOptions -> Context AppConfig -> IO (Either String ())
handler request context = do
    appConfig <- readIORef (customContext context)
    (Right <$> scrapeAndReport appConfig request)
        `catch` (pure . Left . show @SomeException)

appConfigEnv :: IO AppConfig
appConfigEnv = do
    dbHostname <- getEnv' "DB_HOSTNAME"
    dbPort <- read <$> getEnv "DB_PORT"
    dbPassword <- getEnv' "DB_PASSWORD"
    mailSender <- getEnv' "MAIL_SENDER_EMAIL"
    mailSenderName <- getEnv' "MAIL_SENDER_NAME"
    mailSmtpHostname <- getEnv' "SMTP_HOSTNAME"
    mailSmtpPassword <- getEnv' "SMTP_PASSWORD"
    mailSmtpUsername <- getEnv' "SMTP_USERNAME"
    mailSmtpPort <- read <$> getEnv "SMTP_PORT"
    pure $
        AppConfig
            { databaseConfig =
                DatabaseConfiguration
                    { hostname = dbHostname
                    , databasePort = dbPort
                    , password = dbPassword
                    }
            , mailConfig =
                MailConfig
                    { senderEmail = mailSender
                    , senderName = mailSenderName
                    , smtpPassword = mailSmtpPassword
                    , smtpHostname = mailSmtpHostname
                    , smtpUsername = mailSmtpUsername
                    , smtpPort = mailSmtpPort
                    }
            , serverPort = 80 -- Unused when running on lambda
            }
  where
    getEnv' varName = T.pack <$> getEnv varName