{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import qualified Amazonka
import           Aws.Lambda
import           Configs
import           Control.Exception
import           Data.IORef
import qualified Data.Text          as T
import           MnetAggregator
import           ScrapingOptions
import           System.Environment (getEnv)

main :: IO ()
main = runLambdaHaskellRuntime
    defaultDispatcherOptions
    appConfigEnv
    id
    (addStandaloneLambdaHandler "handler" handler)

handler :: ScrapingOptions -> Context AppConfig -> IO (Either String ())
handler request context = do
    appConfig <- readIORef (customContext context)
    awsEnv <- Amazonka.newEnv Amazonka.discover
    (Right <$> scrapeAndReport awsEnv appConfig request)
        `catch` (pure . Left . show @SomeException)

appConfigEnv :: IO AppConfig
appConfigEnv = do
    mailSender <- getEnv' "MAIL_SENDER_EMAIL"
    mailSenderName <- getEnv' "MAIL_SENDER_NAME"
    mailSmtpHostname <- getEnv' "SMTP_HOSTNAME"
    mailSmtpPassword <- getEnv' "SMTP_PASSWORD"
    mailSmtpUsername <- getEnv' "SMTP_USERNAME"
    mailSmtpPort <- read <$> getEnv "SMTP_PORT"
    ddbTableName <- getEnv' "DYNAMO_TABLE"
    pure $ AppConfig
        { mailConfig = MailConfig
            { senderEmail = mailSender
            , senderName = mailSenderName
            , smtpPassword = mailSmtpPassword
            , smtpHostname = mailSmtpHostname
            , smtpUsername = mailSmtpUsername
            , smtpPort = mailSmtpPort
            }
        , serverPort = 80 -- Unused when running on lambda
        , dynamoDBTableName = ddbTableName
        }
  where
    getEnv' varName = T.pack <$> getEnv varName
