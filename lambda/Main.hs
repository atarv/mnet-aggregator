{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import qualified Amazonka
import           Aws.Lambda
import           Configs
import           Control.Exception
import           Data.IORef
import qualified Data.Text          as T
import           GHC.IO.Encoding
import           MnetAggregator
import           ScrapingOptions
import           System.Environment (getEnv)

main :: IO ()
main = do
    -- Ensure UTF-8 to not get any garbled input/output
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    runLambdaHaskellRuntime
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
    ddbTableName <- getEnv' "DYNAMO_TABLE"
    pure $ AppConfig
        { mailConfig = MailConfig
            { senderEmail = mailSender
            , senderName = mailSenderName
            }
        , serverPort = 80 -- Unused when running on lambda
        , dynamoDBTableName = ddbTableName
        }
  where
    getEnv' varName = T.pack <$> getEnv varName
