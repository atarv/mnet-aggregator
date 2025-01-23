{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Amazonka
import           Aws.Lambda
import           Configs
import           Control.Exception
import           Data.IORef
import qualified Data.Text          as T
import           GHC.IO.Encoding
import           MnetAggregator
import           Report
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
        (addAPIGatewayHandler "handler" handler)

handler :: ApiGatewayRequest Report
    -> Context AppConfig
    -> IO (Either (ApiGatewayResponse String) (ApiGatewayResponse ()))
handler request context = do
    appConfig <- readIORef (customContext context)
    awsEnv <- Amazonka.newEnv Amazonka.discover
    case apiGatewayRequestBody request of
        Nothing -> pure $ Left $ mkApiGatewayResponse
                400
                [ ("Content-Type", "text/plain") ]
                "Error: Body was empty"
        Just report -> do
            sendReport awsEnv appConfig report
            pure $ Right $ mkApiGatewayResponse 200 [] ()

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
        , dynamoDBTableName = ddbTableName
        }
  where
    getEnv' varName = T.pack <$> getEnv varName
