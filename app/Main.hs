{-# LANGUAGE OverloadedStrings #-}
import qualified Amazonka
import           Configs
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.Text.Encoding      as T
import           GHC.Stack               (HasCallStack)
import           MnetAggregator
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import           Options.Applicative
import           ScrapingOptions         (ScrapingOptions (apiKey, postReportUrl))
import           System.Exit             (exitFailure)

data CliOptions = CliOptions { scrapingOptionsPath :: FilePath }

options :: Parser CliOptions
options = CliOptions
    <$> strArgument (metavar "SCRAPING_OPTIONS_PATH"
        <> help "Path to scraping options (.json)")

main :: HasCallStack => IO ()
main = do
    opts <- customExecParser
        (prefs $ showHelpOnEmpty <> showHelpOnError)
        (info (options <**> helper)
        (header "mnet-aggregator-exe"
            <> progDesc "Scrape Muusikoiden.net listings and send a report via email"
            <> fullDesc))
    scrapingOpts <- throwDecode =<< BSL.readFile (scrapingOptionsPath opts)
    reportJson <- encode <$> scrapeReport scrapingOpts

    -- POST report to Lambda for further processing
    initReq <- parseRequest (postReportUrl scrapingOpts)
    let request = initReq
            { method = "POST"
            , secure = True
            , requestHeaders =
                [ ("Content-Type", "application/json")
                , ("x-api-key", T.encodeUtf8 $ apiKey scrapingOpts)
                ]
            , requestBody = RequestBodyLBS reportJson
            }
    response <- httpLbs request =<< newTlsManager
    let status =  responseStatus response
    BSL.putStr $ responseBody response
    unless (statusIsSuccessful status) exitFailure
