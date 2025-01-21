{-# LANGUAGE OverloadedStrings #-}
import qualified Amazonka
import           Configs
import           Data.Aeson
import qualified Data.ByteString     as BS
import           Data.Text           as T
import           GHC.Stack           (HasCallStack)
import           MnetAggregator
import           Options.Applicative

data CliOptions = CliOptions
    { configPath          :: Maybe String
    , scrapingOptionsPath :: String
    }

options :: Parser CliOptions
options = CliOptions
    <$> optional (
        strOption (long "config"
            <> short 'c'
            <> metavar "PATH"
            <> help "Path to config (.dhall)")
        )
    <*> strArgument (metavar "SCRAPING_OPTIONS_PATH"
        <> help "Path to scraping options (.json)")

main :: HasCallStack => IO ()
main = do
    opts <- customExecParser
        (prefs $ showHelpOnEmpty <> showHelpOnError)
        (info (options <**> helper)
        (header "mnet-aggregator-exe"
            <> progDesc "Scrape Muusikoiden.net listings and send a report via email"
            <> fullDesc))
    aws <- Amazonka.newEnv Amazonka.discover
    config <- loadConfig (T.pack <$> configPath opts)
    scrapingOpts <- throwDecodeStrict =<< BS.readFile (scrapingOptionsPath opts)
    scrapeAndReport aws config scrapingOpts
