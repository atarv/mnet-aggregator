{-# LANGUAGE OverloadedStrings #-}

import qualified Amazonka
import           Configs
import           Control.Exception
import           Control.Monad.IO.Class
import           GHC.IO                      (catchAny)
import           MnetAggregator
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Wai.Middleware.Cors
import           ScrapingOptions
import qualified Web.Scotty                  as S
import qualified Web.Scotty.Trans            as ST

main :: IO ()
main = runServer

-- | Start the server with given configuration
startServer :: MonadIO m => AppConfig -> m ()
startServer conf = do
    awsEnv <- liftIO $ Amazonka.newEnv Amazonka.discover
    liftIO $ S.scotty (fromIntegral $ serverPort conf) $ do
        S.middleware $ cors (const $ Just corsPolicy)
        S.post "/generatereport" $ do
            queryOpts <- S.jsonData
            ST.liftAndCatchIO $ 
                scrapeAndReport 
                    awsEnv 
                    conf 
                    (queryOpts :: ScrapingOptions) 
                `catchAny` (\e -> do 
                    putStrLn $ "Error: " <> show e
                    throwIO e)

-- | Load config and then start server
runServer :: IO ()
runServer = loadConfig Nothing >>= startServer

corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy
    Nothing
    [methodPost, methodOptions]
    [hContentType]
    Nothing
    Nothing
    False
    False
    False
