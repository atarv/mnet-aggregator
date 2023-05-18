{-# LANGUAGE OverloadedStrings #-}
import           Configs
import           MnetAggregator
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Wai.Middleware.Cors
import           ScrapingOptions
import qualified Web.Scotty                    as S
import qualified Web.Scotty.Trans              as ST

main :: IO ()
main = runServer 

-- | Start the server with given configuration
startServer :: AppConfig -> IO ()
startServer conf = S.scotty (fromIntegral $ serverPort conf) $ do
    S.middleware $ cors (const $ Just corsPolicy)
    S.post "/generatereport" $ do
        queryOpts <- S.jsonData
        ST.liftAndCatchIO $ scrapeAndReport conf (queryOpts :: ScrapingOptions)

-- | Load config and then start server
runServer :: IO ()
runServer = loadConfig Nothing >>= startServer

corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy Nothing
                                [methodPost, methodOptions]
                                [hContentType]
                                Nothing
                                Nothing
                                False
                                False
                                False
