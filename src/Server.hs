{-# LANGUAGE OverloadedStrings #-}
module Server (startServer, runServer) where

import           Configs
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Header
import           Network.Wai.Middleware.Cors
import qualified Web.Scotty                    as S
import qualified Web.Scotty.Trans              as ST
import           MnetWatcher
import           QueryOptions

startServer :: AppConfig -> IO ()
startServer conf = S.scotty (fromIntegral $ serverPort conf) $ do
    S.middleware $ cors (const $ Just corsPolicy)
    S.post "/generatereport" $ do
        queryOpts <- S.jsonData
        ST.liftAndCatchIO $ scrapeAndReport conf (queryOpts :: QueryOptions)

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
