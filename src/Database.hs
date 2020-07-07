{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Database where

import           Database.Redis
import           Configs                        ( DatabaseConfiguration(..) )
import           Data.String                    ( IsString )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE

seenKey :: (Semigroup a, IsString a) => a -> a
seenKey = (<>) "seen:"

catchallErrorMsg :: T.Text
catchallErrorMsg = "Database error: this shouldn't happen"

connect :: DatabaseConfiguration -> IO Connection
connect DatabaseConfiguration {..} = checkedConnect $ defaultConnectInfo
    { connectHost = T.unpack hostname
    , connectPort = PortNumber (fromIntegral databasePort)
    , connectAuth = Just $ TE.encodeUtf8 password
    }

disconnect :: Connection -> IO ()
disconnect = Database.Redis.disconnect

getUsersSeenAnnouncements :: Connection -> T.Text -> IO (Either T.Text [T.Text])
getUsersSeenAnnouncements dbConnection userEmail = runRedis dbConnection $ do
    dbResult <- smembers (TE.encodeUtf8 (seenKey userEmail))
    pure $ case dbResult of
        Right seen      -> Right $ fmap TE.decodeUtf8 seen
        Left  (Error e) -> Left $ TE.decodeUtf8 e
        Left  _         -> Left catchallErrorMsg


storeSeenAnnouncements
    :: Connection -> T.Text -> [T.Text] -> IO (Either T.Text Integer)
storeSeenAnnouncements _ _ [] = pure $ Right 0
storeSeenAnnouncements dbConnection userEmail announcementIds =
    runRedis dbConnection $ do
        res <- sadd (TE.encodeUtf8 $ seenKey userEmail)
                    (map TE.encodeUtf8 announcementIds)
        pure $ case res of
            Right numberOfAdded -> Right numberOfAdded
            Left  (Error e)     -> Left $ TE.decodeUtf8 e
            Left  _             -> Left catchallErrorMsg

