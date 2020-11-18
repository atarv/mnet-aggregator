{-|
Module         : Database
Description    : Applications interface to Redis database.
Copyright      : (c) Aleksi Tarvainen, 2020
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}
module Database
    ( ListingId
    , Database.connect
    , disconnect
    , getUsersSeenListings
    , storeSeenListings
    )
where

import           Configs                        ( DatabaseConfiguration(..) )
import           Data.String                    ( IsString )
import           Database.Redis
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE

type ListingId = T.Text

-- | Prefix given string with the key for seen listings
seenKey :: (Semigroup a, IsString a) => a -> a
seenKey = (<>) "seen:"

-- | Redis should fail with 'Left (Error e)', but just in case...
catchallErrorMsg :: T.Text
catchallErrorMsg = "Database error: this shouldn't happen"

-- | Connect to database with user configured modifications to default
-- connection info
connect :: DatabaseConfiguration -> IO Connection
connect DatabaseConfiguration {..} = checkedConnect $ defaultConnectInfo
    { connectHost = T.unpack hostname
    , connectPort = PortNumber (fromIntegral databasePort)
    , connectAuth = Just $ TE.encodeUtf8 password
    }

-- | Query the set of listings that are seen by user with given email address
getUsersSeenListings :: Connection -> T.Text -> IO (Either T.Text [ListingId])
getUsersSeenListings dbConnection userEmail = runRedis dbConnection $ do
    dbResult <- smembers (TE.encodeUtf8 (seenKey userEmail))
    pure $ case dbResult of
        Right seen      -> Right $ fmap TE.decodeUtf8 seen
        Left  (Error e) -> Left $ TE.decodeUtf8 e
        Left  _         -> Left catchallErrorMsg


-- | Store the set of listings user with given email address has seen.
-- Returns number of newly stored listings.
storeSeenListings
    :: Connection -> T.Text -> [ListingId] -> IO (Either T.Text Integer)
storeSeenListings _ _ [] = pure $ Right 0
storeSeenListings dbConnection userEmail listingIds =
    runRedis dbConnection
        $   \case
                Right numberOfAdded -> Right numberOfAdded
                Left  (Error e)     -> Left $ TE.decodeUtf8 e
                Left  _             -> Left catchallErrorMsg
        <$> sadd (TE.encodeUtf8 $ seenKey userEmail)
                 (map TE.encodeUtf8 listingIds)

