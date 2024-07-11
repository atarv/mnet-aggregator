{-|
Module         : Database
Description    : Application's interface to DynamoDB
Copyright      : (c) Aleksi Tarvainen, 2024
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
module Database where

import           Amazonka                 (runResourceT)
import qualified Amazonka
import           Amazonka.DynamoDB
import           Amazonka.DynamoDB.Lens
import           Amazonka.DynamoDB.Types
import           Amazonka.Prelude
import           Control.Lens
import           Control.Lens.Combinators
import           Control.Monad
import           Data.List.Lens
import           Data.Maybe
import qualified Data.Text                as T
import qualified Data.Text.Internal.Read  as T
import           Listing

type ListingId = T.Text

getUsersSeenListings :: Amazonka.Env -> T.Text -> IO (Either T.Text [ListingId])
getUsersSeenListings env userEmail = undefined

-- | Store the set of listings user with given email address has seen.
-- Returns identifiers of listings that already existed.
storeListings :: Amazonka.Env
    -> T.Text -- ^ DynamoDB table name
    -> T.Text -- ^ User identifier (email)
    -> T.Text -- ^ Section title
    -> [Listing] -- ^ Listings to store
    -> IO [ListingId]
storeListings awsEnv tableName userId sectionTitle listings = do
        results <- runResourceT $ forM listings $ \listing -> do
            let model = toModel userId sectionTitle listing
                putRequest = newPutItem tableName
                    & putItem_returnValues ?~ ReturnValue_ALL_OLD
                    & putItem_item .~ model
            Amazonka.send awsEnv putRequest
        let returnedItems = mapMaybe (^. putItemResponse_attributes) results
        pure $ listingId . fromModel <$> returnedItems
