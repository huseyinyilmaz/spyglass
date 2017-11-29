{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}

module Types(CollectionInfo(..),
             CollectionItem(..),
             CommitCollectionAPI,
             GetCollectionsAPI,
             CollectionAPI
            ) where

import Servant
import Data.Text
import Data.Aeson.Types
import GHC.Generics


-- type MStack = StateT

data CollectionInfo = CollectionInfo {
  name :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON CollectionInfo

data CollectionItem = CollectionItem {
  id::Text, value::Text
  } deriving (Eq, Show, Generic)

instance ToJSON CollectionItem
instance FromJSON CollectionItem

---------------
-- API TYPES --
---------------
type CommitCollectionAPI = Capture "collection" Text :> ReqBody '[JSON] [CollectionItem] :> Post '[JSON] CollectionInfo

type GetCollectionsAPI = Get '[JSON] [CollectionInfo]

type CollectionAPI = CommitCollectionAPI :<|> GetCollectionsAPI
