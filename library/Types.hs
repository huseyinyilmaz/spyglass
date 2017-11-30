{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}

module Types(CollectionInfo(..),
             CollectionItem(..),
             CommitCollectionAPI,
             GetCollectionsAPI,
             CollectionAPI,
             State(..),
             AppM,
            ) where

import Servant
import Data.Text
import Data.Aeson.Types
import GHC.Generics
import qualified Data.Map.Strict as Map
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans.Reader(ReaderT)
import Control.Monad.Except(ExceptT)
--import Control.Monad.Reader.ReaderT -- , ask, runReaderT)

-- type MStack = StateT

type Key = String

data Item = Item {
  getKey :: Key,
  getSearchTerms :: [String],
  getValue :: String
  }

type Collection = Map.Map Key Item

data State = State { getCollection :: STM.TVar Collection }


type AppM = ReaderT State (ExceptT ServantErr IO)


---------------
-- API TYPES --
---------------
data CollectionInfo = CollectionInfo {
  name :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON CollectionInfo

data CollectionItem = CollectionItem {
  id::Text, value::Text
  } deriving (Eq, Show, Generic)

instance ToJSON CollectionItem
instance FromJSON CollectionItem


type CommitCollectionAPI = Capture "collection" Text :> ReqBody '[JSON] [CollectionItem] :> Post '[JSON] CollectionInfo

type GetCollectionsAPI = Get '[JSON] [CollectionInfo]

type CollectionAPI = CommitCollectionAPI :<|> GetCollectionsAPI
