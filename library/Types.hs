{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import qualified Control.Concurrent.STM as STM
import Data.Trie(Trie)
import Data.Map.Strict(Map)
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import Web.HttpApiData (FromHttpApiData(..))
import Data.Aeson
import Control.Monad(mzero)
import GHC.Generics
-- import Control.Monad.IO.Class (MonadIO)
import Data.String(IsString)
-- import Network.Wai
-- import Control.Monad.Reader (ReaderT)

-- Make bytestrig type usable by api.
instance FromHttpApiData B.ByteString where
  parseUrlPiece = Right . encodeUtf8
  parseQueryParam = Right . encodeUtf8


instance FromJSON B.ByteString where
  parseJSON (String str) = return $ encodeUtf8 str
  parseJSON _            = mzero

instance ToJSON B.ByteString where
  toJSON = (toJSON . decodeUtf8)

newtype ItemContent = ItemContent {getItemContent:: B.ByteString}
  deriving (Show, Generic, ToJSON, FromJSON, Eq, Ord, Monoid, IsString)

data Item = Item {
  term:: !B.ByteString,
  content:: !ItemContent } deriving (Show, Generic)

instance ToJSON Item
instance FromJSON Item

data AuthUser = AuthUser {
  username:: B.ByteString,
  password:: B.ByteString
  } deriving (Generic, Show, Eq)

instance ToJSON AuthUser
instance FromJSON AuthUser

data Config = Config {
  port:: Int,
  callbacks:: [B.ByteString],
  gzipEnabled:: Bool,
  monitoringEnabled:: Bool,
  monitoringIP:: B.ByteString,
  monitoringPort:: Int,
  loggingEnabled:: Bool,
  loggingForDevelopment:: Bool,
  defaultResultLimit:: Int,
  users :: [AuthUser]
  } deriving (Generic, Show, Eq)

instance ToJSON Config
instance FromJSON Config

data AppState = AppState {
  getMapRef::STM.TVar (Map Text (Trie [ItemContent])),
  getConfig::Config
}


-- data AppStack = ReaderT AppState IO Response
