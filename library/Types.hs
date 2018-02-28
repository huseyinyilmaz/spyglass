{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

-- import Web.Spock.Config

-- import Data.IORef
import qualified Control.Concurrent.STM as STM
import Data.Trie(Trie)
import Data.Map.Strict(Map)
import qualified Data.ByteString as B
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
--import qualified Control.Monad.IO.Class.MonadIO as MonadIO
import Web.HttpApiData (FromHttpApiData(..))
import Data.Aeson
import Control.Monad(mzero)
--import Data.Generics
import GHC.Generics
-- FromHttpApiData instance for bytestring
import Control.Monad.IO.Class (MonadIO)
import Web.Spock
import Data.String(IsString)

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
  term:: B.ByteString,
  content:: ItemContent } deriving (Show, Generic)

instance ToJSON Item
instance FromJSON Item

data Config = Config {
  port::Int,
  callbacks::[B.ByteString],
  monitoringEnabled::Bool,
  monitoringIP::B.ByteString,
  monitoringPort::Int,
  loggingEnabled::Bool,
  loggingForDevelopment::Bool
  } deriving (Generic, Show, Eq)

instance ToJSON Config
instance FromJSON Config

data AppSession = EmptySession

data AppState = AppState {
  getMapRef::(STM.TVar (Map B.ByteString (STM.TVar (Trie [ItemContent])))),
  getConfig::Config
}

type View ctx m =(SpockState (ActionCtxT ctx m) ~ AppState,
                  MonadIO m,
                  HasSpock (ActionCtxT ctx m)) => ActionCtxT ctx m ()
