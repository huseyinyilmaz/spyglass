module Types where

-- import Web.Spock.Config

-- import Data.IORef
import qualified Control.Concurrent.STM as STM
import Data.Trie(Trie)
import Data.Map.Strict(Map)
import qualified Data.ByteString as B
import Data.Text.Encoding(encodeUtf8)
--import qualified Control.Monad.IO.Class.MonadIO as MonadIO
import Web.HttpApiData (FromHttpApiData(..))
import Data.Aeson
import Data.Generics
-- FromHttpApiData instance for bytestring
instance FromHttpApiData B.ByteString where
  parseUrlPiece = Right . encodeUtf8
  parseQueryParam = Right . encodeUtf8

data Item = Item {
  term:: B.ByteString,
  content:: B.ByteString }
  deriving (Generic, Show)

instance ToJSON Item
instance FromJSON Item

data AppSession = EmptySession

data AppState = AppState (STM.TVar (Map B.ByteString (STM.TVar (Trie B.ByteString))))

-- type ActionCtx ctx a = SpockActionCtx ctx () AppSession AppState a
-- type AppActionCtx = MonadIO m => ActionCtxT ctx m a
-- type AppActionCtx = ActionCtxT ctx m a
