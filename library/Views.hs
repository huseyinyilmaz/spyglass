module Views(getCollection,
             postCollection) where
--import qualified Data.Text as Text
--import Data.Text.Encoding (decodeUtf8)
import Web.Spock
import Types
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid
import qualified Data.ByteString as B
-- import Control.Monad.Trans
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class(liftIO)
import qualified Data.Map.Strict as Map
import Utility(noContent)

getCollection :: (SpockState (ActionCtxT ctx m) ~ AppState, Control.Monad.IO.Class.MonadIO m, HasSpock (ActionCtxT ctx m)) => B.ByteString -> ActionCtxT ctx m b
getCollection name = do
  (AppState _ref) <- getState
  bytes ("hello" <> name) --("Hello " <> decodeUtf8 name <> ", you are visitor number " <> Text.pack (show (0::Integer)))


postCollection :: (SpockState (ActionCtxT ctx m) ~ AppState, MonadIO m, HasSpock (ActionCtxT ctx m)) => B.ByteString -> ActionCtxT ctx m ()
postCollection name = do
  (AppState mapRef) <- getState
  -- b <- jsonBody
  m <- liftIO $ STM.readTVarIO mapRef
  case Map.lookup name m of
    Just _trieRef -> bytes "found it"
    Nothing ->
      -- json (b::Maybe [Item])
      noContent -- <<<<<<<<<
