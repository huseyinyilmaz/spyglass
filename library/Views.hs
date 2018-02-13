module Views(getCollection,
             postCollection) where
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Web.Spock
import Types
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid
import qualified Data.ByteString as B
-- import Control.Monad.Trans
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class(liftIO)
import qualified Data.Map.Strict as Map


getCollection :: (SpockState (ActionCtxT ctx m) ~ AppState, MonadIO m, HasSpock (ActionCtxT ctx m)) => B.ByteString -> ActionCtxT ctx m b
getCollection name = do
  (AppState ref) <- getState
  text ("Hello " <> decodeUtf8 name <> ", you are visitor number " <> Text.pack (show (0::Integer)))


postCollection :: (SpockState (ActionCtxT ctx m) ~ AppState, MonadIO m, HasSpock (ActionCtxT ctx m)) => B.ByteString -> ActionCtxT ctx m b
postCollection name = do
  (AppState mapRef) <- getState
  m <- liftIO $ STM.readTVarIO mapRef
  case Map.lookup name m of
    Just trieRef -> text "found it"
    Nothing ->
      text ("Hello " <> decodeUtf8 name <> ", you are visitor number " <> Text.pack (show (0::Integer)))
