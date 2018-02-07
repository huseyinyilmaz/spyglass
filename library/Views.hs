module Views(getCollection,
             postCollection) where
import qualified Data.Text as Text
import Web.Spock
import Types
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid
-- import Control.Monad.Trans

getCollection :: (SpockState (ActionCtxT ctx m) ~ AppState, MonadIO m, HasSpock (ActionCtxT ctx m)) => Text.Text -> ActionCtxT ctx m b
getCollection name = do
  (AppState ref) <- getState
  text ("Hello " <> name <> ", you are visitor number " <> Text.pack (show (0::Integer)))


postCollection :: (SpockState (ActionCtxT ctx m) ~ AppState, MonadIO m, HasSpock (ActionCtxT ctx m)) => Text.Text -> ActionCtxT ctx m b
postCollection name = do
  (AppState ref) <- getState
  text ("Hello " <> name <> ", you are visitor number " <> Text.pack (show (0::Integer)))
