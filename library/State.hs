module State where
import Data.Map.Strict(Map)
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as B
import qualified Collection
import Env(Config)
import Control.Monad.Reader
import Network.Wai(Request(..), Response, responseLBS)
import Network.HTTP.Types.Status(status200)
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar (newMVar, tryTakeMVar, tryPutMVar, MVar)
import Control.Lens

data AppState = AppState {
  _mapRef::STM.TVar (Map B.ByteString Collection.Collection),
  _config::Config
}

mapRef :: Lens' AppState (STM.TVar (Map B.ByteString Collection.Collection))
mapRef = lens _mapRef (\i x -> i{_mapRef=x})

config :: Lens' AppState Config
config = lens _config (\i x -> i{_config=x})

newtype AppM a
    = AppM
    { unAppM :: ReaderT AppState IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppState)

-- Gets
getCollection :: Request -> AppM (Maybe Collection.Collection)
getCollection request = do
  AppState {_mapRef=mr} <- ask
  m <- liftIO $ STM.readTVarIO mr
  case Map.lookup path m of
    Just collection -> do
      took <- liftIO $ tryPutMVar (Collection._lock collection) ()
      if took
        then do
        --PostEndpointRequest{timeout=t, endpoint=u}
        return $ Just collection
        else return $ Just collection


    Nothing -> return Nothing
  where
    path = rawPathInfo request
