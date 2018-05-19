module State where
import Data.Map.Strict(Map)
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as Text
import qualified Collection
import qualified Request
import qualified Utility
import Env(Config)
import Control.Monad.Reader
import Network.Wai(Request(..))
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar (tryTakeMVar, tryPutMVar)
import Control.Lens
import Data.Maybe(fromJust)
import Control.Concurrent(forkIO)


-- ============================= AppState ============================
type MapRef = STM.TVar (Map Text.Text Collection.Collection)

data AppState = AppState {
  _mapRef::MapRef,
  _config::Config
}

mapRef :: Lens' AppState (STM.TVar (Map Text.Text Collection.Collection))
mapRef = lens _mapRef (\i x -> i{_mapRef=x})

config :: Lens' AppState Config
config = lens _config (\i x -> i{_config=x})

-- =============================== AppM ==============================
newtype AppM a
    = AppM
    { unAppM :: ReaderT AppState IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppState)

updateCollection :: MapRef -> Text.Text -> Collection.Collection -> STM.STM ()
updateCollection mr name collection= do
  STM.modifyTVar mr update
  where
    update = Map.insert name collection


-- Gets
getCollection :: Request -> AppM (Maybe Collection.Collection)
getCollection request = do
  AppState {_mapRef=mr} <- ask
  m <- liftIO $ STM.readTVarIO mr
  case Map.lookup path m of
    Nothing -> return Nothing
    Just collection -> do
      expired <- liftIO $ Collection.isExpired collection
      if expired then do
        let lock = collection ^. Collection.collectionLock
        took <- liftIO $ tryPutMVar lock ()
        if took then do
          _ <- liftIO $ forkIO $ do
            let ep = fromJust $ collection ^. Collection.collectionEndpoint
                epRequest = Request.PostEndpointRequest{
                  timeout=Just (ep ^. Collection.endpointTimeout),
                  endpoint=ep ^. Collection.endpointUrl}
            maybeCollection <- Collection.bodyToCollection epRequest
            _ <- case maybeCollection of
              Just newCollection -> do
                -- TODO try to lift STM directly to monad IO.
                -- threadDelay (1000 * 1000 * 10)
                STM.atomically (updateCollection mr path newCollection)
                putStrLn "Updated Collection"
                return Nothing
              Nothing -> do
                putStrLn "Could not get collection"
                return Nothing
            maybeLock <- tryTakeMVar lock
            case maybeLock of
              Just () -> return ()
              Nothing -> error "Lock assertion failed!"
          return $ Just collection
        else
          return $ Just collection
      else
        return $ Just collection
  where
    path = Utility.buildPath (pathInfo request)
