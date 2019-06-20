module State where
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as Text
import qualified Collection
import qualified Request
import qualified Utility

import Control.Monad.Except(
  MonadError,
  throwError,
  )

import Env(
  HasMapRef(..),
  MapRefVar,
  AsRuntimeError,
  RuntimeError(..),
  _runtimeCollectionNotFoundError,
  )

import Control.Lens

import Control.Monad.Reader
import Network.Wai(Request(..))
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar (tryTakeMVar, tryPutMVar)
import Control.Lens
import Data.Maybe(fromJust)
import Control.Concurrent(forkIO)


-- ============================= AppState ============================
-- type MapRef = STM.TVar (Map Text.Text Collection.Collection)

-- data AppState = AppState {
--   _mapRef::MapRef,
--   _config::Config
-- }

-- mapRef :: Lens' AppState (STM.TVar (Map Text.Text Collection.Collection))
-- mapRef = lens _mapRef (\i x -> i{_mapRef=x})

-- config :: Lens' AppState Config
-- config = lens _config (\i x -> i{_config=x})

-- -- =============================== AppM ==============================
-- newtype AppM a
--     = AppM
--     { unAppM :: ReaderT AppState IO a
--     } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppState)

updateCollection :: MapRefVar -> Text.Text -> Collection.Collection -> STM.STM ()
updateCollection mr name collection= STM.modifyTVar mr update
  where
    update = Map.insert name collection


-- Gets
getCollection :: (
  AsRuntimeError e,
  HasMapRef c,
  MonadReader c m,
  MonadError e m,
  MonadIO m) => Request -> m (Maybe Collection.Collection)
getCollection request = do
  config <- ask
  let mr = view getMapRefVar config
  m <- liftIO $ STM.readTVarIO mr
  case Map.lookup path m of
    Nothing -> throwError $ review (_runtimeCollectionNotFoundError) "Collection does not exist"
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
