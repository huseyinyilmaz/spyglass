module Env where
-- import Data.Text(Text)
-- import qualified Turtle as Turtle
import GHC.Generics
import Data.Aeson
import Common()
-- import qualified Data.List as L
import qualified Data.Yaml as Y
import qualified Data.ByteString as B
import qualified Data.Text as Text
-- import qualified Data.Text as T
-- import System.Directory (getHomeDirectory)
-- import Data.Monoid((<>))
-- import Types
import qualified Collection

import Control.Lens

import Control.Monad.Except(
  ExceptT,
  MonadError
  )
import Control.Monad.Reader(
  MonadReader,
  ReaderT(..)
  )
import Control.Monad.IO.Class(MonadIO)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map


data Endpoint = Endpoint {
  _path :: Text.Text,
  _url :: Text.Text,
  _timeout :: Maybe Integer
  } deriving(Generic, Show, Eq)

data AuthUser = AuthUser {
  _username:: Text.Text,
  _password:: Text.Text
  } deriving (Generic, Show, Eq)

data Config = Config {
  _port:: Int,
  _gzipEnabled:: Bool,
  _monitoringEnabled:: Bool,
  _monitoringIP:: Text.Text,
  _monitoringPort:: Int,
  _loggingEnabled:: Bool,
  _loggingForDevelopment:: Bool,
  _defaultResultLimit:: Int,
  _users :: [AuthUser],
  _endpoints :: [Endpoint]
  } deriving (Generic, Show, Eq)

class HasConfig a where
  getConfig :: Lens' a Config
  getPort :: Lens' a Int
  getGzipEnabled :: Lens' a Bool
  getMonitoringEnabled :: Lens' a Bool
  getMonitoringIP :: Lens' a Text.Text
  getMonitoringPort :: Lens' a Int
  getLoggingEnabled :: Lens' a Bool
  getLoggingForDevelopment :: Lens' a Bool
  getDefaultResultLimit :: Lens' a Int
  getUsers :: Lens' a [AuthUser]
  getEndpoints :: Lens' a [Endpoint]

  getPort = getConfig . getPort
  getGzipEnabled = getConfig . getGzipEnabled
  getMonitoringEnabled = getConfig . getMonitoringEnabled
  getMonitoringIP = getConfig . getMonitoringIP
  getMonitoringPort = getConfig . getMonitoringPort
  getLoggingEnabled = getConfig . getLoggingEnabled
  getLoggingForDevelopment = getConfig . getLoggingForDevelopment
  getDefaultResultLimit = getConfig . getDefaultResultLimit
  getUsers = getConfig . getUsers
  getEndpoints = getConfig . getEndpoints

instance HasConfig Config where
  getConfig = id
  getPort = lens _getPort (\c p -> c{ _port = p })
  getGzipEnabled = lens _gzipEnabled (\c p -> c{ _gzipEnabled = p })
  getMonitoringEnabled = lens _monitoringEnabled (\ c p -> c{_monitoringEnabled = p})
  getMonitoringIP = lens _monitoringIP (\ c i -> c{_monitoringIP = i})
  getMonitoringPort = lens _monitoringPort (\ c p -> c{_monitoringPort = p})
  getLoggingEnabled = lens _loggingEnabled (\ c p -> c{_loggingEnabled = p})
  getLoggingForDevelopment = lens _loggingForDevelopment (\ c p -> c{_loggingForDevelopment = p})
  getDefaultResultLimit = lens _defaultResultLimit (\ c l -> c{_defaultResultLimit = l})
  getUsers = lens _getUsers (\c us -> c{ _users = us })
  getEndpoints = lens _getEndpoints (\c es -> c{ _endpoints = es })

-- ============================= AppState ============================
-- XXX you were here
newtype MapRef = STM.TVar (Map.Map Text.Text Collection.Collection)

class HasMapRef a where
  getMapRef :: Lens' a MapRef

instance HasMapRef MapRef where
  getMapRef = id

data AppState = AppState {
  _mapRef::MapRef,
  _config::Config
}

instance HasConfig appState where
  getConfig = lens _config (\ a c -> a{_config = c})

instance HasMapRef appState where
  getMapRef = lens _mapRef (\ a m -> a{_mapRef = m})


-- Make everything json serializable
instance ToJSON Endpoint
instance FromJSON Endpoint
instance ToJSON AuthUser
instance FromJSON AuthUser
instance ToJSON Config
instance FromJSON Config

instance Collection.ToCollection Endpoint where
  toCollection Endpoint {_url=u, _timeout=t} = do
    maybeCollection <- Collection.requestCollectionEndpoint u t 5
    case maybeCollection of
      Just collection -> return collection
      Nothing -> error "Problem Reading endpoint quiting...."

readConfig :: IO Config
readConfig = do
  -- home <- getHomeDirectory
  config <- B.readFile "/etc/spyglass/config.yaml"
  case (Y.decode config):: Maybe Config of
    Just c -> return c
    Nothing -> error "Could parse /etc/spyglass/config.yaml!"

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
