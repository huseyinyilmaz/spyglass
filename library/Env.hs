module Env where
-- import Data.Text(Text)
-- import qualified Turtle as Turtle
import GHC.Generics
import Data.Aeson
import Common()
-- import qualified Data.List as L
import qualified Data.Text as Text
-- import qualified Data.Text as T
-- import System.Directory (getHomeDirectory)
-- import Data.Monoid((<>))
-- import Types
import qualified Collection

import Control.Lens

import Control.Monad.Except(
  ExceptT,
  MonadError,
  -- throwError,
  runExceptT,
  )
import Control.Monad.Reader(
  MonadReader,
  ReaderT(..),
  runReaderT,
  )

-- import Control.Monad.IO.Class(liftIO)

import Control.Monad.IO.Class(MonadIO)
import Control.Concurrent.STM (
  STM,
  TVar,
  modifyTVar
  )


import qualified Data.Map.Strict as Map


data Endpoint = Endpoint {
  _path :: Text.Text,
  _url :: Text.Text,
  _timeout :: Maybe Integer
  } deriving(Generic, Show, Eq)

getPath :: Lens' Endpoint Text.Text
getPath = lens _path (\e p -> e{ _path = p })

getUrl :: Lens' Endpoint Text.Text
getUrl = lens _url (\e u -> e{ _url = u })

getTimeout :: Lens' Endpoint (Maybe Integer)
getTimeout = lens _timeout (\e t -> e{ _timeout = t })

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
  getPort = lens _port (\c p -> c{ _port = p })
  getGzipEnabled = lens _gzipEnabled (\c p -> c{ _gzipEnabled = p })
  getMonitoringEnabled = lens _monitoringEnabled (\ c p -> c{_monitoringEnabled = p})
  getMonitoringIP = lens _monitoringIP (\ c i -> c{_monitoringIP = i})
  getMonitoringPort = lens _monitoringPort (\ c p -> c{_monitoringPort = p})
  getLoggingEnabled = lens _loggingEnabled (\ c p -> c{_loggingEnabled = p})
  getLoggingForDevelopment = lens _loggingForDevelopment (\ c p -> c{_loggingForDevelopment = p})
  getDefaultResultLimit = lens _defaultResultLimit (\ c l -> c{_defaultResultLimit = l})
  getUsers = lens _users (\c us -> c{ _users = us })
  getEndpoints = lens _endpoints (\c es -> c{ _endpoints = es })

-- ============================= AppState ============================
type MapRefVar = TVar (Map.Map Text.Text Collection.Collection)

data  MapRef = MapRef{
  _mapRefVar :: MapRefVar
  }

class HasMapRef a where
  getMapRef  :: Lens' a MapRef
  getMapRefVar :: Lens' a MapRefVar

  getMapRefVar = getMapRef . getMapRefVar

instance HasMapRef MapRef where
  getMapRef = id
  getMapRefVar = lens _mapRefVar (\ m v -> m{ _mapRefVar = v })

data AppState = AppState {
  _mapRef::MapRef,
  _config::Config
}

instance HasConfig AppState where
  getConfig = lens _config (\ a c -> a{_config = c})

instance HasMapRef AppState where
  getMapRef = lens _mapRef (\ a m -> a{_mapRef = m})

class AsConfigError a where
  _configError :: Prism' a ConfigError
  _configFileNotFoundError :: Prism' a String
  _configFileNotParsableError :: Prism' a String

  _configFileNotFoundError = _configError . _configFileNotFoundError
  _configFileNotParsableError = _configError . _configFileNotParsableError


data ConfigError = ConfigFileNotFoundError {_msg :: String} |
                   ConfigFileNotParsableError {_msg :: String}
                   deriving (Show)

instance AsConfigError ConfigError where
  _configError = id
  _configFileNotFoundError = prism' ConfigFileNotFoundError (\ case ConfigFileNotFoundError s -> Just s
                                                                    _ -> Nothing)
  _configFileNotParsableError = prism' ConfigFileNotParsableError (\ case ConfigFileNotParsableError s -> Just s
                                                                          _ -> Nothing)


data AppError = AppError |
                AppConfigError { _appConfigError :: ConfigError}
                deriving (Show)

instance AsConfigError AppError where
  _configError = prism' AppConfigError
                       (\ case AppConfigError e -> Just e
                               _                -> Nothing)

newtype AppT m a = App
  {
    unAppT:: (ReaderT AppState (ExceptT AppError m)) a

  } deriving
  (
    Functor,
    Applicative,
    Monad,
    MonadReader AppState,
    MonadError AppError,
    MonadIO
  )


runAppT :: (Monad m) => AppState -> (AppT m a) -> m a
runAppT appState app = do
  let except = runReaderT reader appState
  runResult <- runExceptT except
  case runResult of
    Left e -> error $ show e
    Right a -> return a

  where reader = unAppT app

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

updateCollection :: (HasMapRef m) => m -> Text.Text -> Collection.Collection -> STM ()
updateCollection mr name collection= modifyTVar tvar update
  where
    tvar = view getMapRefVar mr
    update = Map.insert name collection
