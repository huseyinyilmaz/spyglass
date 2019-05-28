module Middlewares where
import System.Remote.Monitoring
import Network.Wai
import Network.Wai.Metrics
import Data.Monoid((<>))
import Network.Wai.Middleware.RequestLogger(logStdout, logStdoutDev)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.HttpAuth
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import qualified Data.Text as Text
import Control.Lens
import Control.Monad.IO.Class(liftIO)
import Control.Monad.IO.Class(MonadIO)

import Control.Monad.Reader(
  MonadReader,
  ReaderT(..),
  ask
  )

--import Types
import Env(
  Config(..),
  AuthUser(..),
  HasConfig,
  getUsers,
  getMonitoringEnabled,
  getMonitoringIP,
  getMonitoringPort,
  getLoggingEnabled,
  getLoggingForDevelopment,
  getGzipEnabled
  )

getMonitoringMiddleware :: (HasConfig c, MonadReader c m, MonadIO m) => m Middleware
getMonitoringMiddleware = do
  config <- ask
  if (view getMonitoringEnabled config) then do
    let monIp=(view getMonitoringIP config)
        monPort=(view getMonitoringPort config)
    ekg <- liftIO $ forkServer (encodeUtf8 monIp) monPort
    waiMetrics <- liftIO $ registerWaiMetrics (serverMetricStore ekg)
    liftIO $ putStrLn ("Monitoring is enabled at " <> (Text.unpack monIp) <> ":" <>(show monPort))
    return (metrics waiMetrics)
  else
    return id


getLoggingMiddleware :: (HasConfig c, MonadReader c m) => m Middleware
getLoggingMiddleware = do
  config <- ask
  return $ if (view getLoggingEnabled config) then
    if (view getLoggingForDevelopment config) then
      logStdoutDev
    else
      logStdout
  else
    id


getGzipMiddleware :: (HasConfig c, MonadReader c m) => m Middleware
getGzipMiddleware = do
  config <- ask
  return $ if (view getGzipEnabled config) then
             gzip def
           else
             id

getAuthMiddleware :: (HasConfig c, MonadReader c m) => m Middleware
-- getAuthMiddleware Config{ users=us } = do
getAuthMiddleware = do
  config <- ask
  let
    users = view getUsers config
    authSettings = "The sea" { authIsProtected = isProtected }
    isProtected :: Request -> IO Bool
    isProtected = return . (=="POST") . requestMethod
    checkCredentials u p = return (any check users)
      where
        check AuthUser {_username=uu, _password=up} = (uu == decodeUtf8 u) && (up == decodeUtf8 p)
  return (basicAuth checkCredentials authSettings)


getMiddlewares :: (HasConfig c, MonadReader c m, MonadIO m) => m Middleware
getMiddlewares = do
  logging <- getLoggingMiddleware
  monitoring <- getMonitoringMiddleware
  auth <- getAuthMiddleware
  gz <- getGzipMiddleware
  return (gz . logging . autohead . monitoring . auth)
