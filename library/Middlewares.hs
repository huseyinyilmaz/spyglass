module Middlewares where
import qualified Data.ByteString.Char8 as C8
import System.Remote.Monitoring
import Network.Wai
import Network.Wai.Metrics
import Data.Monoid((<>))
import Types
import Network.Wai.Middleware.RequestLogger(logStdout, logStdoutDev)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.HttpAuth

getMonitoringMiddleware :: Config -> IO Middleware
getMonitoringMiddleware config = do
  if (monitoringEnabled config) then do
    let monIp=(monitoringIP config)
        monPort=(monitoringPort config)
    ekg <- forkServer monIp monPort
    waiMetrics <- registerWaiMetrics (serverMetricStore ekg)
    putStrLn ("Monitoring is enabled at " <> (C8.unpack monIp) <> ":" <>(show monPort))
    return (metrics waiMetrics)
  else
    return id



getLoggingMiddleware :: Config -> IO Middleware
getLoggingMiddleware config =
  return $ if (loggingEnabled config) then
    if (loggingForDevelopment config) then
      logStdoutDev
    else
      logStdout
  else
    id


getGzipMiddleware :: Config -> IO Middleware
getGzipMiddleware config =
  return $ if (loggingEnabled config) then
             gzip def
           else
             id

getAuthMiddleware :: Config -> IO Middleware
getAuthMiddleware Config{ users=us } =
  return (basicAuth checkCredentials authSettings)
  where
    authSettings = "The sea" { authIsProtected = isProtected }
    isProtected :: Request -> IO Bool
    isProtected = return . (=="POST") . requestMethod
    checkCredentials u p = return (any check us)
      where
        check AuthUser {username=uu, password=up} = (uu == u) && (up == p)

getMiddlewares :: Config -> IO Middleware
getMiddlewares config = do
  logging <- getLoggingMiddleware config
  monitoring <- getMonitoringMiddleware config
  auth <- getAuthMiddleware config
  gz <- getGzipMiddleware config
  return (gz . logging . autohead . monitoring . auth)
