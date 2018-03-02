module Middlewares where
import qualified Data.ByteString.Char8 as C8
import System.Remote.Monitoring
import Network.Wai.Metrics
import Data.Monoid((<>))
import Types
import Network.Wai.Middleware.RequestLogger(logStdout, logStdoutDev)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Gzip

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

getMiddlewares :: Config -> IO Middleware
getMiddlewares config = do
  logging <- getLoggingMiddleware config
  monitoring <- getMonitoringMiddleware config
  gz <- getGzipMiddleware config
  return (gz . logging . monitoring)
