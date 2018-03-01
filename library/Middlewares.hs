module Middlewares where
import qualified Data.ByteString.Char8 as C8
import System.Remote.Monitoring
import Network.Wai.Metrics
import Data.Monoid((<>))
import Types
import Network.Wai.Middleware.RequestLogger(logStdout, logStdoutDev)
import Web.Spock
import Web.Spock.Routing(RouteM)

getMonitoringMiddleware :: (Monad (t ctx m), Monad m, RouteM t) => Config -> IO (t ctx m ())
getMonitoringMiddleware config = do
  m <- if (monitoringEnabled config) then do
    let monIp=(monitoringIP config)
        monPort=(monitoringPort config)
    ekg <- forkServer monIp monPort
    waiMetrics <- registerWaiMetrics (serverMetricStore ekg)
    putStrLn ("Monitoring is enabled at " <> (C8.unpack monIp) <> ":" <>(show monPort))
    return (middleware (metrics waiMetrics))
  else
    return $ return ()
  return m


getLoggingMiddleware :: (Monad (t ctx m), Monad m, Monad m1, RouteM t) => Config -> m1 (t ctx m ())
getLoggingMiddleware config =
  return $ if (loggingEnabled config) then
    if (loggingForDevelopment config) then
      middleware logStdoutDev
    else
      middleware logStdout
  else
    return ()

getMiddlewares :: (RouteM t, Monad (t ctx m), Monad m) => Config -> IO (t ctx m ())
getMiddlewares config = do
  logging <- getLoggingMiddleware config
  monitoring <- getMonitoringMiddleware config
  return $ do
    logging
    monitoring
