module Middlewares where
import qualified Data.ByteString.Char8 as C8
import System.Remote.Monitoring
import Network.Wai.Metrics
import Data.Monoid((<>))
import Network.Wai (Middleware)
import Types

getMonitoringMiddleware :: Config -> IO (Maybe Middleware)
getMonitoringMiddleware config =
  if (monitoringEnabled config) then do
    let monIp=(monitoringIP config)
        monPort=(monitoringPort config)
    ekg <- forkServer monIp monPort
    waiMetrics <- registerWaiMetrics (serverMetricStore ekg)
    putStrLn ("Monitoring is enabled at " <> (C8.unpack monIp) <> ":" <>(show monPort))
    return (Just (metrics waiMetrics))
  else
    return Nothing

getLoggingMiddleware config =
  if (loggingEnabled config) then
    -- ...
    -- XXX conrintue from here.
  else
    Nothing
