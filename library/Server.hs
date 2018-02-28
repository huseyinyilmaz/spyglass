module Server where

import Web.Spock
import Web.Spock.Config
import Network.Wai (Middleware)

--import Control.Monad.Trans
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import Types
import qualified Views as Views
import qualified Env as Env
import System.Remote.Monitoring
import Network.Wai.Metrics

-- import Control.Monad(when)
import Data.Monoid((<>))
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
  config <- Env.readConfig
  r <- if (monitoringEnabled config) then do
         let monIp=(monitoringIP config)
             monPort=(monitoringPort config)
         ekg <- forkServer monIp monPort
         waiMetrics <- registerWaiMetrics (serverMetricStore ekg)
         putStrLn ("Monitoring is enabled at " <> (C8.unpack monIp) <> ":" <>(show monPort))
         let r = do
               (middleware (metrics waiMetrics))
               routes
         return r
       else
         return routes
  runSpock (port config) (getApp config r)

--getApp :: Config -> WaiMetrics -> IO Middleware
getApp :: Config -> SpockM () AppSession AppState () -> IO Middleware
getApp config r =
    do ref <- STM.atomically $ STM.newTVar Map.empty
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState ref config)
       spock (spockCfg{ spc_maxRequestSize=Nothing }) r

routes :: SpockM () AppSession AppState ()
routes =
    do get root $
           text "Hello World!"
       get (var) Views.getCollection
       post (var) Views.postCollection
