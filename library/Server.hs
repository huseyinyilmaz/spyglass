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
import Control.Monad(when)
import Data.Monoid((<>))
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
  config <- Env.readConfig
  when (monitoringEnabled config) $ do
    let monIp=(monitoringIP config)
        monPort=(monitoringPort config)
    _ <- forkServer monIp monPort
    putStrLn ("Monitoring is enabled at " <> (C8.unpack monIp) <> ":" <>(show monPort))
  runSpock (port config) (getApp config)

getApp :: Config -> IO Middleware
getApp config =
    do ref <- STM.atomically $ STM.newTVar Map.empty
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState ref config)
       spock spockCfg routes

routes :: SpockM () AppSession AppState ()
routes =
    do get root $
           text "Hello World!"
       get (var) Views.getCollection
       post (var) Views.postCollection
