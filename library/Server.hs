module Server where

import Web.Spock
import Web.Spock.Config
import Network.Wai (Middleware)

import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import Types
import qualified Views as Views
import qualified Env as Env
import Middlewares

main :: IO ()
main = do
  config <- Env.readConfig
  middlewares <- getMiddlewares config
  let routesWithMiddlewares = do
        middleware middlewares
        routes
  runSpock (port config) (getApp config routesWithMiddlewares)

getApp :: Config -> SpockM () AppSession AppState () -> IO Middleware
getApp config r =
    do ref <- STM.newTVarIO Map.empty
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState ref config)
       spock (spockCfg{ spc_maxRequestSize=Nothing }) r

routes :: SpockM () AppSession AppState ()
routes =
    do get root $
           text "Hello World!"
       get (var) Views.getCollection
       post (var) Views.postCollection
