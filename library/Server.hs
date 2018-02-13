module Server where

import Web.Spock
import Web.Spock.Config
import Network.Wai (Middleware)

--import Control.Monad.Trans
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import Types
import qualified Views as Views

main :: IO ()
main = runSpock 8080 app

app :: IO Middleware
app =
    do ref <- STM.atomically $ STM.newTVar Map.empty
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState ref)
       spock spockCfg routes
routes :: SpockM () AppSession AppState ()
routes =
    do get root $
           text "Hello World!"
       get (var) Views.getCollection
       post (var) Views.postCollection
