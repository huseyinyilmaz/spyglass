module Server where

import Web.Spock
import Web.Spock.Config

--import Control.Monad.Trans
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map

import Types
import qualified Views as Views

main :: IO ()
main =
    do ref <- STM.atomically $ STM.newTVar Map.empty
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState ref)
       runSpock 8080 (spock spockCfg app)

app :: SpockM () AppSession AppState ()
app =
    do get root $
           text "Hello World!"
       get ("api" <//> "v1" <//> var) Views.getCollection
       post ("api" <//> "v1" <//> var) Views.postCollection
