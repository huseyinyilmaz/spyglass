module Server ( main ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.RequestLogger
import Data.Text
import Types
import Collection
import qualified Data.Map.Strict as Map
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans.Reader(runReader)

-- data User = User
--   { userId        :: Int
--   , userFirstName :: String
--   , userLastName  :: String
--   } deriving (Eq, Show)

-- $(deriveJSON defaultOptions ''User)


type API = CollectionAPI

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: AppM Server API
server = commitCollection :<|> getCollections

initialState :: IO State
initialState = do
  col <- STM.atomically $ STM.newTVar Map.empty
  return State{
    getCollection=col
    }


startServer = do
  state <- initialState
  runReader state app
--submit =  ()

-- users :: [User]
-- users :: t -> [User]
-- users a = [ User 1 "Isaac" "Newton"
--         , User 2 "Albert" "Einstein"
--         , User 3 "Stephen" "Hawking"
--         ]

main :: IO ()
main =

  run 1234 (logStdoutDev $ app)
