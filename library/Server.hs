module Server where

import Network.Wai
import Network.Wai.Handler.Warp (run)

import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import Types
import qualified Views as Views
import qualified Env as Env
import Middlewares
import Network.HTTP.Types.Status(status200)
import qualified Data.ByteString.Lazy.Char8 as C8
--import Control.Monad.Trans
import Control.Monad.Reader (ReaderT, runReaderT)
main :: IO ()
main = do
  config <- Env.readConfig
  ref <- STM.newTVarIO Map.empty
  let appState = AppState ref config
  --middlewares <- getMiddlewares config
  run (port config) (getApp config)

getApp :: AppState -> Application
--getApp :: AppState -> Application
getApp config request respond =
  runReaderT response config
  where
    response = mainHandler requestResponde

mainHandler request respond = do
  respond $ case requestMethod request of
    "GET" -> getCollection request
    "POST" -> postCollection request

--(respond $ responseLBS status200 [] ((C8.pack . show) request))
