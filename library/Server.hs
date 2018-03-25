module Server where

import Network.Wai
import Network.Wai.Handler.Warp (run)

import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import Types
import qualified Views as Views
import qualified Env as Env
import Middlewares
import Utility
import State(AppState(..), AppT(..))
-- import Network.HTTP.Types.Status(status200)
-- import qualified Data.ByteString.Lazy.Char8 as C8
--import Control.Monad.Trans
import Control.Monad.Reader (ReaderT, runReaderT)

getState :: Config -> IO AppState
getState config = do
  ref <- STM.newTVarIO Map.empty
  return $ AppState ref config


main :: IO ()
main = do
  config <- Env.readConfig
  appState <- getState config
  middlewares <- getMiddlewares config
  run (port config) (middlewares (getApp appState))

getApp :: AppState -> Application
--getApp :: AppState -> Application
getApp config request respond = do
  resp <- runReaderT response config
  respond resp
  where
    response :: ReaderT AppState IO Response
    response = router request

router :: Request -> ReaderT AppState IO Response
router request = do
  case requestMethod request of
    "GET" -> unAppT $ Views.getCollection request
    "POST" -> unAppT $ Views.postCollection request
    _ -> return notAllowed
--(respond $ responseLBS status200 [] ((C8.pack . show) request))
