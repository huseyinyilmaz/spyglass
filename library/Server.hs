module Server where

import Network.Wai
import Network.Wai.Handler.Warp (run)

import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import Data.Monoid((<>))

--import Types
import Env(Config(..))
import qualified Views as Views
import qualified Env as Env
import Middlewares
import Utility
import State(AppState(..), AppM(..))
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
  putStrLn getTitle
  config <- Env.readConfig
  putStrLn ("Server is listening at 0.0.0.0:" <> (show (port config)))
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
    "GET" -> unAppM $ Views.getCollection request
    "POST" -> unAppM $ Views.postCollection request
    _ -> return notAllowed
--(respond $ responseLBS status200 [] ((C8.pack . show) request))
