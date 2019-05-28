module Server where
import Prelude hiding(map)

import Network.Wai
import Network.Wai.Handler.Warp (run)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import Data.Monoid((<>))
import Control.Monad(foldM)
--import Types
import Env(Config(..))
import qualified Views as Views
import qualified Env as Env
import Middlewares
import Utility
import Collection(toCollection)

import State(AppState(..), AppM(..))
-- import Network.HTTP.Types.Status(status200)
-- import qualified Data.ByteString.Lazy.Char8 as C8
--import Control.Monad.Trans
import Control.Monad.Reader (ReaderT, runReaderT)

getState :: Config -> IO AppState
getState config = do
  newMap <- foldM addEndpoint Map.empty (endpoints config)
  ref <- STM.newTVarIO newMap
  return $ AppState ref config
  where
    addEndpoint map e = do
      putStrLn ("Initializing " <> show e)
      collection <- toCollection e
      putStrLn "Initialization complete."
      return $ Map.insert (Env.path e) collection map
mainApp :: AppT IO ()
mainApp = do
  config <- Env.readConfig
  liftIO $ putStrLn ("Server is listening at 0.0.0.0:" <> (show (port config)))
  appState <- getState config
  middlewares <- liftIO $ getMiddlewares config
  liftIO $ run (port config) (middlewares (getApp appState))


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
  case (rawPathInfo request, requestMethod request) of
    ("/", "GET") -> unAppM $ Views.root
    ("/", _) -> return notAllowed
    (_, "GET") -> unAppM $ Views.getView request
    (_, "POST") -> unAppM $ Views.postView request
    _ -> return notAllowed
--(respond $ responseLBS status200 [] ((C8.pack . show) request))
