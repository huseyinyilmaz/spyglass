module Server where
import Prelude hiding(map)

import Network.Wai
import Network.Wai.Handler.Warp (run)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import Data.Monoid((<>))
import Control.Monad(foldM)
--import Types
import Env(
  AppState(..),
  AppT(..),
  Config(..),
  getEndpoints,
  getPath,
  getPort,
  getConfig,
  MapRef(..),
  runAppT,
  )

import Config(
  readConfig,
  )
import qualified Views as Views
import qualified Env as Env
import Middlewares
import Utility
import Collection(toCollection)
import Control.Lens
import Control.Monad.IO.Class(liftIO)

-- import Network.HTTP.Types.Status(status200)
-- import qualified Data.ByteString.Lazy.Char8 as C8
--import Control.Monad.Trans
import Control.Monad.Reader (ReaderT, runReaderT, ask)

getState :: Config -> IO AppState
getState config = do
  newMap <- foldM addEndpoint Map.empty endpoints
  ref <- STM.newTVarIO newMap
  let newMapRef = MapRef ref
  return $ AppState newMapRef config
  where
    endpoints = view getEndpoints config
    addEndpoint map e = do
      putStrLn ("Initializing " <> show e)
      collection <- toCollection e
      putStrLn "Initialization complete."
      let path = view getPath e
      return $ Map.insert path collection map


mainApp :: AppT IO ()
mainApp = do
  appState <- ask
  let config = view getConfig appState
  let port = view getPort appState
  liftIO $ putStrLn ("Server is listening at 0.0.0.0:" <> (show port))
  middlewares <- getMiddlewares
  liftIO $ run port (middlewares (getApp appState))


main :: IO ()
main = do
  putStrLn getTitle
  eitherConfig <- readConfig
  case eitherConfig of
    Left e -> error e
    Right config -> do
      appState <- getState config
      runAppT appState mainApp


getApp :: AppState -> Application
--getApp :: AppState -> Application
getApp appState request respond = do
  resp <- liftIO $ runAppT appState appT
  respond resp
  where
    appT :: AppT IO Response
    appT = router request

router :: Request -> AppT IO Response
router request = do
  case (rawPathInfo request, requestMethod request) of
    ("/", "GET") -> Views.root request
    ("/", _) -> return notAllowed
    (_, "GET") -> Views.getView request
    (_, "POST") -> Views.postView request
    _ -> return notAllowed
--(respond $ responseLBS status200 [] ((C8.pack . show) request))
