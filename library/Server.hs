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
  runAppWithStateOnlyT,
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


main :: IO ()
main = do
  putStrLn getTitle
  eitherConfig <- readConfig
  case eitherConfig of
    Left e -> error e
    Right config -> do
      appState <- getState config
      middlewares <- (runAppWithStateOnlyT appState getMiddlewares):: IO Middleware
      let port = view getPort appState
      let app request respond = runAppT appState respond (getApp request respond)
      putStrLn ("Server is listening at 0.0.0.0:" <> (show port))
      run port $ middlewares app



getApp :: Request -> (Response -> IO ResponseReceived) -> AppT IO ResponseReceived
getApp request respond = do
  resp <- router request
  liftIO $ respond resp

router :: Request -> AppT IO Response
router request = do
  case (rawPathInfo request, requestMethod request) of
    ("/", "GET") -> Views.root request
    ("/", _) -> return notAllowed
    (_, "GET") -> Views.getView request
    (_, "POST") -> Views.postView request
    _ -> return notAllowed
