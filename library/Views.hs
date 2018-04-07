module Views(getCollection,
             postCollection,
             root,
             debugView) where

import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class(liftIO)
import qualified Data.Map.Strict as Map
import Utility(noContent, errorResponse)
import qualified Data.List
import Data.Maybe (fromMaybe)
import Network.HTTP.Types.Status(status200, status404)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC8
import Network.Wai
import Control.Monad(join)
import Control.Monad.Reader
import Data.Monoid((<>))
import Data.Aeson(encode, eitherDecode)
import Text.Read(readMaybe)
import Control.Concurrent(forkIO)
import Control.Concurrent(threadDelay)

import Collection(bodyToCollection,
                  lookup,
                  isValid)
import Request(PostRequest(..))
import Types(ItemContent(..))
--import State(AppState(..), AppStateT)
import State(AppState(..), AppM(..))
import Env(Config(..))

root :: AppM Response
root = do
  return (responseLBS status200 [] "Root")

getCollection :: Request -> B.ByteString -> AppM Response
--getCollection :: Request -> ReaderT AppState IO Response
getCollection request path = do
  AppState {_mapRef=mapRef,
            _config=Config{ defaultResultLimit=defaultLimit }} <- ask
  m <- liftIO $ STM.readTVarIO mapRef
  liftIO $ putStrLn $ show request
  case Map.lookup path m of
    Nothing -> return (responseLBS status404 [] "Collection Does Not Exist!")

    Just c -> do
      valid <- liftIO $ isValid c
      if valid then do
        let maybeResult :: Maybe [ItemContent]
            maybeResult = do
              query <- join $ Data.List.lookup "query" (queryString request)
              return $ Collection.lookup query c

        case maybeResult of
          Just result -> do
            let maybeLimit :: Maybe Int
                maybeLimit = do
                  maybeLimitBS <- Data.List.lookup "limit" (queryString request)
                  limitBS <- maybeLimitBS
                  (readMaybe . Text.unpack . decodeUtf8) limitBS
                limit = fromMaybe defaultLimit maybeLimit
            return (responseLBS status200 [] (encode (take limit result)))
          Nothing -> do
            _ <- liftIO $ forkIO $ do
              threadDelay (1000 * 1000 * 10)
              putStrLn "test"
            return (responseLBS status404 [] "Not Found")
      else
        return (responseLBS status404 [] "Not Valid anymore.")

--postCollection :: Request -> ReaderT AppState IO Response
postCollection :: Request -> B.ByteString -> AppM Response
postCollection request path = do
  AppState {_mapRef=mapRef} <- ask
  m <- liftIO $ STM.readTVarIO mapRef
  body <- liftIO $ strictRequestBody request

  case ((eitherDecode body)::Either String PostRequest) of
    Left e -> return $ errorResponse ("Error: Invalid request body." <> (LC8.pack e))
    Right postCollectionBody -> do
      newCollection <- liftIO $ bodyToCollection postCollectionBody
      let newMap = Map.insert path newCollection m
      liftIO $ STM.atomically $ STM.writeTVar mapRef newMap
      return noContent

debugView :: Request -> ReaderT AppState IO Response
debugView request = do
  -- state <- ask
  (lift . return) response
  where
    response :: Response
    response = responseLBS status200 [] ((LC8.pack . show) request)
