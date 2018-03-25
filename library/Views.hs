module Views(getCollection,
             postCollection,
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
import qualified Data.ByteString.Lazy.Char8 as LC8
import Network.Wai
import Control.Monad(join)
import Control.Monad.Reader
import Data.Aeson(decode, encode)
import Text.Read(readMaybe)
import Control.Concurrent(forkIO)

import Collection(ItemContent, bodyToCollection, PostCollectionBody(..), lookup)
import Types
--import State(AppState(..), AppStateT)
import State(AppState(..), AppT(..))

import Debug.Trace(traceIO, trace)

getCollection :: Request -> AppT Response
--getCollection :: Request -> ReaderT AppState IO Response
getCollection request = do
  AppState {getMapRef=mapRef,
            getConfig=Config{ defaultResultLimit=defaultLimit }} <- ask
  m <- liftIO $ STM.readTVarIO mapRef

  let maybeResult :: Maybe [ItemContent]
      maybeResult = do
        query <- join $ Data.List.lookup "query" (queryString request)
        c <- Map.lookup name m
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
      _ <- return $ forkIO (do
                               traceIO "We are here!!!!"
                               putStrLn "test")
      return (responseLBS status404 [] "Not Found")
  where
    name = Text.unlines (pathInfo request)

--postCollection :: Request -> ReaderT AppState IO Response
postCollection :: Request -> AppT Response
postCollection request = do
  AppState {getMapRef=mapRef} <- (trace "XXXX") $ ask
  m <- (trace "XXXX-m") $ liftIO $ STM.readTVarIO mapRef
  body <- (trace "XXXX-body") $ liftIO $ strictRequestBody request

  case ((decode body)::Maybe PostCollectionBody) of
    Nothing -> (trace "XXXX-1") $ return $ errorResponse "Error: Invalid request body."
    Just postCollectionBody -> do
      let newCollection = (trace "XXXX-newCollection") $ bodyToCollection postCollectionBody
      let newMap = (trace "XXXX-newMap") $ (Map.insert name newCollection m)
      liftIO $ STM.atomically $ STM.writeTVar mapRef $ (trace "XXXX liftIO") newMap
      return noContent
  where
    name = Text.unlines (pathInfo request)

debugView :: Request -> ReaderT AppState IO Response
debugView request = do
  -- state <- ask
  (lift . return) response
  where
    response :: Response
    response = responseLBS status200 [] ((LC8.pack . show) request)
