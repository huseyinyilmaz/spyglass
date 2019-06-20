module Views(getView,
             postView,
             root,
             debugView) where

import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class(liftIO)
import qualified Data.Map.Strict as Map
import Utility(noContent, errorResponse)
import qualified Data.List
import qualified Utility
import Data.Maybe (fromMaybe)
import Network.HTTP.Types.Status(status200, status404)
import qualified Data.ByteString.Lazy.Char8 as LC8
import Network.Wai
import Control.Monad(join)
import Control.Monad.Reader
import Data.Monoid((<>))
import Data.Aeson(encode, eitherDecode)
import Text.Read(readMaybe)
import Control.Lens
import Collection(bodyToCollection, lookup)


import Request(PostRequest(..))
import Types(ItemContent(..))
--import State(AppState(..), AppStateT)
import State(getCollection)
import Control.Monad.Except(
  MonadError,
  throwError,
  )

import Env(
  Config(..),
  AppT,
  HasConfig,
  HasMapRef,
  AsRuntimeError,
  getDefaultResultLimit,
  getMapRefVar
  )

root :: (HasMapRef c, MonadReader c m, MonadIO m) => Request -> m Response
root _request = do
  config <- ask
  let mapRefVar = view getMapRefVar config
  m <- liftIO $ STM.readTVarIO mapRefVar
  return (responseLBS status200 [] ("Collections:" <> (showByteString (Map.keys m))))
  where
    showByteString :: Show a => a -> LC8.ByteString
    showByteString =  LC8.pack . show

getView :: (AsRuntimeError e,
            HasConfig c,
            HasMapRef c,
            MonadReader c m,
            MonadError e m,
            MonadIO m) => Request -> m Response
getView request = do
  config <- ask
  let defaultResultLimit = view getDefaultResultLimit config
  maybeCollection <- getCollection request
  case maybeCollection of
    Nothing -> return (responseLBS status404 [] "Collection Does Not Exist!")
    Just c -> do
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
              limit = fromMaybe defaultResultLimit maybeLimit
          return (responseLBS status200 [] (encode (take limit result)))
        Nothing -> return (responseLBS status404 [] "Not Found")

postView :: (HasMapRef c, HasConfig c, MonadReader c m, MonadIO m) => Request -> m Response
postView request = do
  config <- ask
  let mapRef = view getMapRefVar config
  m <- liftIO $ STM.readTVarIO mapRef
  body <- liftIO $ strictRequestBody request

  case ((eitherDecode body)::Either String PostRequest) of
    Left e -> return $ errorResponse ("Error: Invalid request body." <> (LC8.pack e))
    Right postCollectionBody -> do
      newCollection <- liftIO $ do
        maybeCollection <- bodyToCollection postCollectionBody
        case maybeCollection of
          Just collection -> return collection
          Nothing -> error "Could not parse document."
      let newMap = Map.insert path newCollection m
      liftIO $ STM.atomically $ STM.writeTVar mapRef newMap
      return noContent
   where
     path = Utility.buildPath (pathInfo request)

debugView :: (HasConfig c, MonadReader c m) => Request -> m Response
debugView request = return response
  where
    response :: Response
    response = responseLBS status200 [] ((LC8.pack . show) request)
