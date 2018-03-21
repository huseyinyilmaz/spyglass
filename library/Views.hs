module Views(getCollection,
             postCollection,
             debugView) where

import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
-- import Data.Text.Encoding (encodeUtf8)
import Types
-- import Control.Monad.IO.Class (MonadIO)
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

-- import Control.Monad.Trans
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class(liftIO)
import qualified Data.Map.Strict as Map
import Utility(toLower, noContent, errorResponse, reverseLengthSort)
import Data.Trie as Trie
import Data.Function (on)
import Data.List (groupBy, head, lookup, sort)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types.Status(status200, status404)
import qualified Data.ByteString.Lazy.Char8 as LC8
import Network.Wai
import Control.Monad.Reader
import Data.Aeson(decode, encode)
import Text.Read(readMaybe)
-- import Data.Ord (comparing)

makeTrie :: [Item] -> Trie [ItemContent]
makeTrie is = trie
  where
    itemList :: [(B.ByteString, ItemContent)]
    itemList = do
      i <- is
      let Item{term=t, content=c} = i
      -- t <- C8.words (term i)
      tt <- C8.tails t
      return (tt, c)
    groupedItems = groupBy equalOnFirst (sort itemList)

    listToKV::[(B.ByteString, ItemContent)] -> (B.ByteString, [ItemContent])
    listToKV l = (key, values)
      where
        -- all the values on first elements are same
        -- because groupBy result will be provided here.
        key = (toLower . fst . Data.List.head) l
        values = (reverseLengthSort . (fmap snd)) l
    equalOnFirst = (==) `on` fst
    items :: [(B.ByteString, [ItemContent])]
    items = fmap listToKV groupedItems
    !trie = fromList items
getCollection :: Request -> ReaderT AppState IO Response
getCollection request = do
  AppState {getMapRef=mapRef,
            getConfig=Config{ defaultResultLimit=defaultLimit }} <- ask
  m <- liftIO $ STM.readTVarIO mapRef

  let maybeQuery :: Maybe [ItemContent]
      maybeQuery = do
        -- Get query string from params
        maybeQ <- Data.List.lookup "query" (queryString request)
        query <- maybeQ
        -- Get path name from map
        Collection{content=trie} <- Map.lookup name m
        -- Get results from query
        return $ (foldr (<>) []) $ fmap snd $ Trie.toList $ Trie.submap (toLower query) trie

  lift $ return $ case maybeQuery of
    Just query -> do
      let maybeLimit :: Maybe Int
          maybeLimit = do
            maybeLimitBS <- Data.List.lookup "limit" (queryString request)
            limitBS <- maybeLimitBS
            (readMaybe . Text.unpack . decodeUtf8) limitBS
          limit = fromMaybe defaultLimit maybeLimit
      responseLBS status200 [] (encode (take limit query))
    Nothing -> responseLBS status404 [] "Not Found"
  where
    name = Text.unlines (pathInfo request)

postCollection :: Request -> ReaderT AppState IO Response
postCollection request = do
  AppState {getMapRef=mapRef} <- ask
  do
    m <- liftIO $ STM.readTVarIO mapRef
    body <- liftIO $ strictRequestBody request
    lift $ case ((decode body)::Maybe [Item]) of
      Nothing -> return $ errorResponse "Error: Invalid request body."
      Just is -> do
        let newCollection = Collection {
              content=makeTrie is,
              endpoint=Nothing}
        let newMap = (Map.insert name newCollection m)

        STM.atomically$ STM.writeTVar mapRef newMap
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

--postCollection request = return $ responseLBS status200 [] ((LC8.pack . show) request)

-- getCollection :: B.ByteString -> IO B.ByteString
-- getCollection name = do
--   AppState {getMapRef=mapRef} <- getState
--   maybeQuery <- param "query"
--   case maybeQuery of
--     Nothing -> notFoundResponse "Error: query parameter required."
--     Just query -> do
--       m <- liftIO $ STM.readTVarIO mapRef
--       case Map.lookup name m of
--         Nothing -> notFoundResponse "Error: Collection does not exist."
--         Just trie -> do
--           json $ (foldr (<>) []) $ fmap snd $ Trie.toList $ Trie.submap (toLower query) trie

-- postCollection :: B.ByteString -> IO B.ByteString
-- postCollection name = do
--   AppState {getMapRef=mapRef} <- getState
--   m <- liftIO $ STM.readTVarIO mapRef
--   b <- jsonBody
--   case (b::Maybe [Item]) of
--     Nothing -> errorResponse "Error: Invalid request body."
--     Just is -> do
--       let newMap = (Map.insert name (makeTrie is) m)
--       liftIO $ STM.atomically$ STM.writeTVar mapRef newMap
--       noContent
