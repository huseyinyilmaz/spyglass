module Views(getCollection,
             postCollection) where
-- import qualified Data.Text as Text
-- import Data.Text.Encoding (decodeUtf8)
-- import Data.Text.Encoding (encodeUtf8)
import Web.Spock
import Types
-- import Control.Monad.IO.Class (MonadIO)
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
-- import Control.Monad.Trans
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class(liftIO)
import qualified Data.Map.Strict as Map
import Utility(noContent, errorResponse, notFoundResponse)
import Data.Trie as Trie
import Data.Function (on)
import Data.List (groupBy, sort, head)

makeTrie :: [Item] -> Trie [ItemContent]
makeTrie is = fromList items
  where
    itemList = do
      i <- is
      -- t <- C8.words (term i)
      st <- C8.tails (term i)
      return (st, content i)

    -- for equal values we dont need to sort by second element here.
    groupedItems = groupBy ((==) `on` fst) $ sort itemList
    listToKV l = ((fst (Data.List.head l)), (fmap snd l))
    items = fmap listToKV groupedItems

getCollection :: B.ByteString -> View ctx m
getCollection name = do
  (AppState mapRef) <- getState
  maybeQuery <- param "query"
  case maybeQuery of
    Nothing -> notFoundResponse "Error: query parameter required."
    Just query -> do
      m <- liftIO $ STM.readTVarIO mapRef
      case Map.lookup name m of
        Nothing -> notFoundResponse "Error: Collection does not exist."
        Just trieRef -> do
          trie <- liftIO $ STM.readTVarIO trieRef
          json $ (foldr (<>) []) $ fmap snd $ Trie.toList $ Trie.submap query trie

postCollection :: B.ByteString -> View ctx m
postCollection name = do
  (AppState mapRef) <- getState
  m <- liftIO $ STM.readTVarIO mapRef
  b <- jsonBody
  -- (b::Maybe [Item])
  case (b::Maybe [Item]) of
    Nothing -> errorResponse "Error: Invalid request body."
    Just is -> do
      emptyRef <- (liftIO . STM.newTVarIO . makeTrie) is
      let newMap = (Map.insert name emptyRef m)
      liftIO $ STM.atomically$ STM.writeTVar mapRef newMap
      noContent
