module Views(getCollection,
             postCollection) where
-- import qualified Data.Text as Text
-- import Data.Text.Encoding (decodeUtf8)
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
import Utility(toLower)
import Data.Trie as Trie
import Data.Function (on)
import Data.List (groupBy, head, sortBy)

import Network.HTTP.Types.Status(status200)
import qualified Data.ByteString.Lazy.Char8 as LC8
import Network.Wai

makeTrie :: [Item] -> Trie [ItemContent]
makeTrie is = fromList items
  where
    itemList :: [(B.ByteString, ItemContent)]
    itemList = do
      i <- is
      let Item{term=t, content=c} = i
      -- t <- C8.words (term i)
      tt <- C8.tails t
      return (tt, c)
    -- for equal values we dont need to sort by second element here.
    groupedItems = groupBy ((==) `on` fst) (sortBy (flip compare) itemList) -- XXX this line takes a long time.
    listToKV::[(B.ByteString, ItemContent)] -> (B.ByteString, [ItemContent])
    listToKV l = (((toLower . fst) (Data.List.head l)), (fmap snd l))

    items :: [(B.ByteString, [ItemContent])]
    items = fmap listToKV groupedItems

getCollection :: Request -> AppStack
getCollection request = responseLBS status200 [] ((LC8.pack . show) request)

postCollection request = return $ responseLBS status200 [] ((LC8.pack . show) request)


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
