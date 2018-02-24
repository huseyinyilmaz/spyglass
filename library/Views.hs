module Views(getCollection,
             postCollection) where
--import qualified Data.Text as Text
--import Data.Text.Encoding (decodeUtf8)
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
import Utility(noContent, errorResponse)
import Data.Trie as Trie


getCollection :: B.ByteString -> View ctx m
getCollection name = do
  (AppState _ref) <- getState
  bytes ("hello" <> name) --("Hello " <> decodeUtf8 name <> ", you are visitor number " <> Text.pack (show (0::Integer)))


postCollection :: B.ByteString -> View ctx m
postCollection name = do
  (AppState mapRef) <- getState
  m <- liftIO $ STM.readTVarIO mapRef
  b <- jsonBody
  -- (b::Maybe [Item])
  case (b::Maybe [Item]) of
    Nothing -> errorResponse "Error: Invalid request body."
    Just is -> do
      emptyRef <- liftIO $ STM.newTVarIO newTrie
      let newMap = (Map.insert name emptyRef m)
      liftIO $ STM.atomically$ STM.writeTVar mapRef newMap
      noContent
      where
        termList :: [(B.ByteString, ItemContent)]
        termList = do
          i <- is
          t <- C8.words (term i)
          return (t, content i)
        newTrie = Trie.fromList termList
