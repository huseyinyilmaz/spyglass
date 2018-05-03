{-# LANGUAGE FlexibleInstances #-}
module Collection where

import Data.List (groupBy, head, sort)
--import Data.String(IsString)
import Data.Time(UTCTime)
import Data.Trie(Trie, fromList)
import GHC.Generics(Generic)
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Time.Clock(getCurrentTime,
                       addUTCTime)
import Data.Maybe(fromMaybe)
import Control.Lens
import Network.HTTP
import qualified Data.Trie as Trie
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
-- import qualified Data.ByteString.Lazy as LB
import Control.Concurrent.MVar (newEmptyMVar, MVar)

import Utility(reverseSort, toLower)
import Types(ItemContent(..))
import Common()
import Utility
import Request(Term(..), PostRequest(..))

-- ============================= Endpoint ============================
-- | Endpoint for a collection. This Endpoint will be called to refresh the data
-- when timeout is achieved.
data Endpoint = Endpoint {
  _endpointUrl::B.ByteString, -- Endpoint url
  _endpointTimeout::Integer,  -- Lifetime of data in seconds.
  _endpointEndOfLife::UTCTime -- End of life time.
  } deriving(Show, Eq, Generic)

instance Aeson.ToJSON Endpoint
instance Aeson.FromJSON Endpoint

endpointUrl :: Lens' Endpoint B.ByteString
endpointUrl = lens _endpointUrl (\e url -> e{_endpointUrl=url})
endpointTimeout :: Lens' Endpoint Integer
endpointTimeout = lens _endpointTimeout (\e url -> e{_endpointTimeout=url})
endpointEndOfLife :: Lens' Endpoint UTCTime
endpointEndOfLife = lens _endpointEndOfLife (\e url -> e{_endpointEndOfLife=url})

-- ============================ Collection ===========================
data Collection = Collection {
  _collectionContent::Trie [ItemContent],
  _collectionEndpoint::Maybe Endpoint,
  _collectionLock:: MVar ()}

collectionContent :: Lens' Collection (Trie [ItemContent])
collectionContent = lens _collectionContent (\c content -> c{_collectionContent=content})
collectionEndpoint :: Lens' Collection (Maybe Endpoint)
collectionEndpoint = lens _collectionEndpoint (\c me -> c{_collectionEndpoint=me})
collectionLock :: Lens' Collection (MVar ())
collectionLock = lens _collectionLock (\c me -> c{_collectionLock=me})

class ToCollection a where
  toCollection :: a -> IO Collection

instance ToCollection [Term] where
  toCollection is = do
    lock <- newEmptyMVar
    return $ Collection c Nothing lock
    where
      c = makeTrie is

bodyToCollection :: PostRequest -> IO Collection
bodyToCollection PostDataRequest{values=is} = toCollection is
bodyToCollection PostEndpointRequest{timeout=t, endpoint=u} = do
  response <- simpleHTTP (getLazyRequest (C8.unpack u))
  body <- getResponseBody response
  case Aeson.decode body of
    Just pr -> do
      now <- getCurrentTime
      collection <- bodyToCollection pr
      let c :: Collection
          c = collection{ _collectionEndpoint=Just $ Endpoint{_endpointUrl=u,
                                                              _endpointTimeout=timeOut,
                                                              _endpointEndOfLife=addUTCTime (fromInteger timeOut) now}}
      return c
    Nothing -> error "Could not parse response"
  where
    timeOut = fromMaybe (60 * 60) t -- default timeout is one hour

isExpired :: Collection -> IO Bool
isExpired c = do
  now <- getCurrentTime
  case _collectionEndpoint c of
    Just Endpoint{_endpointEndOfLife=eol} -> return (now >= eol)
    -- Application assumes that if Collection does not have endpoint isExpired returns false
    Nothing -> return False


lookup :: B.ByteString -> Collection -> [ItemContent]
lookup rawQuery Collection{_collectionContent=trie} = lookupFromTrie trie
  where
    query = toLower rawQuery
    findSubmap = Trie.submap query
    getValues = fmap snd
    merge = foldr (<>) []
    lookupFromTrie = merge . getValues . Trie.toList . findSubmap

makeTrie :: [Term] -> Trie [ItemContent]
makeTrie is = trie
  where
    itemList :: [(B.ByteString, ItemContent)]
    itemList = do
      i <- is
      let Term{term=t, value=c} = i
      -- t <- C8.words (term i)
      tt <- C8.tails t
      return (tt, c)
    groupedItems = groupBy equalOnFirst (sort itemList)

    listToKV::[(B.ByteString, ItemContent)] -> (B.ByteString, [ItemContent])
    listToKV l = (key, vs)
      where
        -- all the values on first elements are same
        -- because groupBy result will be provided here.
        key = (toLower . fst . Data.List.head) l
        vs = (reverseSort . (fmap snd)) l
    equalOnFirst = (==) `on` fst
    items :: [(B.ByteString, [ItemContent])]
    items = fmap listToKV groupedItems
    !trie = fromList items
