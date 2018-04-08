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

import Network.HTTP
import qualified Data.Trie as Trie
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
-- import qualified Data.ByteString.Lazy as LB
import Control.Concurrent.MVar (newMVar, tryTakeMVar, putMVar, MVar)

import Utility(reverseSort, toLower)
import Types(ItemContent(..))
import Common()
import Utility
import Request(Term(..), PostRequest(..))

data Endpoint = Endpoint {
  _endpointUrl::B.ByteString, -- Endpoint url
  _endpointTimeout::Integer,  -- Lifetime of data in seconds.
  _endpointEndOfLife::UTCTime -- End of life time.
  } deriving(Show, Eq, Generic)

instance Aeson.ToJSON Endpoint
instance Aeson.FromJSON Endpoint

data Collection = Collection {
  _collectionContent::Trie [ItemContent],
  _collectionEndpoint::Maybe Endpoint,
  _lock:: MVar ()}


toCollection :: [Term] -> IO Collection
toCollection is = do
  lock <- newMVar ()
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
                                                              _endpointTimeout=to,
                                                              _endpointEndOfLife=addUTCTime (fromInteger to) now}}
      return c
    Nothing -> error "Could not parse response"
  where
    to = fromMaybe (60 * 60) t -- default timeout is one hour

isValid :: Collection -> IO Bool
isValid c = do
  now <- getCurrentTime
  case _collectionEndpoint c of
    Just Endpoint{_endpointEndOfLife=eol} -> return $ now < eol
    Nothing -> return True

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
