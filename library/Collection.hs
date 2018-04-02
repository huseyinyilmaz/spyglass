module Collection where

import Data.List (groupBy, head, sort)
--import Data.String(IsString)
import Data.Time(UTCTime)
import Data.Trie(Trie, fromList)
import GHC.Generics(Generic)
import Data.Function (on)
import Data.Monoid ((<>))
import qualified Data.Trie as Trie
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import Network.HTTP

import Utility(reverseSort, toLower)
import Types(ItemContent(..))
import Common()
import Utility
import Request(Term(..), PostRequest(..))

data Endpoint = Endpoint {
  url::B.ByteString, -- Endpoint url
  timeout::Integer,  -- Lifetime of data in seconds.
  endOfLife::UTCTime -- End of life time.
  } deriving(Show, Eq, Generic)

instance Aeson.ToJSON Endpoint
instance Aeson.FromJSON Endpoint

data Collection = Collection {
  content::Trie [ItemContent],
  endpoint::Maybe Endpoint }

toCollection :: [Term] -> Collection
toCollection is = Collection c Nothing
  where
    c = makeTrie is

bodyToCollection :: PostRequest -> IO Collection
bodyToCollection PostDataRequest{values=is} = return (toCollection is)
bodyToCollection PostEndpointRequest{timeout=t, endpoint=u} = do
  response <- simpleHTTP (getLazyRequest (C8.unpack u))
  body <- getResponseBody response
  case Aeson.decode body of
    Just pr -> bodyToCollection pr --TODO add endpoint data.
    Nothing -> error "Could not parse response"


lookup :: B.ByteString -> Collection -> [ItemContent]
lookup rawQuery Collection{content=trie} = lookupFromTrie trie
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
    listToKV l = (key, values)
      where
        -- all the values on first elements are same
        -- because groupBy result will be provided here.
        key = (toLower . fst . Data.List.head) l
        values = (reverseSort . (fmap snd)) l
    equalOnFirst = (==) `on` fst
    items :: [(B.ByteString, [ItemContent])]
    items = fmap listToKV groupedItems
    !trie = fromList items
