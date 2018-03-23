module Collection where

import Data.List (groupBy, head, sort)
import Data.String(IsString)
import Data.Time(UTCTime)
import Data.Trie(Trie, fromList)
import GHC.Generics(Generic)
import Data.Function (on)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

import Utility(reverseSort, toLower)
import Common()

data Endpoint = Endpoint {
  url::B.ByteString,
  endOfLife::UTCTime} deriving(Show, Eq, Generic)

instance Aeson.ToJSON Endpoint
instance Aeson.FromJSON Endpoint

newtype ItemContent = ItemContent {getItemContent:: B.ByteString}
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON, Eq, Monoid, IsString)

data Item = Item {
  term:: !B.ByteString,
  content:: !ItemContent } deriving (Show, Generic, Eq)

instance Aeson.ToJSON Item
instance Aeson.FromJSON Item

data Collection = Collection {
  content::Trie [ItemContent],
  endpoint::Maybe Endpoint }

data PostCollectionBody = PostCollectionBody {
  endpoint:: Maybe Endpoint,
  content :: [Item]

  } deriving (Show, Generic, Eq)

instance Aeson.ToJSON PostCollectionBody
instance Aeson.FromJSON PostCollectionBody

instance Ord ItemContent where
  compare = (compare `on` (B.length . getItemContent))

-- compareOnLength :: ItemContent -> ItemContent -> Ordering
-- compareOnLength = (compare `on` (B.length . getItemContent))

-- reverseLengthSort:: [ItemContent] -> [ItemContent]
-- reverseLengthSort = sortBy (flip compareOnLength)

toCollection :: [Item] -> Collection
toCollection is = Collection c Nothing
  where
    c = makeTrie is

bodyToCollection :: PostCollectionBody -> Collection
bodyToCollection PostCollectionBody{content=is} = toCollection is


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
        values = (reverseSort . (fmap snd)) l
    equalOnFirst = (==) `on` fst
    items :: [(B.ByteString, [ItemContent])]
    items = fmap listToKV groupedItems
    !trie = fromList items
