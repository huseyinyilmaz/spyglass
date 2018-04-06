module Types where

import qualified Data.ByteString as B
import qualified Data.Aeson as Aeson
import GHC.Generics
import Data.String(IsString)
import Data.Function (on)
import Data.Aeson
import Common()

newtype ItemContent = ItemContent {getItemContent:: B.ByteString}
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON, Eq, Monoid, IsString)

instance Ord ItemContent where
  compare = (compare `on` (B.length . getItemContent))

untagged :: Options
untagged = defaultOptions { sumEncoding = UntaggedValue }
