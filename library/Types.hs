module Types where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import GHC.Generics
import Data.String(IsString)
import Data.Function (on)
import Data.Aeson
import Common()

newtype ItemContent = ItemContent {getItemContent:: Text.Text}
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON, Eq, Monoid, IsString)

instance Ord ItemContent where
  compare = (compare `on` (Text.length . getItemContent))

untagged :: Options
untagged = defaultOptions { sumEncoding = UntaggedValue }
