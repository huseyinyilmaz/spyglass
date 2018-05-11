module Request where

import GHC.Generics(Generic)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Types(ItemContent, untagged)
import Common()
import Data.Aeson

data IndexType = Infix | Prefix deriving (Show, Eq, Generic)



data Term = Term {
  indexType :: !(Maybe IndexType),
  term :: !Text.Text,
  value :: !ItemContent
  } deriving (Show, Eq, Generic)

-- Request is either a Data request or endpoint request.
-- Endpoint requests pulls its own data after post.
data PostRequest =
  PostDataRequest { values:: ![Term] } |
  PostEndpointRequest {
    timeout:: !(Maybe Integer),
    endpoint:: !Text.Text}
 deriving (Show, Generic, Eq)

instance Aeson.ToJSON IndexType
instance Aeson.FromJSON IndexType
instance Aeson.ToJSON Term
instance Aeson.FromJSON Term
-- instance Aeson.ToJSON PostRequest
-- instance Aeson.FromJSON PostRequest

instance Aeson.ToJSON PostRequest where
  toJSON = genericToJSON untagged

instance FromJSON PostRequest where
  parseJSON = genericParseJSON untagged
