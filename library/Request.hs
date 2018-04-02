module Request where

import qualified Data.ByteString as B
import GHC.Generics(Generic)
import qualified Data.Aeson as Aeson
import Types(ItemContent)
import Common()

data IndexType = Infix | Prefix deriving (Show, Eq, Generic)



data Term = Term {
  indexType :: !(Maybe IndexType),
  term :: !B.ByteString,
  value :: !ItemContent
  } deriving (Show, Eq, Generic)

-- Request is either a Data request or endpoint request.
-- Endpoint requests pulls its own data after post.
data PostRequest =
  PostDataRequest { values:: ![Term] } |
  PostEndpointRequest {
    timeout:: !(Maybe Integer),
    endpoint:: !B.ByteString}
 deriving (Show, Generic, Eq)

instance Aeson.ToJSON IndexType
instance Aeson.FromJSON IndexType
instance Aeson.ToJSON Term
instance Aeson.FromJSON Term
instance Aeson.ToJSON PostRequest
instance Aeson.FromJSON PostRequest
