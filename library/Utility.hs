module Utility (toLower,
                noContent,
                errorResponse,
                notFoundResponse,
                notAllowed) where

import Data.Text.Encoding (decodeUtf8)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
-- import Control.Monad.IO.Class (MonadIO)
import Network.Wai
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Network.HTTP.Types.Status(created201,
                                 badRequest400,
                                 notFound404,
                                 methodNotAllowed405)


toLower :: B.ByteString -> B.ByteString
toLower = encodeUtf8 . Text.toLower . decodeUtf8

noContent :: Response
noContent = do
  responseLBS created201 [] ""

notAllowed :: Response
notAllowed = do
  responseLBS methodNotAllowed405 [] ""


errorResponse :: LB.ByteString -> Response
errorResponse e = do
  responseLBS badRequest400 [] e

notFoundResponse :: LB.ByteString -> Response
notFoundResponse e = do
  responseLBS notFound404 [] e
