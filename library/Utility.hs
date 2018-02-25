module Utility (noContent,
                errorResponse,
                notFoundResponse,
                toLower) where

import Data.Text.Encoding (decodeUtf8)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
import Web.Spock
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as B
import Network.HTTP.Types.Status(created201,
                                 badRequest400,
                                 notFound404)


toLower :: B.ByteString -> B.ByteString
toLower = encodeUtf8 . Text.toLower . decodeUtf8

noContent :: MonadIO m => ActionCtxT ctx m ()
noContent = do
  setStatus created201
  bytes ""


errorResponse :: MonadIO m => B.ByteString -> ActionCtxT ctx m ()
errorResponse e = do
  setStatus badRequest400
  bytes e

notFoundResponse :: MonadIO m => B.ByteString -> ActionCtxT ctx m ()
notFoundResponse e = do
  setStatus notFound404
  bytes e
