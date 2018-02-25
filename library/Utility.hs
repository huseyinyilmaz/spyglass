module Utility (noContent,
                errorResponse,
                notFoundResponse) where

import Web.Spock
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as B
import Network.HTTP.Types.Status(created201,
                                 badRequest400,
                                 notFound404)

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
