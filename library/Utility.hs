module Utility (noContent) where

import Web.Spock
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as B
import Network.HTTP.Types.Status(created201)

noContent :: MonadIO m => ActionT m B.ByteString
noContent = do
  setStatus created201
  return ""
