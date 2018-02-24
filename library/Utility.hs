module Utility (noContent) where

import Web.Spock
import Control.Monad.IO.Class (MonadIO)
--import qualified Data.ByteString as B
import Network.HTTP.Types.Status(created201)

noContent :: MonadIO m => ActionCtxT ctx m ()
noContent = do
  setStatus created201
  bytes "test"