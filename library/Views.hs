{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Views(getCollection) where
import qualified Data.Text as Text
import Web.Spock
import Types
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid

-- ActionCtxT ctx (WebStateM conn sess st) () -> SpockCtxM ctx conn sess st ()
-- getCollection :: ActionCtxT ctx AppM ()

-- getCollection :: ActionCtxT ctx m0 a
getCollection :: MonadIO m => Text.Text -> ActionCtxT ctx m a
getCollection a = do
  (AppState ref) <- getState
  text ("Hello " <> a <> ", you are visitor number 0 ")
