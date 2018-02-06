{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Views(getCollection) where
import qualified Data.Text as Text
import Web.Spock
import Types
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid
import Control.Monad.Trans

-- ActionCtxT ctx (WebStateM conn sess st) () -> SpockCtxM ctx conn sess st ()
--getCollection :: Text.Text -> SpockCtxM ctx conn sess st ()
getCollection :: (SpockState (ActionCtxT ctx m) ~ AppState, MonadIO m, HasSpock (ActionCtxT ctx m)) => Text.Text -> ActionCtxT ctx m b
getCollection name = do
  (AppState ref) <- getState
  text ("Hello " <> name <> ", you are visitor number 0 ")
