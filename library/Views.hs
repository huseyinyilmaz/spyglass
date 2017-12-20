{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Views where

import Types
import Web.Spock

import qualified Data.Text as Text
import Data.Monoid

-- ActionCtxT ctx (WebStateM conn sess st) () -> SpockCtxM ctx conn sess st ()
getCollection :: ActionCtxT ctx AppM ()
getCollection = do
  (AppState ref) <- getState
  text ("Hello " <> name <> ", you are visitor number " <> Text.pack (show 0))
