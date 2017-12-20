{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

import Types
import Web.Spock

import qualified Data.Text as Text
import Data.Monoid


getCollection name = do
  (AppState ref) <- getState
  text ("Hello " <> name <> ", you are visitor number " <> Text.pack (show 0))
