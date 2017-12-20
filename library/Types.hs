module Types where

import Web.Spock
import Web.Spock.Config

import Data.IORef
import qualified Data.Text as Text
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map


data AppSession = EmptySession

data AppState = AppState (STM.TVar (Map.Map Text.Text Text.Text))

type AppM = SpockM () AppSession AppState ()
type ActionCtx ctx a = SpockActionCtx ctx () AppSession AppState a
