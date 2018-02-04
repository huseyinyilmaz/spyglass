module Types where

import Web.Spock
import Web.Spock.Config

import Data.IORef
import qualified Data.Text as Text
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
--import qualified Control.Monad.IO.Class.MonadIO as MonadIO

data AppSession = EmptySession

data AppState = AppState (STM.TVar (Map.Map Text.Text Text.Text))

type AppM a = SpockM () AppSession AppState a

-- type ActionCtx ctx a = SpockActionCtx ctx () AppSession AppState a
--type AppActionCtx = MonadIO m => ActionCtxT ctx m a
--type AppActionCtx = ActionCtxT ctx m a
