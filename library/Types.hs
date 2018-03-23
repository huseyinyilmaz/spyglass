module Types where

import qualified Control.Concurrent.STM as STM
import Data.Map.Strict(Map)
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import Collection(Collection)

data AuthUser = AuthUser {
  username:: B.ByteString,
  password:: B.ByteString
  } deriving (Generic, Show, Eq)

instance ToJSON AuthUser
instance FromJSON AuthUser

data Config = Config {
  port:: Int,
  callbacks:: [B.ByteString],
  gzipEnabled:: Bool,
  monitoringEnabled:: Bool,
  monitoringIP:: B.ByteString,
  monitoringPort:: Int,
  loggingEnabled:: Bool,
  loggingForDevelopment:: Bool,
  defaultResultLimit:: Int,
  users :: [AuthUser]
  } deriving (Generic, Show, Eq)

instance ToJSON Config
instance FromJSON Config

data AppState = AppState {
  getMapRef::STM.TVar (Map Text Collection),
  getConfig::Config
}


-- data AppStack = ReaderT AppState IO Response
