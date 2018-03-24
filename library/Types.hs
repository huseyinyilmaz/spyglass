module Types where

import qualified Data.ByteString as B
import Data.Aeson
import GHC.Generics
import Common()

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
