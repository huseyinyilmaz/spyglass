module Env where
-- import Data.Text(Text)
-- import qualified Turtle as Turtle
import GHC.Generics
import Data.Aeson
import Common()
-- import qualified Data.List as L
import qualified Data.Yaml as Y
import qualified Data.ByteString as B
-- import qualified Data.Text as T
-- import System.Directory (getHomeDirectory)
-- import Data.Monoid((<>))
-- import Types

data AuthUser = AuthUser {
  username:: B.ByteString,
  password:: B.ByteString
  } deriving (Generic, Show, Eq)

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

instance ToJSON AuthUser
instance FromJSON AuthUser
instance ToJSON Config
instance FromJSON Config

readConfig :: IO Config
readConfig = do
  -- home <- getHomeDirectory
  config <- B.readFile "/etc/spyglass/config.yaml"
  case (Y.decode config):: Maybe Config of
    Just c -> return c
    Nothing -> error "Could parse /etc/spyglass/config.yaml!"
