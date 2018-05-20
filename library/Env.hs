module Env where
-- import Data.Text(Text)
-- import qualified Turtle as Turtle
import GHC.Generics
import Data.Aeson
import Common()
-- import qualified Data.List as L
import qualified Data.Yaml as Y
import qualified Data.ByteString as B
import qualified Data.Text as Text
-- import qualified Data.Text as T
-- import System.Directory (getHomeDirectory)
-- import Data.Monoid((<>))
-- import Types
import qualified Collection

data Endpoint = Endpoint {
  path :: Text.Text,
  url :: Text.Text,
  timeout :: Maybe Integer
  } deriving(Generic, Show, Eq)

data AuthUser = AuthUser {
  username:: Text.Text,
  password:: Text.Text
  } deriving (Generic, Show, Eq)

data Config = Config {
  port:: Int,
  gzipEnabled:: Bool,
  monitoringEnabled:: Bool,
  monitoringIP:: Text.Text,
  monitoringPort:: Int,
  loggingEnabled:: Bool,
  loggingForDevelopment:: Bool,
  defaultResultLimit:: Int,
  users :: [AuthUser],
  endpoints :: [Endpoint]
  } deriving (Generic, Show, Eq)

instance ToJSON Endpoint
instance FromJSON Endpoint
instance ToJSON AuthUser
instance FromJSON AuthUser
instance ToJSON Config
instance FromJSON Config

instance Collection.ToCollection Endpoint where
  toCollection Endpoint {url=u, timeout=t} = do
    maybeCollection <- Collection.requestCollectionEndpoint u t 5
    case maybeCollection of
      Just collection -> return collection
      Nothing -> error "Problem Reading endpoint quiting...."

readConfig :: IO Config
readConfig = do
  -- home <- getHomeDirectory
  config <- B.readFile "/etc/spyglass/config.yaml"
  case (Y.decode config):: Maybe Config of
    Just c -> return c
    Nothing -> error "Could parse /etc/spyglass/config.yaml!"
