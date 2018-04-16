module Env where
-- import Data.Text(Text)
-- import qualified Turtle as Turtle
import GHC.Generics
import Data.Aeson
import Common()
-- import qualified Data.List as L
import qualified Data.Yaml as Y
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

-- import qualified Data.Text as T
-- import System.Directory (getHomeDirectory)
-- import Data.Monoid((<>))
-- import Types
import qualified Data.Aeson as Aeson
import qualified Collection

data Endpoint = Endpoint {
  path :: B.ByteString,
  url :: B.ByteString,
  timeout :: Maybe Integer
  } deriving(Generic, Show, Eq)

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
  toCollection Endpoint {url=url, timeout=t} = do
    response <- simpleHTTP (getLazyRequest (C8.unpack u))
    body <- getResponseBody response
    case Aeson.decode body of
      Just pr -> do
        now <- getCurrentTime
        collection <- bodyToCollection pr
        let eol = addUTCTime (fromInteger timeOut) now
            ep = Collection.Endpoint{Collection._endpointUrl=u,
                                     Collection._endpointTimeout=timeOut,
                                     Collection._endpointEndOfLife=eol}
            c :: Collection.Collection
            c = collection{ Collection._collectionEndpoint=Just ep }
        return c
      Nothing -> error "Could not parse response"
    where
      timeOut = fromMaybe (60 * 60) t -- default timeout is one hour


readConfig :: IO Config
readConfig = do
  -- home <- getHomeDirectory
  config <- B.readFile "/etc/spyglass/config.yaml"
  case (Y.decode config):: Maybe Config of
    Just c -> return c
    Nothing -> error "Could parse /etc/spyglass/config.yaml!"
