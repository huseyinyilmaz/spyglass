module Config where

import GHC.Generics

import qualified Data.Text as Text
import qualified Data.ByteString as B
import qualified Data.Yaml as Y
import Data.Aeson(FromJSON, ToJSON)
import Env(
  Config(..),
  Endpoint(..),
  AuthUser(..),
    )
data ConfigAuthUser = ConfigAuthUser{
  username:: Text.Text,
  password:: Text.Text
} deriving (Show, Generic, Eq)

data ConfigEndpoint = ConfigEndpoint {
  path :: Text.Text,
  url :: Text.Text,
  timeout :: Maybe Integer
  } deriving (Generic, Show, Eq)

data ConfigFile = ConfigFile {
  port:: Int,
  gzipEnabled:: Bool,
  monitoringEnabled:: Bool,
  monitoringIP:: Text.Text,
  monitoringPort:: Int,
  loggingEnabled:: Bool,
  loggingForDevelopment:: Bool,
  defaultResultLimit:: Int,
  users :: [ConfigAuthUser],
  endpoints :: [ConfigEndpoint]
  } deriving (Show, Generic, Eq)

instance ToJSON ConfigFile
instance FromJSON ConfigFile

instance ToJSON ConfigAuthUser
instance FromJSON ConfigAuthUser

instance ToJSON ConfigEndpoint
instance FromJSON ConfigEndpoint


readConfig :: IO (Either String Config)
readConfig = do
  configTxt <- B.readFile "/etc/spyglass/config.yaml"
  return $ case Y.decodeEither' configTxt of
    Left e -> Left $ show e
    Right c -> Right $ toConfig c
  where
    -- map ConfigFile object into Config object.
    toConfig ConfigFile{
      port = port_,
      gzipEnabled = gzipEnabled_,
      monitoringEnabled = monitoringEnabled_,
      monitoringIP = monitoringIP_,
      monitoringPort = monitoringPort_,
      loggingEnabled = loggingEnabled_,
      loggingForDevelopment = loggingForDevelopment_,
      defaultResultLimit = defaultResultLimit_,
      users = users_,
      endpoints = endpoints_
      } = Config {
        _port = port_,
        _gzipEnabled = gzipEnabled_,
        _monitoringEnabled = monitoringEnabled_,
        _monitoringIP = monitoringIP_,
        _monitoringPort = monitoringPort_,
        _loggingEnabled = loggingEnabled_,
        _loggingForDevelopment = loggingForDevelopment_,
        _defaultResultLimit = defaultResultLimit_,
        _users = fmap toAuthUser users_,
        _endpoints = fmap toEndpoint endpoints_
      }

    toAuthUser ConfigAuthUser{
      username = username_,
      password = password_
      } = AuthUser {
        _username = username_,
        _password = password_
        }

    toEndpoint ConfigEndpoint{
      path = path_,
      url = url_,
      timeout = timeout_
      } = Endpoint {
        _path = path_,
        _url = url_,
        _timeout = timeout_
        }
