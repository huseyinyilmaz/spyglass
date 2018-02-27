module Env where
-- import Data.Text(Text)
-- import qualified Turtle as Turtle
-- import GHC.Generics
-- import Data.Aeson
-- import qualified Data.List as L
import qualified Data.Yaml as Y
import qualified Data.ByteString as B
-- import qualified Data.Text as T
-- import System.Directory (getHomeDirectory)
-- import Data.Monoid((<>))
import Types


readConfig :: IO Config
readConfig = do
  -- home <- getHomeDirectory
  config <- B.readFile "/etc/spyglass/config.yaml"
  case (Y.decode config):: Maybe Config of
    Just c -> return c
    Nothing -> error "Could parse /etc/spyglass/config.yaml!"
