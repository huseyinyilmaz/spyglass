module State where
import Data.Map.Strict(Map)
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as B
import Collection(Collection)
import Env(Config)
import Control.Monad.Reader

data AppState = AppState {
  _mapRef::STM.TVar (Map B.ByteString Collection),
  _config::Config
}

newtype AppM a
    = AppM
    { unAppM :: ReaderT AppState IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppState)
