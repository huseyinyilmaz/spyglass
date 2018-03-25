module State where
import Data.Text (Text)
import Data.Map.Strict(Map)
import qualified Control.Concurrent.STM as STM

import Collection(Collection)
import Types(Config)
import Control.Monad.Reader

data AppState = AppState {
  getMapRef::STM.TVar (Map Text Collection),
  getConfig::Config
}

newtype AppM a
    = AppM
    { unAppM :: ReaderT AppState IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppState)
