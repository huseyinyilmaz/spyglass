module Collection where
import Types
import Servant
import Data.Text(Text)
--import Control.Concurent.TVar

-- DB = TVar
commitCollection :: Text -> [CollectionItem] -> AppM (Server CommitCollectionAPI)
commitCollection name _ =
  return $ return $ (CollectionInfo "test")

getCollections :: Server GetCollectionsAPI
getCollections = return [CollectionInfo "test"]
