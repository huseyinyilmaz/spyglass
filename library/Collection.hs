module Collection where
import Types
import Servant
import Data.Text(Text)
--import Control.Concurent.TVar

-- DB = TVar
commitCollection :: Text -> [CollectionItem] -> AppM (Server CommitCollectionAPI)
commitCollection collectionName _ = return $ return $ return $ return (CollectionInfo collectionName)

getCollections :: Server GetCollectionsAPI
getCollections = return [CollectionInfo "test"]
