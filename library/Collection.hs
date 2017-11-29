module Collection where
import Types
import Servant
--import Control.Concurent.TVar

-- DB = TVar
commitCollection :: Server CommitCollectionAPI
commitCollection c _ = return $ CollectionInfo c

getCollections :: Server GetCollectionsAPI
getCollections = return [CollectionInfo "test"]
