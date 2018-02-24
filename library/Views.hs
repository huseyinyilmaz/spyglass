module Views(getCollection,
             postCollection) where
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Web.Spock
import Types
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid
import qualified Data.ByteString as B
-- import Control.Monad.Trans
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class(liftIO)
import qualified Data.Map.Strict as Map
import Utility(noContent)

getCollection :: (SpockState (ActionCtxT ctx m) ~ AppState, MonadIO m, HasSpock (ActionCtxT ctx m)) => B.ByteString -> ActionCtxT ctx m b
getCollection name = do
  (AppState ref) <- getState
  text ("Hello " <> decodeUtf8 name <> ", you are visitor number " <> Text.pack (show (0::Integer)))


postCollection :: (SpockState (ActionCtxT ctx m) ~ AppState, MonadIO m, HasSpock (ActionCtxT ctx m)) => B.ByteString -> ActionCtxT ctx m b
postCollection name = do
  (AppState mapRef) <- getState
  let b = Nothing --b <- Nothing --jsonBody
  m <- liftIO $ STM.readTVarIO mapRef
  case Map.lookup name m of
    Just trieRef -> text "found it"
    Nothing ->
      -- json (b::Maybe [Item])
      noContent -- <<<<<<<<<

-- Got this error from line 31
--
-- [-Wdeferred-type-errors]
--     • Could not deduce: ctx ~ ()
--       from the context: (SpockState (ActionCtxT ctx m) ~ AppState,
--                          MonadIO m,
--                          HasSpock (ActionCtxT ctx m))
--         bound by the type signature for:
--                    postCollection :: (SpockState (ActionCtxT ctx m) ~ AppState,
--                                       MonadIO m, HasSpock (ActionCtxT ctx m)) =>
--                                      B.ByteString -> ActionCtxT ctx m b
--         at /Users/huseyin/projects/spyglass/.stack-work/intero/intero30689fKO-TEMP.hs:22:1-138
--       ‘ctx’ is a rigid type variable bound by
--         the type signature for:
--           postCollection :: forall ctx (m :: * -> *) b.
--                             (SpockState (ActionCtxT ctx m) ~ AppState, MonadIO m,
--                              HasSpock (ActionCtxT ctx m)) =>
--                             B.ByteString -> ActionCtxT ctx m b
--         at /Users/huseyin/projects/spyglass/.stack-work/intero/intero30689fKO-TEMP.hs:22:19
--       Expected type: ActionCtxT ctx m b
--         Actual type: ActionT m B.ByteString
--     • In the expression: noContent
--       In a case alternative: Nothing -> noContent
--       In a stmt of a 'do' block:
--         case Map.lookup name m of {
--           Just trieRef -> text "found it"
--           Nothing -> noContent }
--     • Relevant bindings include
--         postCollection :: B.ByteString -> ActionCtxT ctx m b
--           (bound at /Users/huseyin/projects/spyglass/.stack-work/intero/intero30689fKO-TEMP.hs:23
-- :1)
