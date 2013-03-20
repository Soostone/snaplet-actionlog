{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.ActionLog.API where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Text (Text)
import           Data.Text.Encoding
import           Database.Persist
import           Database.Persist.EntityDef
import           Database.Persist.GenericSql
import           Database.Persist.GenericSql.Raw
import           Database.Persist.Query.Internal
import           Database.Persist.Store
import           Snap.Snaplet.Persistent
------------------------------------------------------------------------------
import           Snap.Snaplet.ActionLog.Types


------------------------------------------------------------------------------
--                     Adding entries to the action log
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Calculates a list of fields that changed along with ByteString
-- representations of their old and new values.
storeDeltas :: (HasPersistPool m, CanDelta a)
            => LoggedActionId -> a -> a -> m ()
storeDeltas aid old new = do
    runPersist $ mapM_ ins $ getDeltas old new
  where
    ins (f,o,n) = insert $ LoggedActionDetails aid f o n


------------------------------------------------------------------------------
--                     Adding entries to the action log
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Low level function for logging an action.  Usually you will want to use
-- one of the other functions like 'loggedInsert'.  But when those aren't
-- sufficient, this function provides you maximum control to log actions as
-- you see fit.
logAction :: HasActionLog m
          => Text
              -- ^ Entity name.  If you are logging database modifications,
              -- then this might be the name of the table being operated on.
          -> Int
              -- ^ Entity ID.  This is the primary key for the affected row.
          -> ActionType
              -- ^ Type of action, such as create, update, or delete.
          -> m (Key LoggedAction)
logAction entityName entityId action = do
    tid <- alGetTenantId
    uid <- alGetAuthUserId
    now <- alGetTime
    runPersist $ insert $
      LoggedAction tid uid entityName entityId action now


------------------------------------------------------------------------------
-- | Performs a logged insert into the database.  Just about everything should
-- be inserted using this function instead of @runPersist' . insert@
loggedInsert :: ( HasActionLog m
                , PersistEntity a , PersistEntityBackend a ~ SqlBackend)
             => a -> m (Key a)
loggedInsert val = do
    let entityName = unHaskellName $ entityHaskell $ entityDef val
    recKey <- runPersist $ insert val
    let entityId = mkInt recKey
    logAction entityName entityId CreateAction
    return recKey


------------------------------------------------------------------------------
-- | Performs a logged replace of a database record.
loggedReplace :: ( HasActionLog m, CanDelta a
                 , PersistEntity a, PersistEntityBackend a ~ SqlBackend)
              => Key a -> a -> m ()
loggedReplace key new = do
    old <- runPersist $ get key
    maybe (return ()) (\o -> loggedReplace' key o new) old


------------------------------------------------------------------------------
-- | Performs a logged replace of a database record.
loggedReplace' :: ( HasActionLog m, CanDelta a
                  , PersistEntity a, PersistEntityBackend a ~ SqlBackend)
               => Key a -> a -> a -> m ()
loggedReplace' key old new = do
    let entityName = unHaskellName $ entityHaskell $ entityDef new
    runPersist $ replace key new
    let entityId = mkInt key
    aid <- logAction entityName entityId UpdateAction
    storeDeltas aid old new
    return ()


------------------------------------------------------------------------------
-- | Performs a logged update of a database record.
loggedUpdate :: ( HasActionLog m, CanDelta a
                , PersistEntity a, PersistEntityBackend a ~ SqlBackend)
             => Key a -> [Update a] -> m ()
loggedUpdate key updates = do
    old <- runPersist $ get key
    maybe (return ()) (\o -> loggedUpdate' key o updates) old


------------------------------------------------------------------------------
-- | Performs a logged update of a database record.
loggedUpdate' :: ( HasActionLog m, CanDelta a
                 , PersistEntity a, PersistEntityBackend a ~ SqlBackend)
              => Key a -> a -> [Update a] -> m ()
loggedUpdate' key old updates = do
    val <- runPersist $ updateGet key updates
    new <- runPersist $ get key
    let entityName = unHaskellName $ entityHaskell $ entityDef val
    let entityId = mkInt key
    aid <- logAction entityName entityId UpdateAction
    maybe (return ()) (\n -> storeDeltas aid old n) new
    return ()


------------------------------------------------------------------------------
-- | Performs a logged delete of an entity in the database.
loggedDelete :: ( HasActionLog m
                , PersistEntity a, PersistEntityBackend a ~ SqlBackend)
             => Entity a -> m ()
loggedDelete (Entity key val) = do
    let entityName = unHaskellName $ entityHaskell $ entityDef val
    runPersist $ delete key
    logAction entityName (mkInt key) DeleteAction
    return ()


------------------------------------------------------------------------------
-- | Performs a logged delete of a key in the database.
loggedDeleteKey :: ( HasActionLog m
                   , PersistEntity a, PersistEntityBackend a ~ SqlBackend)
                => Key a -> m ()
loggedDeleteKey key = do
    mval <- runPersist $ Database.Persist.get key
    case mval of
      Nothing -> return ()
      Just val -> do
          -- Only log an action if the value existed
          let entityName = unHaskellName $ entityHaskell $ entityDef val
          runPersist $ delete key
          logAction entityName (mkInt key) DeleteAction
          return ()


------------------------------------------------------------------------------
--                          Retrieving log entries
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Gets the LoggedAction entry for the specified entity and id.
getLoggedAction :: HasPersistPool m
                => LoggedActionId -> m (Maybe LoggedAction)
getLoggedAction actionId = runPersist $ get actionId


------------------------------------------------------------------------------
-- | Gets the LoggedAction entry for the specified entity and id.
getEntityActions :: HasPersistPool m
                 => Text -> Int -> m [Entity LoggedAction]
getEntityActions entityName entityId = runPersist $
    selectList [ LoggedActionEntityName ==. entityName
               , LoggedActionEntityId ==. entityId
               ] []


------------------------------------------------------------------------------
-- | Gets all the actions in the action log.  In multi-tenant applications
-- you probably want this to only be accessible by the administrator of the
-- whole site.
getAllActions :: HasPersistPool m => m [Entity LoggedAction]
getAllActions = runPersist $ selectList [] []


------------------------------------------------------------------------------
-- | Gets all the logged actions for the current tenant.
getTenantActions :: HasActionLog m
                 => [Filter LoggedAction]
                 -> [SelectOpt LoggedAction]
                 -> m [Entity LoggedAction]
getTenantActions filters opts = do
    tid <- alGetTenantId
    runPersist $ selectList ((LoggedActionTenantId ==. tid):filters) opts


------------------------------------------------------------------------------
-- | Gets a list of all entities for a specific tenant.
getTenantEntities :: (HasActionLog m) => m [Text]
getTenantEntities = do
    tid <- alGetTenantId
    liftM (map unSingle) $ runPersist $ rawSql
      "SELECT DISTINCT entity_name from logged_action WHERE tenant_id = ?;"
      [PersistInt64 $ fromIntegral tid]


------------------------------------------------------------------------------
-- | Gets a list of all uids for a specific tenant.
getTenantUids :: (HasActionLog m) => m [Int]
getTenantUids = do
    tid <- alGetTenantId
    liftM (map unSingle) $ runPersist $ rawSql
      "SELECT DISTINCT user_id from logged_action WHERE tenant_id = ?;"
      [PersistInt64 $ fromIntegral tid]


