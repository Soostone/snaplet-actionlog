{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.ActionLog.API where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Text (Text)
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
-- | Logs an action.
logAction :: HasActionLog m
          => Text
          -> Int
          -> ActionType
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
loggedReplace :: ( HasActionLog m
                 , PersistEntity a, PersistEntityBackend a ~ SqlBackend)
              => Key a -> a -> m ()
loggedReplace key val = do
    let entityName = unHaskellName $ entityHaskell $ entityDef val
    runPersist $ replace key val
    let entityId = mkInt key
    logAction entityName entityId UpdateAction
    return ()


------------------------------------------------------------------------------
-- | Performs a logged update of a database record.
loggedUpdate :: ( HasActionLog m
                , PersistEntity a, PersistEntityBackend a ~ SqlBackend)
             => Key a -> [Update a] -> m ()
loggedUpdate key updates = do
    val <- runPersist $ updateGet key updates
    let entityName = unHaskellName $ entityHaskell $ entityDef val
    let entityId = mkInt key
    logAction entityName entityId UpdateAction
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
                => Text -> Int -> m (Maybe (Entity LoggedAction))
getLoggedAction entityName entityId = runPersist $
    selectFirst [ LoggedActionEntityName ==. entityName
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


