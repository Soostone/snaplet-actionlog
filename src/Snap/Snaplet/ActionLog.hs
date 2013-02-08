{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Snap.Snaplet.ActionLog
  ( ActionLog
  , ActionType(..)
  , LoggedAction
  -- Can't use (..) syntax...apparently because of TH generation
  , LoggedActionId
  , loggedActionTenantId
  , loggedActionUserId
  , loggedActionEntityName
  , loggedActionEntityId
  , loggedActionAction
  , loggedActionAt

  , actionLogEntityDefs
  , migrateActionLog
  , logAction
  , loggedInsert
  , loggedReplace
  , loggedUpdate
  , loggedDelete
  , loggedDeleteKey
  , getActionLog

  ) where

------------------------------------------------------------------------------
import           Data.Text (Text)
import           Data.Time
import           Database.Persist
import           Database.Persist.EntityDef
import           Database.Persist.GenericSql.Raw
import           Database.Persist.Query.Internal
import           Database.Persist.Quasi
import           Database.Persist.TH          hiding (derivePersistField)
import           Snap.Snaplet.Persistent
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | The list of entity definitions this snaplet exposes. You need
-- them so that you can append to your application's list of
-- entity definitions and perform the migration in one block.
--
-- See how this example combined an app's own entity definitions and
-- the auth snaplet's in one migration block:
--
-- > share [mkMigrate "migrateAll"] $
-- >    actionLogEntityDefs ++
-- >    $(persistFileWith lowerCaseSettings "schema.txt")
actionLogEntityDefs :: [EntityDef]
actionLogEntityDefs = $(persistFileWith lowerCaseSettings "schema.txt")


share [mkPersist sqlSettings, mkMigrate "migrateActionLog"]
      $(persistFileWith lowerCaseSettings "schema.txt")


class (HasPersistPool m, PersistQuery m) => HasActionLog m where
    getTenantId :: m Int
    getAuthUserId :: m Int
    getTime :: m UTCTime


------------------------------------------------------------------------------
-- | Enumeration of possible actions in the action log.
data ActionType
  = CreateAction
  | UpdateAction
  | DeleteAction
  deriving (Read, Show, Ord, Eq, Enum)


data ActionLog


------------------------------------------------------------------------------
-- | Converts an ActionType into an Int64 to be stored in the database.
actionVal :: ActionType -> Int
actionVal CreateAction = 0
actionVal UpdateAction = 1
actionVal DeleteAction = 2


------------------------------------------------------------------------------
-- | Logs an action.
logAction :: HasActionLog m
          => Text
          -> Int
          -> ActionType
          -> m (Key LoggedAction)
logAction entityName entityId action = do
    tid <- getTenantId
    uid <- getAuthUserId
    now <- getTime
    runPersist $ insert $
      LoggedAction tid uid entityName entityId (actionVal action) now


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
-- | Gets the LoggedAction entry for the specified entity and id.
getActionLog :: HasPersistPool m
             => Text -> Int -> m (Maybe (Entity LoggedAction))
getActionLog entityName entityId = runPersist $
    selectFirst [ LoggedActionEntityName ==. entityName
                , LoggedActionEntityId ==. entityId
                ] []


