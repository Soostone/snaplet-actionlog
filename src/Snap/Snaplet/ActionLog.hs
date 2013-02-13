{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.ActionLog
  ( ActionLog
  , ActionType(..)
  , HasActionLog(..)
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
  , loggedActionCSplices
  , loggedActionISplices

  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Data.Text (Text)
import           Data.Time
import           Database.Persist
import           Database.Persist.EntityDef
import           Database.Persist.GenericSql.Raw
import           Database.Persist.Query.Internal
import           Database.Persist.Quasi
import           Database.Persist.Store
import           Database.Persist.TH          hiding (derivePersistField)
import           Heist.Compiled
import           Snap
import           Snap.Restful
import           Snap.Restful.TH
import           Snap.Snaplet.Persistent
import           Text.XmlHtml
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Enumeration of possible actions in the action log.
data ActionType
  = CreateAction
  | UpdateAction
  | DeleteAction
  deriving (Ord, Eq, Enum)


------------------------------------------------------------------------------
-- | Converts an ActionType into an Int64 to be stored in the database.
actionToInt :: ActionType -> Int
actionToInt CreateAction = 0
actionToInt UpdateAction = 1
actionToInt DeleteAction = 2


intToAction :: Int -> Either Text ActionType
intToAction 0 = Right CreateAction
intToAction 1 = Right UpdateAction
intToAction 2 = Right DeleteAction
intToAction _ = Left "int value is not a valid ActionType"


instance Show ActionType where
    show CreateAction = "Create"
    show UpdateAction = "Update"
    show DeleteAction = "Delete"


instance PersistField ActionType where
    toPersistValue = PersistInt64 . fromIntegral . actionToInt
    fromPersistValue (PersistInt64 n) = intToAction $ fromIntegral n
    fromPersistValue _ = Left "ActionType must be backed by a database int"
    sqlType _ = SqlInt32
    isNullable _ = False


instance PrimSplice ActionType where
    iPrimSplice = iPrimShow
    cPrimSplice = cPrimShow


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


loggedActionCSplices :: [(Text, Entity LoggedAction -> Builder)]
loggedActionCSplices = mapSnd (. entityVal) $(cSplices ''LoggedAction)

loggedActionISplices :: [(Text, Entity LoggedAction -> [Node])]
loggedActionISplices = mapSnd (. entityVal) $(iSplices ''LoggedAction)

class (HasPersistPool m) => HasActionLog m where
    alGetTenantId :: m Int
    alGetAuthUserId :: m Int
    alGetTime :: m UTCTime


data ActionLog = ActionLog


initActionLog :: SnapletInit ActionLog ActionLog
initActionLog = makeSnaplet "actionlog" description datadir $ do
    return ActionLog
  where
    description = "Snaplet providing generalized logging"
    datadir = Nothing --Just $ liftM (++"/resources") getDataDir


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
-- | Gets the LoggedAction entry for the specified entity and id.
getActionLog :: HasPersistPool m
             => Text -> Int -> m (Maybe (Entity LoggedAction))
getActionLog entityName entityId = runPersist $
    selectFirst [ LoggedActionEntityName ==. entityName
                , LoggedActionEntityId ==. entityId
                ] []


