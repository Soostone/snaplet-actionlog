{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
module Snap.Snaplet.ActionLog.Types where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Data.Text (Text)
import           Data.Time
import           Database.Persist
import           Database.Persist.EntityDef
import           Database.Persist.Quasi
import           Database.Persist.Store
import           Database.Persist.TH          hiding (derivePersistField)
import           Heist.Compiled
import qualified Heist.Interpreted            as I
import           Snap.Restful
import           Snap.Restful.TH
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


loggedActionCSplices :: [(Text, Entity LoggedAction -> Builder)]
loggedActionCSplices = mapSnd (. entityVal) $(cSplices ''LoggedAction)


loggedActionISplices :: Monad m => LoggedAction -> [(Text, I.Splice m)]
loggedActionISplices = $(iSplices ''LoggedAction)


------------------------------------------------------------------------------
-- | Type class that must be implemented to have an action log.  You do not
-- have to implement both splice functions.  If you only use interpreted mode,
-- you can just use error as the implementation af alCustomCSplices
class (HasPersistPool m) => HasActionLog m where
    alGetTenantId :: m Int
    alGetAuthUserId :: m Int
    alGetTime :: m UTCTime
    alGetName :: Int -> m Text
    alCustomCSplices :: [(Text, Promise (Entity LoggedAction) -> Splice m)]
    alCustomISplices :: Entity LoggedAction -> [(Text, I.Splice m)]


------------------------------------------------------------------------------
-- | Enumeration of possible actions in the action log.
data ActionType
  = CreateAction
  | UpdateAction
  | DeleteAction
  deriving (Ord, Eq, Enum, Bounded)


------------------------------------------------------------------------------
-- | Converts an ActionType into an Int to be stored in the database.  We
-- don't want to use fromEnum here because that will make the numbers
-- sensitive to the ordering of the data type and easier to screw up.
actionToInt :: ActionType -> Int
actionToInt CreateAction = 0
actionToInt UpdateAction = 1
actionToInt DeleteAction = 2


------------------------------------------------------------------------------
-- | Converts an Int into an ActionType.  Again, we want this to be explicit
-- rather than implied by toEnum.
intToAction :: Int -> Either Text ActionType
intToAction 0 = Right CreateAction
intToAction 1 = Right UpdateAction
intToAction 2 = Right DeleteAction
intToAction _ = Left "int value is not a valid ActionType"


------------------------------------------------------------------------------
-- | Use human readable names for the Show instance.
instance Show ActionType where
    show CreateAction = "Create"
    show UpdateAction = "Update"
    show DeleteAction = "Delete"


------------------------------------------------------------------------------
-- | We need to derive PersistField so ActionType can be a column in the
-- LoggedAction table.
instance PersistField ActionType where
    toPersistValue = PersistInt64 . fromIntegral . actionToInt
    fromPersistValue (PersistInt64 n) = intToAction $ fromIntegral n
    fromPersistValue _ = Left "ActionType must be backed by a database int"
    sqlType _ = SqlInt32
    isNullable _ = False


------------------------------------------------------------------------------
-- | Create primitive splices using the show instance.
instance PrimSplice ActionType where
    iPrimSplice = iPrimShow
    cPrimSplice = cPrimShow


