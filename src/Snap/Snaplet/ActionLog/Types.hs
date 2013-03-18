{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
module Snap.Snaplet.ActionLog.Types where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Monad
import           Data.Readable
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
-- | Opaque data type holding any state needed by the action log snaplet.
data ActionLog = ActionLog


------------------------------------------------------------------------------
-- | The list of entity definitions this snaplet exposes. You need them so
-- that you can append to your application's list of entity definitions and
-- perform the migration in one block.
--
-- Here's an example of how to combine your app's own entity definitions and
-- the action log's in one migration block:
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
-- have to have any custom splices.  If you don't need to add splices to what
-- the snaplet provides by default, just have the custom splice functions
-- return an empty list.
--
-- One potential use for the custom splices might be if you want to display
-- your own custom information in action log lists.  Maybe you want to display
-- a user email address in addition to their name, or maybe instead of
-- displaying raw entity IDs you want to do some DB query to get a different
-- field for display.  The custom splices allow you to make any runtime
-- function of a LoggedAction into a splice that can be displayed in action
-- log templates.
class (HasPersistPool m) => HasActionLog m where

    -- | Gets a tenant ID for the current user of your application.  If your
    -- application is not multi-tenanted, then you can just return a constant
    -- here.
    alGetTenantId :: m Int

    -- | Gets the current user's user ID.  Again, if your application does not
    -- have the concept of a user, you can return a constant.
    alGetAuthUserId :: m Int

    -- | In latency sensitive applications where time stamps are used
    -- frequently, you may want to do the system call once and then cache the
    -- results.  This function allows you to provide your own cache-aware
    -- version of getTime if you choose.  Otherwise you can just lift the
    -- getCurrentTime function from Data.Time.Clock into your monad.
    alGetTime :: m UTCTime

    -- | Function that takes a user ID and returns the user's name, email, or
    -- whatever you want the snaplet to call that user.  This function is used
    -- to generate the user dropdown box in action log filter controls.
    alGetName :: Int -> m Text

    -- | Complied version of any custom splices that you want to be available
    -- inside the @actionLogListing@ splice.
    alCustomCSplices :: [(Text, Promise (Entity LoggedAction) -> Splice m)]

    -- | Interpreted version of any custom splices that you want to be
    -- available inside the @actionLogListing@ splice.
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
-- | Use human readable names for the Show instance.
instance Readable ActionType where
    fromText "Create" = return CreateAction
    fromText "Update" = return UpdateAction
    fromText "Delete" = return DeleteAction
    fromText _ = mzero


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


