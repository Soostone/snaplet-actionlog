{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Snap.Snaplet.ActionLog.InternalTypes where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Monad
import           Data.ByteString          (ByteString)
import           Data.Int
import           Data.Readable
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding
import           Data.Time
import           Data.Word
import           Database.Persist
import           Database.Persist.Quasi
import           Database.Persist.Sql
import           Database.Persist.TH
import           Heist.Compiled
import qualified Heist.Interpreted        as I
import           Snap.Restful
import           Snap.Restful.TH
import           Snap.Snaplet.Persistent
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Opaque data type holding any state needed by the action log snaplet.
data ActionLog = ActionLog


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


instance PersistFieldSql ActionType where
    sqlType _ =  SqlInt32

------------------------------------------------------------------------------
-- | Create primitive splices using the show instance.
instance PrimSplice ActionType where
    iPrimSplice = iPrimShow
    cPrimSplice = cPrimShow
