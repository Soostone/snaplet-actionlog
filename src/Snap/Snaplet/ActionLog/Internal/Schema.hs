{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Snap.Snaplet.ActionLog.Internal.Schema where


-------------------------------------------------------------------------------
import           Data.Text                            (Text)
import           Data.Time
import           Database.Persist
import           Database.Persist.Quasi
import           Database.Persist.TH
-------------------------------------------------------------------------------
import           Snap.Snaplet.ActionLog.InternalTypes
-------------------------------------------------------------------------------



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
