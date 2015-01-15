{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Snap.Snaplet.ActionLog.Schema where

-------------------------------------------------------------------------------
import           Data.Text                            (Text)
import           Data.Time
import           Database.Persist.Quasi
import           Database.Persist.TH
-------------------------------------------------------------------------------
import           Snap.Snaplet.ActionLog.InternalTypes
------------------------------------------------------------------------------


share [mkPersist sqlSettings, mkMigrate "migrateActionLog"]
      $(persistFileWith lowerCaseSettings "schema.txt")

