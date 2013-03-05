{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.ActionLog
  ( -- * Core types and functions
    ActionLog
  , initActionLog
  , HasActionLog(..)
  , ActionType(..)
  , actionToInt
  , intToAction

  -- * Retrieving actions
  , getLoggedAction
  , getAllActions
  , getTenantActions
  , getTenantEntities
  , getTenantUids

  -- * Storing actions
  -- $storingActions
  , logAction
  , loggedInsert
  , loggedReplace
  , loggedUpdate
  , loggedDelete
  , loggedDeleteKey

  -- * Types
  , actionLogEntityDefs
  , LoggedActionGeneric(..)
  , LoggedAction
  , LoggedActionId
  , migrateActionLog

  ) where

------------------------------------------------------------------------------
import           Data.Monoid
import           Heist
import           Snap
import           Snap.Restful
import           Snap.Snaplet.Heist.Compiled
------------------------------------------------------------------------------
import           Snap.Snaplet.ActionLog.API
import           Snap.Snaplet.ActionLog.Resource
import           Snap.Snaplet.ActionLog.Types
import           Paths_snaplet_actionlog


------------------------------------------------------------------------------
-- | Opaque data type holding any state needed by the action log snaplet.
data ActionLog = ActionLog


------------------------------------------------------------------------------
-- | Initializer for the action log snaplet.  It sets up templates, routes,
-- and compiled and interpreted splices.
--
-- Includes two built-in top level splices: actionLogListing and
-- actionLogFilterForm
initActionLog :: (HasActionLog (Handler b b), HasHeist b)
              => Snaplet (Heist b) -> SnapletInit b ActionLog
initActionLog heist = makeSnaplet "actionlog" description datadir $ do
    addConfig heist $ mempty { hcCompiledSplices = actionLogSplices
                             , hcInterpretedSplices = actionLogISplices
                             }
    addTemplates heist "actionlog"
    addRoutes (resourceRoutes actionLogR)
    return ActionLog
  where
    description = "Snaplet providing generalized logging"
    datadir = Just $ liftM (++"/resources") getDataDir

-- $storingActions
-- These functions provide a nice API for logging actions based on database
-- operations.  Typically you should be able to simply substitute the
-- 'loggedInsert', 'loggedUpdate', etc functions in the place of your existing
-- calls to 'insert', 'update', etc from the persistent library.

