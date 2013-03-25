{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
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
  , DeltaField(..)
  , CanDelta(..)
  , storeDeltas
  , getDeltas

  -- * Retrieving actions
  , getLoggedAction
  , getEntityActions
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
  , migrateActionLog
  , LoggedActionGeneric(..)
  , LoggedAction
  , LoggedActionId
  , LoggedActionDetailsGeneric(..)
  , LoggedActionDetails
  , LoggedActionDetailsId

  ) where

------------------------------------------------------------------------------
import           Data.Monoid
import           Data.Text.Encoding
import           Heist
import qualified Heist.Interpreted                     as I
import           Snap
import           Snap.Restful
import           Snap.Snaplet.Heist.Compiled
------------------------------------------------------------------------------
import           Snap.Snaplet.ActionLog.API
import           Snap.Snaplet.ActionLog.Resource
import           Snap.Snaplet.ActionLog.Types
import           Paths_snaplet_actionlog


------------------------------------------------------------------------------
-- | Initializer for the action log snaplet.  It sets up templates, routes,
-- and compiled and interpreted splices.
--
-- Includes two built-in top level splices: actionLogListing and
-- actionLogFilterForm
initActionLog :: (HasActionLog (Handler b b), HasHeist b)
              => Snaplet (Heist b) -> SnapletInit b ActionLog
initActionLog heist = makeSnaplet "actionlog" description datadir $ do
    url <- getSnapletRootURL
    let resource = actionLogR { rRoot = decodeUtf8 url }
    addResourceRelative resource
      [(RIndex, indexH), (RShow, showH)] [] [] heist

    addConfig heist $ mempty
      { hcCompiledSplices = actionLogSplices resource
      , hcInterpretedSplices = actionLogISplices resource

      -- Load time splices are for splices that can be used in the apply and
      -- bind tags.
      , hcLoadTimeSplices =
          [ ("actionlogTemplate", I.textSplice $ decodeUtf8 url)
          ]
      }
    addTemplates heist ""
    return ActionLog
  where
    description = "Snaplet providing generalized logging"
    datadir = Just $ liftM (++"/resources") getDataDir

-- $storingActions
-- These functions provide a nice API for logging actions based on database
-- operations.  Typically you should be able to simply substitute the
-- 'loggedInsert', 'loggedUpdate', etc functions in the place of your existing
-- calls to 'insert', 'update', etc from the persistent library.

