{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.ActionLog
  ( module Snap.Snaplet.ActionLog.Types
  , ActionLog(..)
  , initActionLog

  , logAction
  , loggedInsert
  , loggedReplace
  , loggedUpdate
  , loggedDelete
  , loggedDeleteKey

  , getLoggedAction
  , getAllActions
  , getTenantActions
  , getTenantEntities
  , getTenantUids

  , actionLogR
  , actionLogSplices
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


data ActionLog = ActionLog


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


