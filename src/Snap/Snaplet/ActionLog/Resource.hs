{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.ActionLog.Resource
  ( actionLogR
  , actionLogSplices
  ) where

------------------------------------------------------------------------------
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString                       as B
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text                             as T
import           Data.Text.Encoding
import           Database.Persist
import           Heist.Compiled
import           Snap
import           Snap.Restful
import           Snap.Snaplet.ActionLog.API
import           Snap.Snaplet.ActionLog.Types
import           Snap.Snaplet.Heist.Compiled
import           Text.Digestive
import qualified Text.Digestive                        as Form
import           Text.Digestive.Heist.Compiled
import           Text.Digestive.Snap                   hiding (method)
import qualified Text.Digestive.Snap                   as Form
------------------------------------------------------------------------------


resourceName :: Text
resourceName = "actionlog"

resourceUrl :: Text
resourceUrl = "actionlog"

tDir :: ByteString -> ByteString
tDir n = B.concat [encodeUtf8 $ resourceUrl, "/", n]


indexH :: HasHeist b => Handler b v ()
indexH = render (tDir "_index")

showH :: HasHeist b => Handler b v ()
showH = render (tDir "_show")

-------------------------------------------------------------------------------
actionLogR :: HasHeist b => Resource b v ()
actionLogR = Resource {
           rName = resourceName
         , rRoot = resourceUrl
         , rHandlers = [ (RIndex, indexH)
                       , (RShow, showH) ]
         , rResourceActions = []
         , rItemActions = []
         }


-------------------------------------------------------------------------------
actionLogSplices :: (HasActionLog n, MonadSnap n) => [(Text, Splice n)]
actionLogSplices =
    [ ("actionLogListing", actionsSplice)
    , ("actionLogFilterForm", logFilterFormSplice)
    ]


actionsSplice :: (HasActionLog n, MonadSnap n) => Splice n
actionsSplice = manyWithSplices runChildren actionSplices $ do
      (_,r) <- runLogFilterForm Nothing
      let filters = case r of
            Nothing -> []
            Just lf -> mkFilters lf
      getTenantActions filters []


actionSplices :: HasActionLog n
              => [(Text, Promise (Entity LoggedAction) -> Splice n)]
actionSplices =
    pureSplices loggedActionCSplices ++
    alCustomSplices


data LogFilter = LogFilter
    { filterUser     :: Maybe Int
    , filterEntity   :: Maybe Text
    , filterEntityId :: Maybe Int
    , filterAction   :: Maybe ActionType
    } deriving (Show)


mkFilters :: LogFilter -> [Filter LoggedAction]
mkFilters (LogFilter u e eid a) =
    maybe [] (\x -> [LoggedActionUserId ==. x]) u ++
    maybe [] (\x -> [LoggedActionEntityName ==. x]) e ++
    maybe [] (\x -> [LoggedActionEntityId ==. x]) eid ++
    maybe [] (\x -> [LoggedActionAction ==. x]) a


------------------------------------------------------------------------------
-- | 
logFilterForm :: HasActionLog m => Maybe LogFilter -> Form Text m LogFilter
logFilterForm d = monadic $ do
    entities <- getTenantEntities
    let entityPairs = noFilter : map (\x -> (Just x,x)) entities
    uids <- getTenantUids
    names <- mapM alGetName uids
    let userPairs = noFilter : (map firstJust $ zip uids names)
    return $ LogFilter
      <$> "user"      .: choice userPairs (filterUser <$> d)
      <*> "entity"    .: choice entityPairs (filterEntity <$> d)
      <*> "entity-id" .: optionalStringRead "id must be an int"
                                            (filterEntityId =<< d)
      <*> "action"    .: choice actions (filterAction <$> d)
  where
    noFilter = (Nothing, "Any")
    firstJust (k,u) = (Just k, u)
    actions = noFilter : map (\x -> (Just x,T.pack $ show x)) [minBound..maxBound]


logFilterFormName :: Text
logFilterFormName = "log-filter-form"


-------------------------------------------------------------------------------
runLogFilterForm :: (HasActionLog m, MonadSnap m)
                 => Maybe LogFilter -> m (View Text, Maybe LogFilter)
runLogFilterForm def = runFormWith cfg logFilterFormName (logFilterForm def)
  where
    cfg = defaultSnapFormConfig { Form.method = Just Form.Post }


-------------------------------------------------------------------------------
logFilterFormSplice :: (HasActionLog m, MonadSnap m) => Splice m
logFilterFormSplice = do
    formSplice' [] [("disableonsingle", disable)] $
      liftM fst $ runLogFilterForm Nothing
  where
    disable _ = do
        mp <- lift $ getParam "single"
        return $ if isJust mp then [("disabled","")] else []

