{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.ActionLog.Resource
  ( actionLogR
  , indexH
  , showH
  , actionLogSplices
  , actionLogISplices
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder.Char8
import           Control.Error
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Char8                 as B
import           Data.Monoid
import qualified Data.Readable                         as R
import           Data.Text (Text)
import qualified Data.Text                             as T
import           Database.Persist
import           Heist
import           Heist.Compiled
import qualified Heist.Interpreted                     as I
import           Snap
import           Snap.Restful
import           Snap.Snaplet.ActionLog.API
import           Snap.Snaplet.ActionLog.Types
import           Snap.Snaplet.Heist.Generic
import           Snap.Snaplet.Persistent
import           Text.Digestive
import qualified Text.Digestive                        as Form
import qualified Text.Digestive.Heist                  as DHI
import           Text.Digestive.Heist.Compiled
import           Text.Digestive.Snap                   hiding (method)
import qualified Text.Digestive.Snap                   as Form
import qualified Text.XmlHtml                          as X
------------------------------------------------------------------------------


snapletRender :: HasHeist b => ByteString -> Handler b v ()
snapletRender name = do
    root <- getSnapletRootURL
    let p = B.intercalate "/" $ filter (not . B.null) [root, name]
    gRender p


indexH :: HasHeist b => Handler b v ()
indexH = snapletRender "index"


showH :: HasHeist b => Handler b v ()
showH = snapletRender "show"


-------------------------------------------------------------------------------
-- | A restful-snap resource for the action log CRUD.
actionLogR :: Resource
actionLogR = Resource
    { rName = "actionlog"
    , rRoot = ""
    , rResourceEndpoints = []
    , rItemEndpoints = []
    }


data LogFilter = LogFilter
    { filterUser     :: Maybe Int
    , filterEntity   :: Maybe Text
    , filterEntityId :: Maybe Int
    , filterAction   :: Maybe ActionType
    } deriving (Show)


instance Monoid LogFilter where
    mempty = LogFilter Nothing Nothing Nothing Nothing
    mappend (LogFilter u1 e1 i1 a1) (LogFilter u2 e2 i2 a2) =
      LogFilter (getFirst $ mappend (First u1) (First u2))
                (getFirst $ mappend (First e1) (First e2))
                (getFirst $ mappend (First i1) (First i2))
                (getFirst $ mappend (First a1) (First a2))


mkFilters :: LogFilter -> [Filter LoggedAction]
mkFilters (LogFilter u e eid a) =
    maybe [] (\x -> [LoggedActionUserId ==. x]) u ++
    maybe [] (\x -> [LoggedActionEntityName ==. x]) e ++
    maybe [] (\x -> [LoggedActionEntityId ==. x]) eid ++
    maybe [] (\x -> [LoggedActionAction ==. x]) a


disableOnJust :: (Maybe a -> Form v m b) -> Maybe a -> Form v m b
disableOnJust f Nothing = f Nothing
disableOnJust f def = disable $ f def


------------------------------------------------------------------------------
-- | 
logFilterForm :: HasActionLog m
              => Bool
              -> Maybe LogFilter -> Form Text m LogFilter
logFilterForm isDisabling d = monadic $ do
    entities <- getTenantEntities
    let entityPairs = noFilter : map (\x -> (Just x,x)) entities
    uids <- getTenantUids
    names <- mapM alGetName uids
    let userPairs = noFilter : (map firstJust $ zip uids names)
    return $ LogFilter
      <$> "user"      .: choice userPairs ?$ (filterUser <$> d)
      <*> "entity"    .: choice entityPairs ?$ (filterEntity <$> d)
      <*> "entity-id" .: optionalStringRead "id must be an int" ?$
                                            (filterEntityId =<< d)
      <*> "action"    .: choice actions ?$ (filterAction <$> d)
  where
    noFilter = (Nothing, "Any")
    firstJust (k,u) = (Just k, u)
    actions = noFilter : map (\x -> (Just x,T.pack $ show x)) [minBound..maxBound]
    -- An infix function here makes the syntax nice
    infixr 6 ?$
    (?$) :: (Maybe a -> Form v m b) -> Maybe a -> Form v m b
    (?$) = if isDisabling then disableOnJust else ($)



logFilterFormName :: Text
logFilterFormName = "log-filter-form"


-------------------------------------------------------------------------------
runLogFilterForm :: (HasActionLog m, MonadSnap m)
                 => Bool -> Maybe LogFilter -> m (View Text, Maybe LogFilter)
runLogFilterForm isDisabling def =
    runFormWith cfg logFilterFormName (logFilterForm isDisabling def)
  where
    cfg = defaultSnapFormConfig { Form.method = Just Form.Post }


-------------------------------------------------------------------------------
--                                 Splices
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
actionLogSplices :: (HasActionLog n, MonadSnap n)
                 => Resource -> Splices (Splice n)
actionLogSplices r = mconcat
    [ applyS (return mempty) (coupledSplices r False)
    , splices ]
  where
    splices = do
      "actionDetails" ## actionViewSplice r
      "defaultActions" ## defaultActionsSplice r


coupledSplices :: (HasActionLog n, MonadSnap n)
               => Resource
               -> Bool
               -> Splices (RuntimeSplice n LogFilter -> Splice n)
coupledSplices r b = do
    "actionlogListing" ## actionsSplice r (runLogFilterForm b)
    "actionlogFilterForm" ## logFilterFormSplice (runLogFilterForm b)


getFilterFunc :: Monad n => HeistT n IO (RuntimeSplice n LogFilter)
getFilterFunc = do
    n <- getParamNode
    attrFunc <- runAttributesRaw $ X.elementAttrs n
    return $ do
        as <- attrFunc
        return $ filterCommon as


filterCommon :: [(Text, Text)] -> LogFilter
filterCommon as =
    LogFilter
      (R.fromText =<< lookup "uid" as)
      (lookup "entity" as)
      (R.fromText =<< lookup "entity-id" as)
      (R.fromText =<< lookup "action" as)


------------------------------------------------------------------------------
-- | This is a splice that wraps both the action log filter form splice and
-- the listing splice.  It handles greying out the appropriate form fields and
-- limiting the things in the listing.
defaultActionsSplice :: (MonadSnap m, HasActionLog m) => Resource -> Splice m
defaultActionsSplice r = do
    filterFunc <- getFilterFunc
    withSplices runChildren (coupledSplices r True) filterFunc


actionFromId :: (MonadSnap m, HasPersistPool m)
             => m (Maybe (Entity LoggedAction))
actionFromId = runMaybeT $ do
    idBS <- MaybeT $ getParam "id"
    _id <- R.fromBS idBS
    let key = mkKey _id
    action <- MaybeT $ getLoggedAction key
    return $ Entity key action


actionViewSplice :: (HasActionLog n, MonadSnap n) => Resource -> Splice n
actionViewSplice r = manyWithSplices runChildren (actionSplices r) $ do
    ma <- lift actionFromId
    return $ maybe [] (:[]) ma


actionsSplice :: HasActionLog n
              => Resource
              -> (Maybe a -> n (t, Maybe LogFilter))
              -> RuntimeSplice n LogFilter
              -> Splice n
actionsSplice res form rt = manyWithSplices runChildren (actionSplices res) $ do
    f <- rt
    (_,r) <- lift $ form Nothing
    let filters = case r of
          Nothing -> []
          Just lf -> mkFilters (f `mappend` lf)
    lift $ getTenantActions filters []


actionSplices :: HasActionLog n
              => Resource
              -> Splices (RuntimeSplice n (Entity LoggedAction) -> Splice n)
actionSplices r = mconcat
    [ mapS pureSplice loggedActionCSplices
    , alCustomCSplices
    , mapS ( deferMap (return . Just . DBId . mkWord64 . entityKey)
           . pureSplice . textSplice) (itemCSplices r)
    , splices
    ]
  where
    splices = do
      "loggedActionUserName" ## getUserName
      "loggedActionDetails" ## detailsSplice

    detailsSplice :: HasActionLog n
                  => RuntimeSplice n (Entity LoggedAction)
                  -> Splice n
    detailsSplice rt =
      manyWithSplices runChildren (mapS pureSplice detailsCSplices)
        (lift . getActionDetails . entityKey =<< rt)
    
    getUserName :: HasActionLog n
                => RuntimeSplice n (Entity LoggedAction)
                -> Splice n
    getUserName = bindLater $ \entity -> lift $ do
        name <- alGetName $ loggedActionUserId $ entityVal entity
        return $ fromText name


-------------------------------------------------------------------------------
logFilterFormSplice :: Monad m
                    => (Maybe a -> m (View Text, b))
                    -> RuntimeSplice m a
                    -> Splice m
logFilterFormSplice form rt =
    formSplice mempty mempty $ do
        f <- rt
        lift $ liftM fst $ form (Just f)


-------------------------------------------------------------------------------
--                               Interpreted
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
logFilterFormISplice :: MonadIO m
                     => (Maybe a -> m (View Text, t))
                     -> a -> HeistT m m Template
logFilterFormISplice form f = do
    (v,_) <- lift $ form (Just f)
    localHS (DHI.bindDigestiveSplices v) (DHI.dfForm v >>= I.runNodeList)


--crudUrlISplice :: MonadSnap m => ByteString -> CRUD -> HeistT n m Template
--crudUrlISplice root crud =
--    I.textSplice . decodeUtf8 . (root -/-) =<< go crud
--  where
--    go RIndex = return ""
--    go RCreate = return ""
--    go RShow = (getParam "id") >>= maybe (go RIndex) return
--    go RNew = return "new"
--    go REdit = (getParam "id") >>= maybe (go RIndex) (return . (-/- "edit"))
--    go RUpdate = (getParam "id") >>= maybe (go RIndex) return
--    go RDestroy =
--        (getParam "id") >>= maybe (go RIndex) (return . (-/- "/destroy"))
--
--
--viewLinkISplice :: MonadSnap m => ByteString -> I.Splice m
--viewLinkISplice root = do
--    n <- getParamNode
--    case X.getAttribute "entity" n of
--      Nothing -> return []
--      Just e -> do
--          mid <- lift $ getParam "id"
--          case mid of
--            Nothing -> return []
--            Just _id -> do
--              let page = printf "%s?%s&%s" (kv "entity" e)
--                                           (kv "entity-id" (decodeUtf8 _id))
--                  url = root -/- encodeUtf8 (T.pack page)
--              I.runChildrenWithText [("linkUrl", decodeUtf8 url)]
--  where
--    kv :: Text -> Text -> String
--    kv k v = printf "%s.%s=%s" (T.unpack logFilterFormName)
--                               (T.unpack k) (T.unpack v)


-------------------------------------------------------------------------------
-- | Interpreted splice for an action log listing.
actionLogISplices :: (HasActionLog n, MonadSnap n)
                  => Resource -> Splices (I.Splice n)
actionLogISplices r =
    splices `mappend` coupledISplices r False mempty
  where
    splices = do
      "actionDetails" ## actionDetailsISplice r
      "defaultActions" ## defaultActionsISplice r
    


coupledISplices :: (HasActionLog m, MonadSnap m)
                => Resource -> Bool -> LogFilter -> Splices (I.Splice m)
coupledISplices r b f = do
    "actionlogListing" ## actionLogListingISplice r (runLogFilterForm b) f
    "actionlogFilterForm" ## logFilterFormISplice (runLogFilterForm b) f
    


actionDetailsISplice :: (HasActionLog n, MonadSnap n)
                     => Resource -> I.Splice n
actionDetailsISplice r = do
    ma <- lift $ actionFromId
    maybe (return []) (I.runChildrenWith . actionISplices r) ma


actionLogListingISplice :: HasActionLog m
                        => Resource
                        -> (Maybe a -> m (t, Maybe LogFilter))
                        -> LogFilter
                        -> I.Splice m
actionLogListingISplice res form f = do
    (_,r) <- lift $ form Nothing
    let filters = case r of
          Nothing -> []
          Just lf -> mkFilters (f `mappend` lf)
    actions <- lift $ getTenantActions filters []
    I.mapSplices (I.runChildrenWith . actionISplices res) actions


actionISplices :: HasActionLog m
               => Resource
               -> Entity LoggedAction
               -> Splices (I.Splice m)
actionISplices r e = mconcat
    [ splices
    , loggedActionISplices (entityVal e)
    , alCustomISplices e
    , itemSplices r (DBId $ mkWord64 $ entityKey e)
    ]
  where
    splices = do
      "loggedActionUserName" ## I.textSplice =<< getUserName
      "loggedActionDetails" ## detailsISplice
    getUserName = lift $ alGetName $ loggedActionUserId $ entityVal e
    detailsISplice = do
        ds <- lift $ getActionDetails $ entityKey e
        I.mapSplices (I.runChildrenWith . detailsISplices . entityVal) ds


------------------------------------------------------------------------------
-- | This is a splice that wraps both the action log filter form splice and
-- the listing splice.  It handles greying out the appropriate form fields and
-- limiting the things in the listing.
defaultActionsISplice :: (MonadSnap m, HasActionLog m)
                      => Resource -> I.Splice m
defaultActionsISplice r = do
    n <- getParamNode
    let f = filterCommon $ X.elementAttrs n
    I.runChildrenWith $ coupledISplices r True f


