{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}

module Api.Params (
  StandardParams (..),
  defaultStandardParams,
  lookupStandardParams,
  lookupGetParam400,
  lookupGetParam401,
  lookupGetParam403,
  lookupGetParam404,
  lookupEnt,
  lookupLikeEnt,
  lookupStarEnt,
  spToSelect,
  spToSelectE,
  selectListDb,
  selectListDb',
  selectKeysListDb,
  selectKeysListDb',
  selectFirstDb,
  insertDb,
  insertEntityDb,
  updateDb,
  updateWhereDb,
  deleteWhereDb,
  deleteCascadeDb,
  deleteCascadeWhereDb,
  countDb,
  timestamp,
  timestampH,
  timestampH'
) where



import           Data.List             (nub)
import           Data.Time             ()
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Import
import           Misc.Codec
import qualified Database.Esqueleto      as E
-- import           Database.Esqueleto      ((^.))
import           LN.T.Param
import           LN.T.Ent (Ent(..))



data StandardParams = StandardParams {
  spOffset           :: Maybe Int,
  spLimit            :: Maybe Int,
  spSortOrder        :: Maybe SortOrderBy,
  spOrder            :: Maybe OrderBy,
  spTs               :: Maybe UTCTime,
  spUnixTs           :: Maybe Int64,
  spCreatedTs        :: Maybe UTCTime,
  spCreatedUnixTs    :: Maybe Int64,
  spRealIp           :: Maybe Text,
  spIp               :: Maybe Text,
  -- id
  spBoardId          :: Maybe BoardId,
  spBucketId         :: Maybe BucketId,
  spForumId          :: Maybe ForumId,
  spLeuronId         :: Maybe LeuronId,
  spPmId             :: Maybe PmId,
  spReminderId       :: Maybe ReminderId,
  spReminderFolderId :: Maybe ReminderFolderId,
  spResourceId       :: Maybe ResourceId,
  spOrganizationId   :: Maybe OrganizationId,
  spTeamId           :: Maybe TeamId,
  spThreadId         :: Maybe ThreadId,
  spThreadPostId     :: Maybe ThreadPostId,
  spUserId           :: Maybe UserId,
  spParentId         :: Maybe Int64,
  -- ids
  spUserIds          :: Maybe [UserId], -- TODO FIXME: change to spUsersIds, same for the others below
  spThreadIds        :: Maybe [ThreadId],
  spThreadPostIds    :: Maybe [ThreadPostId],
  -- names
  spOrganizationName :: Maybe Text,
  spTeamName         :: Maybe Text,
  spUserNick         :: Maybe Text,
  spForumName        :: Maybe Text,
  spBoardName        :: Maybe Text,
  spThreadName       :: Maybe Text,
  spThreadPostName   :: Maybe Text,
  spResourceName     :: Maybe Text,
  spParentName       :: Maybe Text
} deriving (Eq, Ord, Show)



defaultStandardParams :: StandardParams
defaultStandardParams = StandardParams {

  spOffset           = Nothing,
  spLimit            = Nothing,
  spSortOrder        = Nothing,
  spOrder            = Nothing,
  spTs               = Nothing,
  spUnixTs           = Nothing,
  spCreatedTs        = Nothing,
  spCreatedUnixTs    = Nothing,
  spRealIp           = Nothing,
  spIp               = Nothing,
  -- id
  spBoardId          = Nothing,
  spBucketId         = Nothing,
  spForumId          = Nothing,
  spLeuronId         = Nothing,
  spOrganizationId   = Nothing,
  spPmId             = Nothing,
  spReminderId       = Nothing,
  spReminderFolderId = Nothing,
  spResourceId       = Nothing,
  spTeamId           = Nothing,
  spThreadId         = Nothing,
  spThreadPostId     = Nothing,
  spUserId           = Nothing,
  spParentId         = Nothing,
  -- ids
  spUserIds          = Nothing,
  spThreadIds        = Nothing,
  spThreadPostIds    = Nothing,
  -- names
  spOrganizationName = Nothing,
  spTeamName         = Nothing,
  spUserNick         = Nothing,
  spForumName        = Nothing,
  spBoardName        = Nothing,
  spThreadName       = Nothing,
  spThreadPostName   = Nothing,
  spResourceName     = Nothing,
  spParentName       = Nothing
}



lookupStandardParams :: Handler StandardParams
lookupStandardParams = do

  offset             <- lookupGetParam $ tshow ParamTag_Offset
  limit              <- lookupGetParam $ tshow ParamTag_Limit
  sort_order         <- lookupGetParam $ tshow ParamTag_SortOrder
  order              <- lookupGetParam $ tshow ParamTag_Order
  ts                 <- lookupGetParam $ tshow ParamTag_Timestamp
  unix_ts            <- lookupGetParam $ tshow ParamTag_UnixTimestamp
  created_ts         <- lookupGetParam $ tshow ParamTag_CreatedAtTimestamp
  created_unix_ts    <- lookupGetParam $ tshow ParamTag_CreatedAtUnixTimestamp
  real_ip            <- lookupHeader "real_ip"  -- TODO FIXME: case insensitive bytestring $ tshow ParamTag_RealIP
  ip                 <- lookupGetParam $ tshow ParamTag_IP
  -- id
  board_id           <- lookupGetParam $ tshow ParamTag_ByBoardId
  bucket_id          <- lookupGetParam $ tshow ParamTag_ByBucketId
  forum_id           <- lookupGetParam $ tshow ParamTag_ByForumId
  leuron_id          <- lookupGetParam $ tshow ParamTag_ByLeuronId
  organization_id    <- lookupGetParam $ tshow ParamTag_ByOrganizationId
  pm_id              <- lookupGetParam $ tshow ParamTag_ByPmId
  reminder_id        <- lookupGetParam $ tshow ParamTag_ByReminderId
  reminder_folder_id <- lookupGetParam $ tshow ParamTag_ByReminderFolderId
  resource_id        <- lookupGetParam $ tshow ParamTag_ByResourceId
  team_id            <- lookupGetParam $ tshow ParamTag_ByTeamId
  thread_id          <- lookupGetParam $ tshow ParamTag_ByThreadId
  thread_post_id     <- lookupGetParam $ tshow ParamTag_ByThreadPostId
  user_id            <- lookupGetParam $ tshow ParamTag_ByUserId
  parent_id          <- lookupGetParam $ tshow ParamTag_ByParentId
  -- ids
  user_ids           <- lookupGetParam $ tshow ParamTag_ByUsersIds
  thread_ids         <- lookupGetParam $ tshow ParamTag_ByThreadsIds
  thread_post_ids    <- lookupGetParam $ tshow ParamTag_ByThreadPostsIds
  -- names
  org_name           <- lookupGetParam $ tshow ParamTag_ByOrganizationName
  team_name          <- lookupGetParam $ tshow ParamTag_ByTeamName
  user_nick          <- lookupGetParam $ tshow ParamTag_ByUserNick
  forum_name         <- lookupGetParam $ tshow ParamTag_ByForumName
  board_name         <- lookupGetParam $ tshow ParamTag_ByBoardName
  thread_name        <- lookupGetParam $ tshow ParamTag_ByThreadName
  thread_post_name   <- lookupGetParam $ tshow ParamTag_ByThreadPostName
  resource_name      <- lookupGetParam $ tshow ParamTag_ByResourceName
  parent_name        <- lookupGetParam $ tshow ParamTag_ByParentName

  -- TODO: FIXME: need to safely tread, because the value may not read properly (incorrect input)
  return StandardParams {
    spOffset           = fmap tread offset,
    spLimit            = fmap (abs . tread) limit,
    spSortOrder        = fmap tread sort_order,
    spOrder            = fmap tread order,
    spTs               = fmap tread ts,
    spUnixTs           = fmap tread unix_ts,
    spCreatedTs        = fmap tread created_ts,
    spCreatedUnixTs    = fmap tread created_unix_ts,
    spRealIp           = fmap bread real_ip,
    spIp               = fmap tread ip,
    -- id
    spBoardId          = fmap textToKey' board_id,
    spBucketId         = fmap textToKey' bucket_id,
    spForumId          = fmap textToKey' forum_id,
    spLeuronId         = fmap textToKey' leuron_id,
    spOrganizationId   = fmap textToKey' organization_id,
    spPmId             = fmap textToKey' pm_id,
    spReminderId       = fmap textToKey' reminder_id,
    spReminderFolderId = fmap textToKey' reminder_folder_id,
    spResourceId       = fmap textToKey' resource_id,
    spTeamId           = fmap textToKey' team_id,
    spThreadId         = fmap textToKey' thread_id,
    spThreadPostId     = fmap textToKey' thread_post_id,
    spUserId           = fmap textToKey' user_id,
    spParentId         = fmap tread parent_id,
    -- ids
    spUserIds          = fmap (nub . textToKeys') user_ids,
    spThreadIds        = fmap (nub . textToKeys') thread_ids,
    spThreadPostIds    = fmap (nub . textToKeys') thread_post_ids,
    --- names
    spOrganizationName = org_name,
    spTeamName         = team_name,
    spUserNick         = user_nick,
    spForumName        = forum_name,
    spBoardName        = board_name,
    spThreadName       = thread_name,
    spThreadPostName   = thread_post_name,
    spResourceName     = resource_name,
    spParentName       = parent_name
  }



lookupGetParam400 :: Text -> Handler Text
lookupGetParam400 = lookupGetParamStatus 400

lookupGetParam401 :: Text -> Handler Text
lookupGetParam401 = lookupGetParamStatus 401

lookupGetParam403 :: Text -> Handler Text
lookupGetParam403 = lookupGetParamStatus 403

lookupGetParam404 :: Text -> Handler Text
lookupGetParam404 = lookupGetParamStatus 404



lookupGetParamStatus :: Int -> Text -> Handler Text
lookupGetParamStatus status param = do
  r <- lookupGetParam param
  case r of
    Nothing -> case status of
      400 -> badMethod
      401 -> notAuthenticated
      403 -> permissionDenied "Nope."
      404 -> notFound
      _   -> notFound
    Just r' -> return r'



-- ** Errors
-- , notFound
-- , badMethod
-- , notAuthenticated
-- , permissionDenied
-- , permissionDeniedI
-- , invalidArgs
-- , invalidArgsI




lookupEnt :: StandardParams -> Maybe (Ent, Int64)
lookupEnt (spOrganizationId -> Just v) = Just (Ent_Organization, keyToInt64 v)
lookupEnt (spTeamId -> Just v)         = Just (Ent_Team, keyToInt64 v)
lookupEnt (spUserId -> Just v)         = Just (Ent_User, keyToInt64 v)
lookupEnt (spForumId -> Just v)        = Just (Ent_Forum, keyToInt64 v)
lookupEnt (spBoardId -> Just v)        = Just (Ent_Board, keyToInt64 v)
lookupEnt (spThreadId -> Just v)       = Just (Ent_Thread, keyToInt64 v)
lookupEnt (spThreadPostId -> Just v)   = Just (Ent_ThreadPost, keyToInt64 v)
lookupEnt (spResourceId -> Just v)     = Just (Ent_Resource, keyToInt64 v)
lookupEnt (spLeuronId -> Just v)       = Just (Ent_Leuron, keyToInt64 v)
-- lookupEnt (spComment -> Just v)     = Just (Ent_Comment, keyToInt64 v)
-- lookupEnt (spLikeId -> Just v)         = Just (Ent_Like, keyToInt64 v)
-- lookupEnt (spStarId -> Just v)         = Just (Ent_Star, keyToInt64 v)
lookupEnt _                            = Nothing




lookupLikeEnt :: StandardParams -> Maybe (Ent, Int64)
lookupLikeEnt (spBoardId -> Just v)        = Just (Ent_Board, keyToInt64 v)
lookupLikeEnt (spThreadId -> Just v)       = Just (Ent_Thread, keyToInt64 v)
lookupLikeEnt (spThreadPostId -> Just v)   = Just (Ent_ThreadPost, keyToInt64 v)
lookupLikeEnt (spLeuronId -> Just v)       = Just (Ent_Leuron, keyToInt64 v)
-- lookupLikeEnt (spComment -> Just v)     = Just (Ent_Comment, keyToInt64 v)
lookupLikeEnt _                            = Nothing




lookupStarEnt :: StandardParams -> Maybe (Ent, Int64)
lookupStarEnt (spOrganizationId -> Just v) = Just (Ent_Organization, keyToInt64 v)
lookupStarEnt (spTeamId -> Just v)         = Just (Ent_Team, keyToInt64 v)
lookupStarEnt (spUserId -> Just v)         = Just (Ent_User, keyToInt64 v)
lookupStarEnt (spForumId -> Just v)        = Just (Ent_Forum, keyToInt64 v)
lookupStarEnt (spBoardId -> Just v)        = Just (Ent_Board, keyToInt64 v)
lookupStarEnt (spThreadId -> Just v)       = Just (Ent_Thread, keyToInt64 v)
lookupStarEnt (spThreadPostId -> Just v)   = Just (Ent_ThreadPost, keyToInt64 v)
lookupStarEnt (spResourceId -> Just v)     = Just (Ent_Resource, keyToInt64 v)
lookupStarEnt (spLeuronId -> Just v)       = Just (Ent_Leuron, keyToInt64 v)
-- lookupStarEnt (spComment -> Just v)     = Just (Ent_Comment, keyToInt64 v)
lookupStarEnt _                            = Nothing




-- | For filtering, ordering, limiting etc.
--
spToSelect :: forall typ record. StandardParams -> EntityField record typ -> [SelectOpt record]
spToSelect StandardParams{..} field =
  offset ++ limit ++ order
  where
  offset = case spOffset of
           Nothing      -> []
           Just offset' -> [OffsetBy offset']
  limit  = case spLimit of
           Nothing     -> []
           Just limit' -> [LimitTo limit']
  order  = case spSortOrder of
           Nothing               -> []
           Just SortOrderBy_Asc  -> [Asc field]
           Just SortOrderBy_Dsc  -> [Desc field]
           -- TODO FIXME: need rand
           _                     -> [Asc field]



-- spToSelect :: forall typ record. StandardParams -> EntityField record typ -> [SelectOpt record]
-- esqueleto

spToSelectE ::
  forall (expr :: * -> *) backend (m :: * -> *).
  E.Esqueleto m expr backend =>
  StandardParams -> m ()
spToSelectE StandardParams{..} {-field-} = do
  offset
  limit
--  order
  where
  offset = case spOffset of
           Nothing      -> E.offset 0
           Just offset' -> E.offset $ fromIntegral offset'
  limit  = case spLimit of
           Nothing     -> return ()
           Just limit' -> E.limit $ fromIntegral limit'
           {-
  order  = case spOrder of
           Nothing         -> []
           Just SpOrderAsc  -> E.orderBy [E.asc field]
           Just SpOrderDsc  -> E.orderBy [E.desc field]
           Just SpOrderRand -> E.orderBy [E.rand]
           -}



-- | selectList query helper
--
selectListDb :: forall site val typ.
  (PersistEntity val, PersistQuery (PersistEntityBackend val),
  YesodPersist site,
  PersistEntityBackend val ~ YesodPersistBackend site) =>
  StandardParams
  -> [Filter val]
  -> [SelectOpt val]
  -> EntityField val typ
  -> HandlerT site IO [Entity val]
selectListDb sp query filt field = do

  runDB $ selectList query ((spToSelect sp field) <> filt)



selectListDb' :: forall val typ.
  (PersistEntity val, PersistQuery (PersistEntityBackend val),
  PersistEntityBackend val ~ SqlBackend) =>
  [Filter val]
  -> [SelectOpt val]
  -> EntityField val typ
  -> HandlerT App IO [Entity val]
selectListDb' query filt field = do
  sp <- lookupStandardParams
  selectListDb sp query filt field



-- | selectKeysList query helper
--
selectKeysListDb :: forall site val typ.
                    (PersistEntity val, YesodPersist site,
                     PersistQuery (PersistEntityBackend val),
                     PersistEntityBackend val ~ YesodPersistBackend site) =>
                    StandardParams
                    -> [Filter val]
                    -> [SelectOpt val]
                    -> EntityField val typ
                    -> HandlerT site IO [Key val]
selectKeysListDb sp query filt field = do

  runDB $ selectKeysList query ((spToSelect sp field) <> filt)



-- | selectKeysListDb' helper
--
selectKeysListDb' :: forall val typ.
                     (PersistEntity val, PersistQuery (PersistEntityBackend val),
                      PersistEntityBackend val ~ SqlBackend) =>
                     [Filter val]
                     -> [SelectOpt val]
                     -> EntityField val typ
                     -> HandlerT App IO [Key val]
selectKeysListDb' query filt field = do
  sp <- lookupStandardParams
  selectKeysListDb sp query filt field



-- | selectFirst query helper
--
selectFirstDb :: forall site val.
                 (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val]
                 -> [SelectOpt val] -> HandlerT site IO (Maybe (Entity val))
selectFirstDb query filt = runDB $ selectFirst query filt



-- | insert helper
--
insertDb :: forall a site.
            (PersistEntity a, YesodPersist site,
             PersistStore (PersistEntityBackend a),
             PersistEntityBackend a ~ YesodPersistBackend site) =>
            a -> HandlerT site IO (Key a)
insertDb = runDB . insert



-- | insertEntity helper
--
insertEntityDb :: forall site e.
                  (PersistEntity e, YesodPersist site,
                   PersistStore (YesodPersistBackend site),
                   YesodPersistBackend site ~ PersistEntityBackend e) =>
                  e -> HandlerT site IO (Entity e)
insertEntityDb entity = runDB $ insertEntity entity



-- | update helper
--
updateDb :: forall site val.
            (PersistEntity val, YesodPersist site,
             PersistStore (YesodPersistBackend site),
             YesodPersistBackend site ~ PersistEntityBackend val) =>
            Key val -> [Update val] -> HandlerT site IO ()
updateDb key update_values = runDB $ update key update_values



-- | updateWhere helper
--
updateWhereDb :: forall site val.
                 (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val] -> [Update val] -> HandlerT site IO ()
updateWhereDb filt query = runDB $ updateWhere filt query



-- | deleteWhere helper
--
deleteWhereDb :: forall site val.
                 (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val] -> HandlerT site IO ()
deleteWhereDb filt = runDB $ deleteWhere filt



-- | deleteCascade helper
--
deleteCascadeDb :: forall site record.
                  (YesodPersist site,
                   DeleteCascade record (PersistEntityBackend record),
                   PersistEntityBackend record ~ YesodPersistBackend site) =>
                  Key record -> HandlerT site IO ()
deleteCascadeDb entity = runDB $ deleteCascade entity



-- | deleteCascadeWhere helper
--
deleteCascadeWhereDb :: forall site record.
  (PersistQuery (PersistEntityBackend record),
  DeleteCascade record (PersistEntityBackend record),
  YesodPersist site,
  PersistEntityBackend record ~ YesodPersistBackend site) =>
  [Filter record]
  -> HandlerT site IO ()
deleteCascadeWhereDb = runDB . deleteCascadeWhere



-- | count helper
countDb :: forall site val.
  (PersistEntity val, PersistQuery (YesodPersistBackend site),
  YesodPersist site,
  YesodPersistBackend site ~ PersistEntityBackend val) =>
  [Filter val]
  -> HandlerT site IO Int
countDb query = runDB $ count query



-- | Used to modify createdAt, modifiedAt, etc timestamps
--
-- DANGEROUS IN PRODUCTION, FIXME
--
-- Should have DEVEL/PRODUCTION mode dictate whether this works or not.
--
timestamp :: StandardParams -> IO UTCTime
timestamp sp = do
  case (spTs sp, spUnixTs sp) of
    (Nothing, Nothing) -> getCurrentTime
    (Just ts, _)       -> return ts
    (_, Just ts)       -> return $ posixSecondsToUTCTime $ fromIntegral ts



timestampH :: StandardParams -> Handler UTCTime
timestampH sp = do
  liftIO $ timestamp sp



timestampH' :: Handler UTCTime
timestampH' = do
  sp <- lookupStandardParams
  timestampH sp
