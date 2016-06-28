{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}

module Api.Params (
  StandardParams (..),
  MStandardParams,
  Sp,
  MSp,
  defaultStandardParams,
  lookupStandardParams,
  lookupSpMay,
  lookupSpBool,
  lookupGetParam400,
  lookupGetParam401,
  lookupGetParam403,
  lookupGetParam404,
  lookupEnt,
  lookupLikeEnt,
  lookupStarEnt,
  spToSelect,
  spToSelectMay,
  spToSelectE,
  selectListDb,
  selectListDbMay,
  selectListDbEither,
  selectListDb',
  selectListDb'',
  selectKeysListDb,
  selectKeysListDb',
  selectKeysListDbMay,
  selectKeysListDbEither,
  selectFirstDb,
  selectFirstDbEither,
  insertDb,
  insertEntityDb,
  updateDb,
  updateWhereDb,
  deleteWhereDb,
  deleteWhereDbEither,
  deleteCascadeDb,
  deleteCascadeDbEither,
  deleteCascadeWhereDb,
  deleteCascadeWhereDbEither,
  countDb,
  timestamp,
  timestampH,
  timestampH'
) where



import           Control
import           Lifted
import           Data.List             (nub)
import           Data.Time             ()
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Import
import           Misc.Codec
import qualified Database.Esqueleto      as E
-- import           Database.Esqueleto      ((^.))
import           LN.T.Param
import           LN.T.Ent (Ent(..))
import           LN.T.Error (ApplicationError(..))



type Sp              = StandardParams
type MSp             = Maybe StandardParams
type MStandardParams = Maybe StandardParams



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
  spParentName       :: Maybe Text,
  spSelf             :: Bool,
  spWithOrganization :: Bool,
  spWithForum        :: Bool,
  spWithBoard        :: Bool,
  spWithThread       :: Bool,
  spWithResource     :: Bool
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
  spParentName       = Nothing,
  spSelf             = False,
  spWithOrganization = False,
  spWithForum        = False,
  spWithBoard        = False,
  spWithThread       = False,
  spWithResource     = False
}



lookupStandardParams :: HandlerEff StandardParams
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
  org_id    <- lookupGetParam $ tshow ParamTag_ByOrganizationId
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
  self               <- (maybe False (const True)) <$> (lookupGetParam $ tshow ParamTag_BySelf)
  with_organization  <- (maybe False (const True)) <$> (lookupGetParam $ tshow ParamTag_WithOrganization)
  with_forum         <- (maybe False (const True)) <$> (lookupGetParam $ tshow ParamTag_WithForum)
  with_board         <- (maybe False (const True)) <$> (lookupGetParam $ tshow ParamTag_WithBoard)
  with_thread        <- (maybe False (const True)) <$> (lookupGetParam $ tshow ParamTag_WithThread)
  with_resource      <- (maybe False (const True)) <$> (lookupGetParam $ tshow ParamTag_WithResource)

  -- TODO: FIXME: need to safely tread, because the value may not read properly (incorrect input)
  pure $ StandardParams {
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
    spOrganizationId   = fmap textToKey' org_id,
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
    spParentName       = parent_name,
    spSelf             = self,
    spWithOrganization = with_organization,
    spWithForum        = with_forum,
    spWithBoard        = with_board,
    spWithThread       = with_thread,
    spWithResource     = with_resource
  }



lookupSpMay :: Maybe StandardParams -> (StandardParams -> Maybe a) -> Maybe a
lookupSpMay Nothing _   = Nothing
lookupSpMay (Just sp) f = f sp



lookupSpBool :: Maybe StandardParams -> (StandardParams -> Bool) -> Bool
lookupSpBool Nothing f   = False
lookupSpBool (Just sp) f = f sp



lookupGetParam400 :: Text -> HandlerEff Text
lookupGetParam400 = lookupGetParamStatus 400

lookupGetParam401 :: Text -> HandlerEff Text
lookupGetParam401 = lookupGetParamStatus 401

lookupGetParam403 :: Text -> HandlerEff Text
lookupGetParam403 = lookupGetParamStatus 403

lookupGetParam404 :: Text -> HandlerEff Text
lookupGetParam404 = lookupGetParamStatus 404



lookupGetParamStatus :: Int -> Text -> HandlerEff Text
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







--
-- TODO FIXME: MOVE THESE DB FUNCTIONS OUT INTO THEIR OWN MODULE
--







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



spToSelectMay :: forall typ record. Maybe StandardParams -> EntityField record typ -> [SelectOpt record]
spToSelectMay Nothing field = []
spToSelectMay (Just StandardParams{..}) field =
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
  -> ControlMA (HandlerT site IO) [Entity val]
selectListDb sp query filt field = do
  _runDB $ selectList query ((spToSelect sp field) <> filt)



selectListDbMay :: forall site val typ.
  (PersistEntity val, PersistQuery (PersistEntityBackend val),
  YesodPersist site,
  PersistEntityBackend val ~ YesodPersistBackend site) =>
  Maybe StandardParams
  -> [Filter val]
  -> [SelectOpt val]
  -> EntityField val typ
  -> ControlMA (HandlerT site IO) [Entity val]
selectListDbMay m_sp query filt field = do
  _runDB $ selectList query ((spToSelectMay m_sp field) <> filt)



selectListDbEither :: forall site val typ.
  (PersistEntity val, PersistQuery (PersistEntityBackend val),
  YesodPersist site,
  PersistEntityBackend val ~ YesodPersistBackend site) =>
  Maybe StandardParams
  -> [Filter val]
  -> [SelectOpt val]
  -> EntityField val typ
  -> ControlMA (HandlerT site IO) (ErrorEff [Entity val])
selectListDbEither m_sp query filt field = do
  Right <$> (_runDB $ selectList query ((spToSelectMay m_sp field) <> filt))



selectListDb' :: forall val typ.
  (PersistEntity val, PersistQuery (PersistEntityBackend val),
  PersistEntityBackend val ~ SqlBackend) =>
  [Filter val]
  -> [SelectOpt val]
  -> EntityField val typ
  -> ControlMA (HandlerT App IO) [Entity val]
selectListDb' query filt field = do
  sp <- lookupStandardParams
  selectListDb sp query filt field



selectListDb'' :: forall val typ.
  (PersistEntity val, PersistQuery (PersistEntityBackend val),
  PersistEntityBackend val ~ SqlBackend) =>
  [Filter val]
  -> [SelectOpt val]
  -> EntityField val typ
  -> ControlMA (HandlerT App IO) [Entity val]
selectListDb'' query filt field = do
  _runDB $ selectList query filt



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
                    -> ControlMA (HandlerT site IO) [Key val]
selectKeysListDb sp query filt field = do
  _runDB $ selectKeysList query ((spToSelect sp field) <> filt)



selectKeysListDbMay :: forall site val typ.
                    (PersistEntity val, YesodPersist site,
                     PersistQuery (PersistEntityBackend val),
                     PersistEntityBackend val ~ YesodPersistBackend site) =>
                    Maybe StandardParams
                    -> [Filter val]
                    -> [SelectOpt val]
                    -> EntityField val typ
                    -> ControlMA (HandlerT site IO) [Key val]
selectKeysListDbMay m_sp query filt field = do
  _runDB $ selectKeysList query ((spToSelectMay m_sp field) <> filt)



selectKeysListDbEither :: forall site val typ.
                    (PersistEntity val, YesodPersist site,
                     PersistQuery (PersistEntityBackend val),
                     PersistEntityBackend val ~ YesodPersistBackend site) =>
                    Maybe StandardParams
                    -> [Filter val]
                    -> [SelectOpt val]
                    -> EntityField val typ
                    -> ControlMA (HandlerT site IO) (ErrorEff [Key val])
selectKeysListDbEither m_sp query filt field = do
  Right <$> (_runDB $ selectKeysList query ((spToSelectMay m_sp field) <> filt))



-- | selectKeysListDb' helper
--
selectKeysListDb' :: forall val typ.
                     (PersistEntity val, PersistQuery (PersistEntityBackend val),
                      PersistEntityBackend val ~ SqlBackend) =>
                     [Filter val]
                     -> [SelectOpt val]
                     -> EntityField val typ
                     -> ControlMA (HandlerT App IO) [Key val]
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
                 -> [SelectOpt val] -> ControlMA (HandlerT site IO) (Maybe (Entity val))
selectFirstDb query filt = _runDB $ selectFirst query filt



selectFirstDbEither :: forall site val.
                 (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val]
                 -> [SelectOpt val] -> ControlMA (HandlerT site IO) (ErrorEff (Entity val))
selectFirstDbEither query filt = do
  m <- _runDB $ selectFirst query filt
  case m of
    Nothing -> pure $ Left Error_NotFound
    Just v  -> pure $ Right v



-- | insert helper
--
insertDb :: forall a site.
            (PersistEntity a, YesodPersist site,
             PersistStore (PersistEntityBackend a),
             PersistEntityBackend a ~ YesodPersistBackend site) =>
            a -> ControlMA (HandlerT site IO) (Key a)
insertDb = _runDB . insert



-- | insertEntity helper
--
insertEntityDb :: forall site e.
                  (PersistEntity e, YesodPersist site,
                   PersistStore (YesodPersistBackend site),
                   YesodPersistBackend site ~ PersistEntityBackend e) =>
                  e -> ControlMA (HandlerT site IO) (Entity e)
insertEntityDb entity = _runDB $ insertEntity entity



-- | update helper
--
updateDb :: forall site val.
            (PersistEntity val, YesodPersist site,
             PersistStore (YesodPersistBackend site),
             YesodPersistBackend site ~ PersistEntityBackend val) =>
            Key val -> [Update val] -> ControlMA (HandlerT site IO) ()
updateDb key update_values = _runDB $ update key update_values



-- | updateWhere helper
--
updateWhereDb :: forall site val.
                 (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val] -> [Update val] -> ControlMA (HandlerT site IO) ()
updateWhereDb filt query = _runDB $ updateWhere filt query



-- | deleteWhere helper
--
deleteWhereDb :: forall site val.
                 (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val] -> ControlMA (HandlerT site IO) ()
deleteWhereDb filt = _runDB $ deleteWhere filt



deleteWhereDbEither :: forall site val.
                 (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val] -> ControlMA (HandlerT site IO) (ErrorEff ())
deleteWhereDbEither filt = Right <$> (_runDB $ deleteWhere filt)



-- | deleteCascade helper
--
deleteCascadeDb :: forall site record.
                  (YesodPersist site,
                   DeleteCascade record (PersistEntityBackend record),
                   PersistEntityBackend record ~ YesodPersistBackend site) =>
                  Key record -> ControlMA (HandlerT site IO) ()
deleteCascadeDb entity = _runDB $ deleteCascade entity



deleteCascadeDbEither :: forall site record.
                  (YesodPersist site,
                   DeleteCascade record (PersistEntityBackend record),
                   PersistEntityBackend record ~ YesodPersistBackend site) =>
                  Key record -> ControlMA (HandlerT site IO) (ErrorEff ())
deleteCascadeDbEither entity = Right <$> (_runDB $ deleteCascade entity)



-- | deleteCascadeWhere helper
--
deleteCascadeWhereDb :: forall site record.
  (PersistQuery (PersistEntityBackend record),
  DeleteCascade record (PersistEntityBackend record),
  YesodPersist site,
  PersistEntityBackend record ~ YesodPersistBackend site) =>
  [Filter record]
  -> ControlMA (HandlerT site IO) ()
deleteCascadeWhereDb = _runDB . deleteCascadeWhere



deleteCascadeWhereDbEither :: forall site record.
  (PersistQuery (PersistEntityBackend record),
  DeleteCascade record (PersistEntityBackend record),
  YesodPersist site,
  PersistEntityBackend record ~ YesodPersistBackend site) =>
  [Filter record]
  -> ControlMA (HandlerT site IO) (ErrorEff ())
deleteCascadeWhereDbEither filt = Right <$> (_runDB $ deleteCascadeWhere filt)



-- | count helper
countDb :: forall site val.
  (PersistEntity val, PersistQuery (YesodPersistBackend site),
  YesodPersist site,
  YesodPersistBackend site ~ PersistEntityBackend val) =>
  [Filter val]
  -> ControlMA (HandlerT site IO) Int
countDb query = _runDB $ count query



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



timestampH :: StandardParams -> HandlerEff UTCTime
timestampH sp = do
  liftIO $ timestamp sp



timestampH' :: HandlerEff UTCTime
timestampH' = lookupStandardParams >>= timestampH
