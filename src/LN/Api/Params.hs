{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module LN.Api.Params (
  StandardParams (..),
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
  lookupLikeEntMay,
  lookupStarEnt,
  spToSelect,
  spToSelectMay,
  spToSelectE,
  timestamp,
  timestampH,
  timestampH'
) where



import           Data.List             (nub)
import           Data.Time             ()
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Database.Esqueleto    as E
import           LN.Control
import           LN.Import
import           LN.Misc.Codec
import           LN.T.Ent              (Ent (..))
import           LN.T.Param



data StandardParams = StandardParams {
  spOffset           :: Maybe Int64,
  spLimit            :: Maybe Int64,
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
  spUserName         :: Maybe Text,
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
  spWithThreadPosts  :: Bool,
  spWithResource     :: Bool
} deriving (Eq, Show)



defaultStandardParams :: StandardParams
defaultStandardParams = StandardParams {

  spOffset           = Nothing,
  spLimit            = defLimit,
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
  spUserName         = Nothing,
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
  spWithThreadPosts  = False,
  spWithResource     = False
}



defLimit :: Maybe Int64
defLimit = Just 10

minLimit :: Int64
minLimit = 1

maxLimit :: Int64
maxLimit = 50


sanitizeLimit :: Int64 -> Int64
sanitizeLimit limit
  | limit < minLimit = minLimit
  | limit > maxLimit = maxLimit
  | otherwise        = limit



minOffset :: Int64
minOffset = 0

sanitizeOffset :: Int64 -> Int64
sanitizeOffset offset
  | offset < minOffset = minOffset
  | otherwise          = offset



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
  user_name          <- lookupGetParam $ tshow ParamTag_ByUserName
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
  with_thread_posts  <- (maybe False (const True)) <$> (lookupGetParam $ tshow ParamTag_WithThreadPosts)
  with_resource      <- (maybe False (const True)) <$> (lookupGetParam $ tshow ParamTag_WithResource)

  -- TODO: FIXME: need to safely tread, because the value may not read properly (incorrect input)
  pure $ StandardParams {
    spOffset           = fmap (sanitizeOffset . tread) offset,
    spLimit            = fmap (sanitizeLimit . tread) limit,
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
    spUserName         = user_name,
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
    spWithThreadPosts  = with_thread_posts,
    spWithResource     = with_resource
  }



lookupSpMay :: Maybe StandardParams -> (StandardParams -> Maybe a) -> Maybe a
lookupSpMay Nothing _   = Nothing
lookupSpMay (Just sp) f = f sp



lookupSpBool :: Maybe StandardParams -> (StandardParams -> Bool) -> Bool
lookupSpBool Nothing _   = False
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
    Just r' -> pure r'



-- ** LN.Errors
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




lookupLikeEntMay :: Maybe StandardParams -> Maybe (Ent, Int64)
lookupLikeEntMay Nothing   = Nothing
lookupLikeEntMay (Just sp) = lookupLikeEnt sp



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
  offset <> limit <> order
  where
  offset = case spOffset of
           Nothing      -> []
           Just offset' -> [OffsetBy $ fromIntegral offset']
  limit  = case spLimit of
           Nothing     -> []
           Just limit' -> [LimitTo $ fromIntegral limit']
  order  = case spSortOrder of
           Nothing               -> []
           Just SortOrderBy_Asc  -> [Asc field]
           Just SortOrderBy_Dsc  -> [Desc field]
           -- TODO FIXME: need rand
           _                     -> [Asc field]



spToSelectMay :: forall typ record. Maybe StandardParams -> EntityField record typ -> [SelectOpt record]
spToSelectMay Nothing _       = []
spToSelectMay (Just sp) field = spToSelect sp field



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
           Nothing     -> pure ()
           Just limit' -> E.limit $ fromIntegral limit'
           {-
  order  = case spOrder of
           Nothing         -> []
           Just SpOrderAsc  -> E.orderBy [E.asc field]
           Just SpOrderDsc  -> E.orderBy [E.desc field]
           Just SpOrderRand -> E.orderBy [E.rand]
           -}



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
    (Just ts, _)       -> pure ts
    (_, Just ts)       -> pure $ posixSecondsToUTCTime $ fromIntegral ts



timestampH :: StandardParams -> HandlerEff UTCTime
timestampH sp = do
  liftIO $ timestamp sp



timestampH' :: HandlerEff UTCTime
timestampH' = lookupStandardParams >>= timestampH
