module All.GlobalGroup (
  -- handler
  getGlobalGroupsR,
  postGlobalGroupR0,
  getGlobalGroupR,
  getGlobalGroupH,
  putGlobalGroupR,
  deleteGlobalGroupR,

  getCountGlobalGroupsR,

  getGlobalGroupStatsR,
  getGlobalGroupStatR,

  getGlobalGroupPacksR,
  getGlobalGroupPackR,
  getGlobalGroupPackH,



  -- model/functions
  globalGroupRequestToGlobalGroup,
  globalGroupToResponse,
  globalGroupsToResponses,



  -- model/internal
  getGlobalGroupsM,
  getGlobalGroups_ByUserIdM,
  getGlobalGroups_ByEverythingM,

  getGlobalGroupM,
  insertGlobalGroupM,
  updateGlobalGroupM,
  deleteGlobalGroupM,


  -- model/pack

) where



import           Handler.Prelude
import           Model.Prelude



getGlobalGroupsR :: Handler Value
getGlobalGroupsR = do
  user_id <- requireAuthId
  (toJSON . globalGroupsToResponses) <$> getGlobalGroupsM user_id



postGlobalGroupR0 :: Handler Value
postGlobalGroupR0 = do

  user_id <- requireAuthId

  global_group_request <- requireJsonBody :: Handler GlobalGroupRequest
  (toJSON . globalGroupToResponse) <$> insertGlobalGroupM user_id global_group_request



getGlobalGroupR :: GlobalGroupId -> Handler Value
getGlobalGroupR global_group_id = do
  user_id <- requireAuthId
  (toJSON . globalGroupToResponse) <$> getGlobalGroupM user_id global_group_id



getGlobalGroupH :: Text -> Handler Value
getGlobalGroupH group_name = do
  user_id <- requireAuthId
  (toJSON . globalGroupToResponse) <$> getGlobalGroupMH user_id group_name



putGlobalGroupR :: GlobalGroupId -> Handler Value
putGlobalGroupR global_group_id = do
  user_id <- requireAuthId
  global_group_request <- requireJsonBody
  (toJSON . globalGroupToResponse) <$> updateGlobalGroupM user_id global_group_id global_group_request



deleteGlobalGroupR :: GlobalGroupId -> Handler Value
deleteGlobalGroupR global_group_id = do
  user_id <- requireAuthId
  void $ deleteGlobalGroupM user_id global_group_id
  sendResponseStatus status200 ("DELETED" :: Text)



getCountGlobalGroupsR :: Handler Value
getCountGlobalGroupsR = do
  user_id <- requireAuthId
  toJSON <$> countGlobalGroupsM user_id



getGlobalGroupStatsR :: Handler Value
getGlobalGroupStatsR = do
  user_id <- requireAuthId
  toJSON <$> getGlobalGroupStatsM user_id



getGlobalGroupStatR :: GlobalGroupId -> Handler Value
getGlobalGroupStatR global_group_id = do
  user_id <- requireAuthId
  toJSON <$> getGlobalGroupStatM user_id global_group_id



getGlobalGroupPacksR :: Handler Value
getGlobalGroupPacksR = do
  user_id <- requireAuthId
  toJSON <$> getGlobalGroupPacksM user_id



getGlobalGroupPackR :: GlobalGroupId -> Handler Value
getGlobalGroupPackR global_group_id = do
  user_id <- requireAuthId
  toJSON <$> getGlobalGroupPackM user_id global_group_id



getGlobalGroupPackH :: Text -> Handler Value
getGlobalGroupPackH global_group_name = do
  user_id <- requireAuthId
  toJSON <$> getGlobalGroupPackMH user_id global_group_name



-- model/function



globalGroupRequestToGlobalGroup :: UserId -> GlobalGroupRequest -> GlobalGroup
globalGroupRequestToGlobalGroup user_id GlobalGroupRequest{..} = GlobalGroup {
  globalGroupUserId      = user_id,
  globalGroupName        = toPrettyUrl globalGroupRequestDisplayName,
  globalGroupDisplayName = globalGroupRequestDisplayName,
  globalGroupDescription = globalGroupRequestDescription,
  globalGroupMembership  = globalGroupRequestMembership,
  globalGroupIcon        = globalGroupRequestIcon,
  globalGroupTags        = globalGroupRequestTags,
  globalGroupVisibility  = globalGroupRequestVisibility,
  globalGroupActive      = True,
  globalGroupGuard       = globalGroupRequestGuard,
  globalGroupCreatedAt   = Nothing,
  globalGroupModifiedBy  = Nothing,
  globalGroupModifiedAt  = Nothing,
  globalGroupActivityAt  = Nothing
}



globalGroupToResponse :: Entity GlobalGroup -> GlobalGroupResponse
globalGroupToResponse (Entity global_groupid GlobalGroup{..}) = GlobalGroupResponse {
  globalGroupResponseId          = keyToInt64 global_groupid,
  globalGroupResponseUserId      = keyToInt64 globalGroupUserId,
  globalGroupResponseName        = globalGroupName,
  globalGroupResponseDisplayName = globalGroupDisplayName,
  globalGroupResponseDescription = globalGroupDescription,
  globalGroupResponseMembership  = globalGroupMembership,
  globalGroupResponseIcon        = globalGroupIcon,
  globalGroupResponseTags        = globalGroupTags,
  globalGroupResponseVisibility  = globalGroupVisibility,
  globalGroupResponseActive      = globalGroupActive,
  globalGroupResponseGuard       = globalGroupGuard,
  globalGroupResponseCreatedAt   = globalGroupCreatedAt,
  globalGroupResponseModifiedBy  = fmap keyToInt64 globalGroupModifiedBy,
  globalGroupResponseModifiedAt  = globalGroupModifiedAt,
  globalGroupResponseActivityAt  = globalGroupActivityAt
}



globalGroupsToResponses :: [Entity GlobalGroup] -> GlobalGroupResponses
globalGroupsToResponses globalGroups = GlobalGroupResponses {
  globalGroupResponses = map globalGroupToResponse globalGroups
}



-- model/internal

getGlobalGroupsM :: UserId -> Handler [Entity GlobalGroup]
getGlobalGroupsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spUserId of
    Just lookup_user_id -> getGlobalGroups_ByUserIdM user_id lookup_user_id sp
    _                   -> notFound



getGlobalGroups_ByUserIdM :: UserId -> UserId -> StandardParams -> Handler [Entity GlobalGroup]
getGlobalGroups_ByUserIdM _ lookup_user_id sp = do
  selectListDb sp [GlobalGroupUserId ==. lookup_user_id] [] GlobalGroupId



getGlobalGroups_ByEverythingM :: UserId -> StandardParams -> Handler [Entity GlobalGroup]
getGlobalGroups_ByEverythingM _ sp = do
  selectListDb sp [] [] GlobalGroupId



getGlobalGroupM :: UserId -> GlobalGroupId -> Handler (Entity GlobalGroup)
getGlobalGroupM _ global_groupid = do
  notFoundMaybe =<< selectFirstDb [ GlobalGroupId ==. global_groupid ] []



getGlobalGroupMH :: UserId -> Text -> Handler (Entity GlobalGroup)
getGlobalGroupMH user_id global_group_name = do

  sp@StandardParams{..} <- lookupStandardParams

  case spUserId of

    Just lookup_user_id -> getGlobalGroup_ByUserIdMH user_id global_group_name user_id sp
    _                   -> getGlobalGroupMH' user_id global_group_name sp



getGlobalGroup_ByUserIdMH :: UserId -> Text -> UserId -> StandardParams -> Handler (Entity GlobalGroup)
getGlobalGroup_ByUserIdMH user_id global_group_name lookup_user_id _ = notFound



getGlobalGroupMH' :: UserId -> Text -> StandardParams -> Handler (Entity GlobalGroup)
getGlobalGroupMH' user_id global_group_name _ = notFound



insertGlobalGroupM :: UserId -> GlobalGroupRequest -> Handler (Entity GlobalGroup)
insertGlobalGroupM user_id global_group_request = do

--  sp <- lookupStandardParams

  ts <- timestampH'

  let
    globalGroup = (globalGroupRequestToGlobalGroup user_id global_group_request) { globalGroupCreatedAt = Just ts }

  insertEntityDb globalGroup



updateGlobalGroupM :: UserId -> GlobalGroupId -> GlobalGroupRequest -> Handler (Entity GlobalGroup)
updateGlobalGroupM user_id global_groupid global_group_request = do

  ts <- timestampH'

  let
    GlobalGroup{..} = (globalGroupRequestToGlobalGroup user_id global_group_request) { globalGroupModifiedAt = Just ts }

  updateWhereDb
    [ GlobalGroupUserId ==. user_id, GlobalGroupId ==. global_groupid ]
    [ GlobalGroupModifiedAt  =. globalGroupModifiedAt
    , GlobalGroupName        =. globalGroupName
    , GlobalGroupDisplayName =. globalGroupDisplayName
    , GlobalGroupDescription =. globalGroupDescription
    , GlobalGroupMembership  =. globalGroupMembership
    , GlobalGroupIcon        =. globalGroupIcon
    , GlobalGroupTags        =. globalGroupTags
    , GlobalGroupVisibility  =. globalGroupVisibility
    , GlobalGroupGuard      +=. globalGroupGuard
    ]

  notFoundMaybe =<< selectFirstDb [ GlobalGroupUserId ==. user_id, GlobalGroupId ==. global_groupid ] []



deleteGlobalGroupM :: UserId -> GlobalGroupId -> Handler ()
deleteGlobalGroupM user_id global_groupid = do
  deleteWhereDb [ GlobalGroupUserId ==. user_id, GlobalGroupId ==. global_groupid ]



countGlobalGroupsM :: UserId -> Handler CountResponses
countGlobalGroupsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spUserId of

    Nothing             -> do
      notFound
--      n <- countDb [ GlobalGroupId /=. 0 ]
--      return $ CountResponses [CountResponse (fromIntegral 0) (fromIntegral n)]

    Just lookup_user_id -> do
      n <- countDb [ GlobalGroupUserId ==. lookup_user_id ]
      return $ CountResponses [CountResponse (keyToInt64 lookup_user_id) (fromIntegral n)]



getGlobalGroupStatsM :: UserId -> Handler GlobalGroupStatResponses
getGlobalGroupStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spBoardId of

    Just _  -> notFound
    Nothing -> notFound



getGlobalGroupStatM :: UserId -> GlobalGroupId -> Handler GlobalGroupStatResponse
getGlobalGroupStatM _ global_group_id = do

  return $ GlobalGroupStatResponse {
    globalGroupStatResponseGroups = 0 -- TODO FIXME
  }



-- model/pack


getGlobalGroupPacksM :: UserId -> Handler GlobalGroupPackResponses
getGlobalGroupPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spUserId of

    Just lookup_user_id -> getGlobalGroupPacks_ByUserIdM user_id lookup_user_id sp
    _                   -> getGlobalGroupPacks_ByEverythingM user_id sp



getGlobalGroupPackM :: UserId -> GlobalGroupId -> Handler GlobalGroupPackResponse
getGlobalGroupPackM user_id global_group_id = do

  globalGroup         <- getGlobalGroupM user_id global_group_id
  getGlobalGroupPack_ByGlobalGroupM user_id globalGroup



getGlobalGroupPackMH :: UserId -> Text -> Handler GlobalGroupPackResponse
getGlobalGroupPackMH user_id global_group_name = do

  globalGroup         <- getGlobalGroupMH user_id global_group_name
  getGlobalGroupPack_ByGlobalGroupM user_id globalGroup



getGlobalGroupPacks_ByEverythingM :: UserId -> StandardParams -> Handler GlobalGroupPackResponses
getGlobalGroupPacks_ByEverythingM user_id sp = do
  globalGroups       <- getGlobalGroups_ByEverythingM user_id sp
  globalGroups_packs <- mapM (\globalGroup -> getGlobalGroupPack_ByGlobalGroupM user_id globalGroup) globalGroups
  return $ GlobalGroupPackResponses {
    globalGroupPackResponses = globalGroups_packs
  }



getGlobalGroupPacks_ByUserIdM :: UserId -> UserId -> StandardParams -> Handler GlobalGroupPackResponses
getGlobalGroupPacks_ByUserIdM user_id lookup_user_id sp = do

  globalGroups       <- getGlobalGroups_ByUserIdM user_id lookup_user_id sp
  globalGroups_packs <- mapM (\globalGroup -> getGlobalGroupPack_ByGlobalGroupM user_id globalGroup) globalGroups
  return $ GlobalGroupPackResponses {
    globalGroupPackResponses = globalGroups_packs
  }



getGlobalGroupPack_ByGlobalGroupM :: UserId -> Entity GlobalGroup -> Handler GlobalGroupPackResponse
getGlobalGroupPack_ByGlobalGroupM user_id globalGroup = do

  -- let sp = defaultStandardParams {
  --     spSortOrder = Just SortOrderBy_Dsc,
  --     spOrder     = Just OrderBy_ActivityAt,
  --     spLimit     = Just 1
  --   }

  global_group_stats   <- getGlobalGroupStatM user_id (entityKey globalGroup)

  return $ GlobalGroupPackResponse {
    globalGroupPackResponseGlobalGroup   = globalGroupToResponse globalGroup,
    globalGroupPackResponseGlobalGroupId = global_group_id,
    globalGroupPackResponseStat    = global_group_stats
  }
  where
  global_group_id = entityKeyToInt64 globalGroup
