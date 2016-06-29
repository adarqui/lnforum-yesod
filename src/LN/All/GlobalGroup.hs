{-# LANGUAGE RecordWildCards #-}

module LN.All.GlobalGroup (
  -- Handler
  getGlobalGroupsR,
  postGlobalGroupR0,
  getGlobalGroupR,
  getGlobalGroupH,
  putGlobalGroupR,
  deleteGlobalGroupR,
  getGlobalGroupsCountR,
  getGlobalGroupStatsR,
  getGlobalGroupStatR,

  -- Model/Functions
  globalGroupRequestToGlobalGroup,
  globalGroupToResponse,
  globalGroupsToResponses,

  -- Model/internal
  getGlobalGroupsM,
  getGlobalGroups_ByUserIdM,
  getGlobalGroups_ByEverythingM,
  getGlobalGroupM,
  getGlobalGroupMH,
  insertGlobalGroupM,
  updateGlobalGroupM,
  deleteGlobalGroupM,
  getGlobalGroupStatsM,
  getGlobalGroupStatM
) where



import           LN.All.Prelude



getGlobalGroupsR :: Handler Value
getGlobalGroupsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON globalGroupsToResponses $ getGlobalGroupsM (pure sp) user_id



postGlobalGroupR0 :: Handler Value
postGlobalGroupR0 = run $ do
  user_id              <- _requireAuthId
  global_group_request <- requireJsonBody
  errorOrJSON globalGroupToResponse $ insertGlobalGroupM user_id global_group_request



getGlobalGroupR :: GlobalGroupId -> Handler Value
getGlobalGroupR global_group_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON globalGroupToResponse $ getGlobalGroupM user_id global_group_id



getGlobalGroupH :: Text -> Handler Value
getGlobalGroupH group_name = run $ do
  user_id <- _requireAuthId
  errorOrJSON globalGroupToResponse $ getGlobalGroupMH user_id group_name



putGlobalGroupR :: GlobalGroupId -> Handler Value
putGlobalGroupR global_group_id = run $ do
  user_id              <- _requireAuthId
  global_group_request <- requireJsonBody
  errorOrJSON globalGroupToResponse $ updateGlobalGroupM user_id global_group_id global_group_request



deleteGlobalGroupR :: GlobalGroupId -> Handler Value
deleteGlobalGroupR global_group_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteGlobalGroupM user_id global_group_id



getGlobalGroupsCountR :: Handler Value
getGlobalGroupsCountR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ countGlobalGroupsM (pure sp) user_id



getGlobalGroupStatsR :: Handler Value
getGlobalGroupStatsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getGlobalGroupStatsM (pure sp) user_id



getGlobalGroupStatR :: GlobalGroupId -> Handler Value
getGlobalGroupStatR global_group_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getGlobalGroupStatM user_id global_group_id







-- Model/Function

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








-- Model/Internal

getGlobalGroupsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity GlobalGroup]
getGlobalGroupsM m_sp user_id = do

  case (lookupSpMay m_sp spUserId) of
    Just lookup_user_id -> getGlobalGroups_ByUserIdM m_sp user_id lookup_user_id
    _                   -> left $ Error_InvalidArguments "user_id"



getGlobalGroups_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity GlobalGroup]
getGlobalGroups_ByUserIdM m_sp _ lookup_user_id = do
  selectListDbE m_sp [GlobalGroupUserId ==. lookup_user_id, GlobalGroupActive ==. True] [] GlobalGroupId



getGlobalGroups_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity GlobalGroup]
getGlobalGroups_ByEverythingM m_sp _ = do
  selectListDbE m_sp [GlobalGroupActive ==. True] [] GlobalGroupId



getGlobalGroupM :: UserId -> GlobalGroupId -> HandlerErrorEff (Entity GlobalGroup)
getGlobalGroupM _ global_groupid = do
  selectFirstDbE [GlobalGroupId ==. global_groupid, GlobalGroupActive ==. True] []



getGlobalGroupMH :: UserId -> Text -> HandlerErrorEff (Entity GlobalGroup)
getGlobalGroupMH user_id global_group_name = do
  selectFirstDbE [GlobalGroupName ==. global_group_name, GlobalGroupActive ==. True] []



insertGlobalGroupM :: UserId -> GlobalGroupRequest -> HandlerErrorEff (Entity GlobalGroup)
insertGlobalGroupM user_id global_group_request = do
  ts <- timestampH'
  let
    global_group = (globalGroupRequestToGlobalGroup user_id global_group_request) { globalGroupCreatedAt = Just ts }
  insertEntityDbE global_group



updateGlobalGroupM :: UserId -> GlobalGroupId -> GlobalGroupRequest -> HandlerErrorEff (Entity GlobalGroup)
updateGlobalGroupM user_id global_groupid global_group_request = do

  ts <- timestampH'

  let
    GlobalGroup{..} = (globalGroupRequestToGlobalGroup user_id global_group_request) { globalGroupModifiedAt = Just ts }

  updateWhereDb
    [ GlobalGroupUserId ==. user_id, GlobalGroupId ==. global_groupid, GlobalGroupActive ==. True]
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

  selectFirstDbE [GlobalGroupUserId ==. user_id, GlobalGroupId ==. global_groupid, GlobalGroupActive ==. True] []



deleteGlobalGroupM :: UserId -> GlobalGroupId -> HandlerErrorEff ()
deleteGlobalGroupM user_id global_groupid = do
  deleteWhereDbE [GlobalGroupUserId ==. user_id, GlobalGroupId ==. global_groupid, GlobalGroupActive ==. True]



countGlobalGroupsM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countGlobalGroupsM m_sp _ = do

  case (lookupSpMay m_sp spUserId) of

    Just lookup_user_id -> do
      n <- countDb [GlobalGroupUserId ==. lookup_user_id, GlobalGroupActive ==. True]
      right $ CountResponses [CountResponse (keyToInt64 lookup_user_id) (fromIntegral n)]

    _                   -> left $ Error_InvalidArguments "user_id"



getGlobalGroupStatsM :: Maybe StandardParams -> UserId -> HandlerErrorEff GlobalGroupStatResponses
getGlobalGroupStatsM _ _ = left $ Error_NotImplemented



getGlobalGroupStatM :: UserId -> GlobalGroupId -> HandlerErrorEff GlobalGroupStatResponse
getGlobalGroupStatM _ global_group_id = do
  right $ GlobalGroupStatResponse {
    globalGroupStatResponseGroups = 0 -- TODO FIXME
  }
