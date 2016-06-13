module All.GlobalGroup (
  -- handler
  getGlobalGroupsR,
  postGlobalGroupsR,
  getGlobalGroupR,
  putGlobalGroupR,
  deleteGlobalGroupR,


  -- model/functions
  globalGroupRequestToGlobalGroup,
  globalGroupToResponse,
  globalGroupsToResponses,


  -- model/internal
  getGlobalGroupsM,
  getGlobalGroupsBy_UserIdM,
  getGlobalGroupsBy_EverythingM,

  getGlobalGroupM,
  insertGlobalGroupM,
  updateGlobalGroupM,
  deleteGlobalGroupM,

  getGlobalGroupCountM

  -- pack

) where



import           Handler.Prelude
import           Model.Prelude



getGlobalGroupsR :: Handler Value
getGlobalGroupsR = do
  user_id <- requireAuthId
  (toJSON . globalGroupsToResponses) <$> getGlobalGroupsM user_id



postGlobalGroupsR :: Handler Value
postGlobalGroupsR = do

  user_id <- requireAuthId

  global_group_request <- requireJsonBody :: Handler GlobalGroupRequest
  (toJSON . globalGroupToResponse) <$> insertGlobalGroupM user_id global_group_request



getGlobalGroupR :: GlobalGroupId -> Handler Value
getGlobalGroupR global_group_id = do
  user_id <- requireAuthId
  (toJSON . globalGroupToResponse) <$> getGlobalGroupM user_id global_group_id



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
    Just lookup_user_id -> getGlobalGroupsBy_UserIdM user_id lookup_user_id sp
    _                   -> notFound



getGlobalGroupsBy_UserIdM :: UserId -> UserId -> StandardParams -> Handler [Entity GlobalGroup]
getGlobalGroupsBy_UserIdM _ lookup_user_id sp = do
  selectListDb sp [GlobalGroupUserId ==. lookup_user_id] [] GlobalGroupId



getGlobalGroupsBy_EverythingM :: UserId -> StandardParams -> Handler [Entity GlobalGroup]
getGlobalGroupsBy_EverythingM _ sp = do
  selectListDb sp [] [] GlobalGroupId



getGlobalGroupM :: UserId -> GlobalGroupId -> Handler (Entity GlobalGroup)
getGlobalGroupM _ global_groupid = do
  notFoundMaybe =<< selectFirstDb [ GlobalGroupId ==. global_groupid ] []



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



getGlobalGroupCountM :: Handler Int
getGlobalGroupCountM = do
  runDB $ count [ GlobalGroupName !=. "" ]
