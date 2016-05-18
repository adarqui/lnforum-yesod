{-# LANGUAGE RecordWildCards #-}

module Model.Forum.Internal (
  getForumsM,
  getForumsBy_OrganizationIdM,
  getForumsBy_OrganizationId_KeysM,
  getForumsBy_OrganizationNameM,
  getForumsBy_UserIdM,
  getForumsBy_UserNickM,
  getForumsBy_EverythingM,

  getForumM,
  getForumMH,
  getForumBy_OrganizationName_ForumNameM,

  insertForumM,
  updateForumM,
  deleteForumM,

  countForumsM,

  getForumStatsM,
  getForumStatM,
) where



import           Model.Prelude
import           Model.Forum.Function
import           Model.Organization.Internal
import           Model.User.Internal2



getForumsM :: UserId -> Handler [Entity Forum]
getForumsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spOrganizationId, spOrganizationName, spUserId, spUserNick) of

    (Just org_id, _, _, _)         -> getForumsBy_OrganizationIdM user_id org_id sp

    (_, Just org_name, _, _)       -> getForumsBy_OrganizationNameM user_id org_name sp

    (_, _, Just lookup_user_id, _) -> getForumsBy_UserIdM user_id lookup_user_id sp

    (_, _, _, Just user_nick)      -> getForumsBy_UserNickM user_id user_nick sp

    (_, _, _, _)                   -> getForumsBy_EverythingM user_id sp



getForumsBy_OrganizationIdM :: UserId -> OrganizationId -> StandardParams -> Handler [Entity Forum]
getForumsBy_OrganizationIdM _ org_id sp = do

  selectListDb sp [ForumOrgId ==. org_id] [] ForumId



getForumsBy_OrganizationId_KeysM :: UserId -> OrganizationId -> StandardParams -> Handler [Key Forum]
getForumsBy_OrganizationId_KeysM _ org_id sp = do

  selectKeysListDb sp [ForumOrgId ==. org_id] [] ForumId




getForumsBy_OrganizationNameM :: UserId -> Text -> StandardParams -> Handler [Entity Forum]
getForumsBy_OrganizationNameM user_id org_name sp = do

  (Entity org_id _) <- getOrganizationMH user_id org_name
  getForumsBy_OrganizationIdM user_id org_id sp



getForumsBy_UserIdM :: UserId -> UserId -> StandardParams -> Handler [Entity Forum]
getForumsBy_UserIdM _ lookup_user_id sp = do

  selectListDb sp [ForumUserId ==. lookup_user_id] [] ForumId



getForumsBy_UserNickM :: UserId -> Text -> StandardParams -> Handler [Entity Forum]
getForumsBy_UserNickM user_id lookup_user_nick sp = do

  (Entity lookup_user_id _) <- getUserMH user_id lookup_user_nick
  getForumsBy_UserIdM user_id lookup_user_id sp



getForumsBy_EverythingM :: UserId -> StandardParams -> Handler [Entity Forum]
getForumsBy_EverythingM _ sp = do

  selectListDb sp [] [] ForumId



getForumM :: UserId -> ForumId -> Handler (Entity Forum)
getForumM _ forum_id = do
  notFoundMaybe =<< selectFirstDb [ ForumId ==. forum_id ] []



getForumMH :: UserId -> Text -> Handler (Entity Forum)
getForumMH user_id forum_name = do

  sp@StandardParams{..} <- lookupStandardParams

  case spOrganizationName of

    Just org_name -> getForumBy_OrganizationName_ForumNameM user_id org_name forum_name sp
    Nothing       -> notFound



getForumBy_OrganizationName_ForumNameM :: UserId -> Text -> Text -> StandardParams -> Handler (Entity Forum)
getForumBy_OrganizationName_ForumNameM user_id org_name forum_name _ = do

  (Entity org_id _) <- getOrganizationBy_OrganizationNameM user_id org_name
  notFoundMaybe =<< selectFirstDb [ForumOrgId ==. org_id, ForumName ==. forum_name] []




insertForumM :: UserId -> OrganizationId -> ForumRequest -> Handler (Entity Forum)
insertForumM user_id org_id forum_request = do

  ts <- timestampH'

  let
    forum = (forumRequestToForum user_id org_id forum_request) { forumCreatedAt = Just ts }

  insertEntityDb forum



updateForumM :: UserId -> ForumId -> ForumRequest -> Handler (Entity Forum)
updateForumM user_id forum_id forum_request = do

  ts <- timestampH'

  let
    Forum{..} = (forumRequestToForum user_id dummyId forum_request) { forumModifiedAt = Just ts }

  updateWhereDb
    [ ForumUserId ==. user_id, ForumId ==. forum_id ]
    [ ForumModifiedAt =. forumModifiedAt
    , ForumName =. forumName
    , ForumDescription =. forumDescription
    ]

  notFoundMaybe =<< selectFirstDb [ ForumUserId ==. user_id, ForumId ==. forum_id ] []



deleteForumM :: UserId -> ForumId -> Handler ()
deleteForumM user_id forum_id = do
  deleteWhereDb [ ForumUserId ==. user_id, ForumId ==. forum_id ]



countForumsM :: UserId -> Handler CountResponses
countForumsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spOrganizationId of

    Nothing -> notFound

    Just org_id -> do
      n <- countDb [ ForumOrgId ==. org_id ]
      return $ CountResponses [CountResponse (keyToInt64 org_id) (fromIntegral n)]



getForumStatsM :: UserId -> Handler ForumStatResponses
getForumStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spBoardId of

    Just _  -> notFound
    Nothing -> notFound




getForumStatM :: UserId -> ForumId -> Handler ForumStatResponse
getForumStatM _ forum_id = do

  num_forum_boards <- countDb [ BoardForumId ==. forum_id ]

  return $ ForumStatResponse {
    forumStatResponseForumId     = keyToInt64 forum_id,
    forumStatResponseBoards      = fromIntegral $ num_forum_boards,
    forumStatResponseThreads     = 0, -- TODO FIXME
    forumStatResponseThreadPosts = 0, -- TODO FIXME
    forumStatResponseViews       = 0
  }
