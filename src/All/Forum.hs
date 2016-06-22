{-# LANGUAGE RecordWildCards #-}

module All.Forum (
  -- Handler
  getForumsR,
  postForumR0,
  getForumR,
  getForumH,
  putForumR,
  deleteForumR,
  getCountForumsR,
  getForumStatsR,
  getForumStatR,

  -- Model/Function
  forumRequestToForum,
  forumToResponse,
  forumsToResponses,

  -- Model/Internal
  getForumsM,
  getForums_ByOrganizationIdM,
  getForums_ByOrganizationId_KeysM,
  getForums_ByOrganizationNameM,
  getForums_ByUserIdM,
  getForums_ByUserNickM,
  getForums_ByEverythingM,
  getForumM,
  getForumMH,
  getForum_ByOrganizationIdMH,
  insertForumM,
  updateForumM,
  deleteForumM,
  countForumsM,
  getForumStatsM,
  getForumStatM,
) where



import           All.Organization
import           All.Prelude
import           All.User



--
-- Handler
--

getForumsR :: HandlerEff Value
getForumsR = do
  user_id <- requireAuthId
  (toJSON . forumsToResponses) <$> getForumsM user_id



postForumR0 :: HandlerEff Value
postForumR0 = do

  user_id <- requireAuthId

  sp <- lookupStandardParams

  case (spOrganizationId sp) of
    Nothing -> permissionDenied "Must supply org_id"

    (Just org_id) -> do
      forum_request <- requireJsonBody :: HandlerEff ForumRequest
      (toJSON . forumToResponse) <$> insertForumM user_id org_id forum_request



getForumR :: ForumId -> HandlerEff Value
getForumR forum_id = do
  user_id <- requireAuthId
  (toJSON . forumToResponse) <$> getForumM user_id forum_id



getForumH :: Text -> HandlerEff Value
getForumH forum_name = do -- getForumR' getForumMH forum_name
  user_id <- requireAuthId
  (toJSON . forumToResponse) <$> getForumMH user_id forum_name



putForumR :: ForumId -> HandlerEff Value
putForumR forum_id = do
  user_id <- requireAuthId
  forum_request <- requireJsonBody
  (toJSON . forumToResponse) <$> updateForumM user_id forum_id forum_request



deleteForumR :: ForumId -> HandlerEff Value
deleteForumR forum_id = do
  user_id <- requireAuthId
  void $ deleteForumM user_id forum_id
  pure $ toJSON ()



getCountForumsR :: HandlerEff Value
getCountForumsR = do
  user_id <- requireAuthId
  toJSON <$> countForumsM user_id



getForumStatsR :: HandlerEff Value
getForumStatsR = do
  user_id <- requireAuthId
  toJSON <$> getForumStatsM user_id



getForumStatR :: ForumId -> HandlerEff Value
getForumStatR forum_id = do
  user_id <- requireAuthId
  toJSON <$> getForumStatM user_id forum_id








--
-- Model/Function
--

forumRequestToForum :: UserId -> OrganizationId -> ForumRequest -> Forum
forumRequestToForum user_id org_id ForumRequest{..} = Forum {
  forumUserId               = user_id,
  forumOrgId                = org_id,
  forumName                 = toPrettyUrl forumRequestDisplayName,
  forumDisplayName          = forumRequestDisplayName,
  forumDescription          = forumRequestDescription,
  forumThreadsPerBoard      = forumRequestThreadsPerBoard,
  forumThreadPostsPerThread = forumRequestThreadPostsPerThread,
  forumIcon                 = forumRequestIcon,
  forumTags                 = forumRequestTags,
  forumVisibility           = forumRequestVisibility,
  forumActive               = True,
  forumGuard                = forumRequestGuard,
  forumCreatedAt            = Nothing,
  forumModifiedBy           = Nothing,
  forumModifiedAt           = Nothing,
  forumActivityAt           = Nothing
}



forumToResponse :: Entity Forum -> ForumResponse
forumToResponse (Entity forum_id Forum{..}) = ForumResponse {
  forumResponseUserId               = keyToInt64 forumUserId,
  forumResponseId                   = keyToInt64 forum_id,
  forumResponseOrgId                = keyToInt64 forumOrgId,
  forumResponseName                 = forumName,
  forumResponseDisplayName          = forumDisplayName,
  forumResponseDescription          = forumDescription,
  forumResponseThreadsPerBoard      = forumThreadsPerBoard,
  forumResponseThreadPostsPerThread = forumThreadPostsPerThread,
  forumResponseIcon                 = forumIcon,
  forumResponseTags                 = forumTags,
  forumResponseVisibility           = forumVisibility,
  forumResponseActive               = forumActive,
  forumResponseGuard                = forumGuard,
  forumResponseCreatedAt            = forumCreatedAt,
  forumResponseModifiedBy           = fmap keyToInt64 forumModifiedBy,
  forumResponseModifiedAt           = forumModifiedAt,
  forumResponseActivityAt           = forumActivityAt
}



forumsToResponses :: [Entity Forum] -> ForumResponses
forumsToResponses forums = ForumResponses {
  forumResponses = map forumToResponse forums
}





--
-- Model/Internal
--

getForumsM :: UserId -> HandlerEff [Entity Forum]
getForumsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spOrganizationId, spOrganizationName, spUserId, spUserNick) of

    (Just org_id, _, _, _)         -> getForums_ByOrganizationIdM user_id org_id sp

    (_, Just org_name, _, _)       -> getForums_ByOrganizationNameM user_id org_name sp

    (_, _, Just lookup_user_id, _) -> getForums_ByUserIdM user_id lookup_user_id sp

    (_, _, _, Just user_nick)      -> getForums_ByUserNickM user_id user_nick sp

    (_, _, _, _)                   -> getForums_ByEverythingM user_id sp



getForums_ByOrganizationIdM :: UserId -> OrganizationId -> StandardParams -> HandlerEff [Entity Forum]
getForums_ByOrganizationIdM _ org_id sp = do

  selectListDb sp [ForumOrgId ==. org_id] [] ForumId



getForums_ByOrganizationId_KeysM :: UserId -> OrganizationId -> StandardParams -> HandlerEff [Key Forum]
getForums_ByOrganizationId_KeysM _ org_id sp = do

  selectKeysListDb sp [ForumOrgId ==. org_id] [] ForumId




getForums_ByOrganizationNameM :: UserId -> Text -> StandardParams -> HandlerEff [Entity Forum]
getForums_ByOrganizationNameM user_id org_name sp = do

  (Entity org_id _) <- getOrganizationMH user_id org_name
  getForums_ByOrganizationIdM user_id org_id sp



getForums_ByUserIdM :: UserId -> UserId -> StandardParams -> HandlerEff [Entity Forum]
getForums_ByUserIdM _ lookup_user_id sp = do

  selectListDb sp [ForumUserId ==. lookup_user_id] [] ForumId



getForums_ByUserNickM :: UserId -> Text -> StandardParams -> HandlerEff [Entity Forum]
getForums_ByUserNickM user_id lookup_user_nick sp = do

  (Entity lookup_user_id _) <- getUserMH user_id lookup_user_nick
  getForums_ByUserIdM user_id lookup_user_id sp



getForums_ByEverythingM :: UserId -> StandardParams -> HandlerEff [Entity Forum]
getForums_ByEverythingM _ sp = do

  selectListDb sp [] [] ForumId



getForum_ByOrganizationIdMH :: UserId -> Text -> OrganizationId -> StandardParams -> HandlerEff (Entity Forum)
getForum_ByOrganizationIdMH user_id forum_name org_id sp = do

  notFoundMaybe =<< selectFirstDb [ ForumOrgId ==. org_id, ForumName ==. forum_name ] []



getForumM :: UserId -> ForumId -> HandlerEff (Entity Forum)
getForumM _ forum_id = do
  notFoundMaybe =<< selectFirstDb [ ForumId ==. forum_id ] []



getForumMH :: UserId -> Text -> HandlerEff (Entity Forum)
getForumMH user_id forum_name = do

  sp@StandardParams{..} <- lookupStandardParams

  case spOrganizationId of

    Just org_id -> getForum_ByOrganizationIdMH user_id forum_name org_id sp
    _           -> notFound



-- getForum_ByOrganizationName_ForumNameM :: UserId -> Text -> Text -> StandardParams -> HandlerEff (Entity Forum)
-- getForum_ByOrganizationName_ForumNameM user_id org_name forum_name _ = do

--   (Entity org_id _) <- getOrganization_ByOrganizationNameM user_id org_name
--   notFoundMaybe =<< selectFirstDb [ForumOrgId ==. org_id, ForumName ==. forum_name] []




insertForumM :: UserId -> OrganizationId -> ForumRequest -> HandlerEff (Entity Forum)
insertForumM user_id org_id forum_request = do

  ts <- timestampH'

  let
    forum = (forumRequestToForum user_id org_id forum_request) { forumCreatedAt = Just ts }

  insertEntityDb forum



updateForumM :: UserId -> ForumId -> ForumRequest -> HandlerEff (Entity Forum)
updateForumM user_id forum_id forum_request = do

  ts <- timestampH'

  let
    Forum{..} = (forumRequestToForum user_id dummyId forum_request) { forumModifiedAt = Just ts }

  updateWhereDb
    [ ForumUserId ==. user_id, ForumId ==. forum_id ]
    [ ForumModifiedAt           =. forumModifiedAt
    , ForumActivityAt           =. Just ts
    , ForumName                 =. forumName
    , ForumDisplayName          =. forumDisplayName
    , ForumDescription          =. forumDescription
    , ForumThreadsPerBoard      =. forumThreadsPerBoard
    , ForumThreadPostsPerThread =. forumThreadPostsPerThread
    , ForumIcon                 =. forumIcon
    , ForumTags                 =. forumTags
    , ForumVisibility           =. forumVisibility
    , ForumGuard               +=. 1
    ]

  notFoundMaybe =<< selectFirstDb [ ForumUserId ==. user_id, ForumId ==. forum_id ] []



deleteForumM :: UserId -> ForumId -> HandlerEff ()
deleteForumM user_id forum_id = do
  deleteWhereDb [ ForumUserId ==. user_id, ForumId ==. forum_id ]



countForumsM :: UserId -> HandlerEff CountResponses
countForumsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spOrganizationId of

    Nothing -> notFound

    Just org_id -> do
      n <- countDb [ ForumOrgId ==. org_id ]
      return $ CountResponses [CountResponse (keyToInt64 org_id) (fromIntegral n)]



getForumStatsM :: UserId -> HandlerEff ForumStatResponses
getForumStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spBoardId of

    Just _  -> notFound
    Nothing -> notFound




getForumStatM :: UserId -> ForumId -> HandlerEff ForumStatResponse
getForumStatM _ forum_id = do

  num_forum_boards <- countDb [ BoardForumId ==. forum_id ]

  return $ ForumStatResponse {
    forumStatResponseForumId     = keyToInt64 forum_id,
    forumStatResponseBoards      = fromIntegral $ num_forum_boards,
    forumStatResponseThreads     = 0, -- TODO FIXME
    forumStatResponseThreadPosts = 0, -- TODO FIXME
    forumStatResponseViews       = 0
  }
