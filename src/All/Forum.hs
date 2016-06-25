{-# LANGUAGE RecordWildCards #-}

module All.Forum (
  -- Handler
  getForumsR,
  postForumR0,
  getForumR,
  getForumH,
  putForumR,
  deleteForumR,
  getForumCountsR,
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
  getForums_ByUserIdM,
  getForumM,
  getForumMH,
  getForum_ByOrganizationIdMH,
  getWithForumM,
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

getForumsR :: Handler Value
getForumsR = run $ do
  user_id <- _requireAuthId
  (toJSON . forumsToResponses) <$> getForumsM user_id



postForumR0 :: Handler Value
postForumR0 = run $ do
  user_id <- _requireAuthId
  forum_request <- requireJsonBody :: HandlerEff ForumRequest
  (toJSON . forumToResponse) <$> insertForumM user_id forum_request



getForumR :: ForumId -> Handler Value
getForumR forum_id = run $ do
  user_id <- _requireAuthId
  (toJSON . forumToResponse) <$> getForumM user_id forum_id



getForumH :: Text -> Handler Value
getForumH forum_name = run $ do -- getForumR' getForumMH forum_name
  user_id <- _requireAuthId
  (toJSON . forumToResponse) <$> getForumMH user_id forum_name



putForumR :: ForumId -> Handler Value
putForumR forum_id = run $ do
  user_id <- _requireAuthId
  forum_request <- requireJsonBody
  (toJSON . forumToResponse) <$> updateForumM user_id forum_id forum_request



deleteForumR :: ForumId -> Handler Value
deleteForumR forum_id = run $ do
  user_id <- _requireAuthId
  void $ deleteForumM user_id forum_id
  pure $ toJSON ()



getForumCountsR :: Handler Value
getForumCountsR = run $ do
  user_id <- _requireAuthId
  toJSON <$> countForumsM user_id



getForumStatsR :: Handler Value
getForumStatsR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getForumStatsM user_id



getForumStatR :: ForumId -> Handler Value
getForumStatR forum_id = run $ do
  user_id <- _requireAuthId
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
  forumRecentThreadsLimit   = forumRequestRecentThreadsLimit,
  forumRecentPostsLimit     = forumRequestRecentPostsLimit,
  forumMotwLimit            = forumRequestMotwLimit,
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
  forumResponseRecentThreadsLimit   = forumRecentThreadsLimit,
  forumResponseRecentPostsLimit     = forumRecentPostsLimit,
  forumResponseMotwLimit            = forumMotwLimit,
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

  case (spOrganizationId, spUserId) of

    (Just org_id, _)         -> getForums_ByOrganizationIdM user_id org_id sp

    (_, Just lookup_user_id) -> getForums_ByUserIdM user_id lookup_user_id sp

    (_, _)                   -> notFound



getForums_ByOrganizationIdM :: UserId -> OrganizationId -> StandardParams -> HandlerEff [Entity Forum]
getForums_ByOrganizationIdM _ org_id sp = do

  selectListDb sp [ForumOrgId ==. org_id] [] ForumId



getForums_ByOrganizationId_KeysM :: UserId -> OrganizationId -> StandardParams -> HandlerEff [Key Forum]
getForums_ByOrganizationId_KeysM _ org_id sp = do

  selectKeysListDb sp [ForumOrgId ==. org_id] [] ForumId




getForums_ByUserIdM :: UserId -> UserId -> StandardParams -> HandlerEff [Entity Forum]
getForums_ByUserIdM _ lookup_user_id sp = do

  selectListDb sp [ForumUserId ==. lookup_user_id] [] ForumId



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



getWithForumM :: Bool -> UserId -> ForumId -> HandlerEff (Maybe (Entity Forum))
getWithForumM False _ _ = pure Nothing
getWithForumM True user_id forum_id  = do
  selectFirstDb [ForumId ==. forum_id] []



insertForumM :: UserId -> ForumRequest -> HandlerEff (Entity Forum)
insertForumM user_id forum_request = do

  sp@StandardParams{..} <- lookupStandardParams

  case spOrganizationId of
    Just org_id -> insertForum_ByOrganizationIdM user_id org_id forum_request
    _                    -> permissionDenied "Must supply an org_id"



insertForum_ByOrganizationIdM :: UserId -> OrganizationId -> ForumRequest -> HandlerEff (Entity Forum)
insertForum_ByOrganizationIdM user_id org_id forum_request = do

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
    , ForumRecentThreadsLimit   =. forumRecentThreadsLimit
    , ForumRecentPostsLimit     =. forumRecentPostsLimit
    , ForumMotwLimit            =. forumMotwLimit
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
