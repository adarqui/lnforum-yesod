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
  errorOrJSON forumsToResponses $ getForumsM user_id



postForumR0 :: Handler Value
postForumR0 = run $ do
  user_id <- _requireAuthId
  forum_request <- requireJsonBody :: HandlerEff ForumRequest
  errorOrJSON forumToResponse $ insertForumM user_id forum_request



getForumR :: ForumId -> Handler Value
getForumR forum_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON forumToResponse $ getForumM user_id forum_id



getForumH :: Text -> Handler Value
getForumH forum_name = run $ do -- getForumR' getForumMH forum_name
  user_id <- _requireAuthId
  errorOrJSON forumToResponse $ getForumMH user_id forum_name



putForumR :: ForumId -> Handler Value
putForumR forum_id = run $ do
  user_id <- _requireAuthId
  forum_request <- requireJsonBody
  errorOrJSON forumToResponse $ updateForumM user_id forum_id forum_request



deleteForumR :: ForumId -> Handler Value
deleteForumR forum_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteForumM user_id forum_id



getForumCountsR :: Handler Value
getForumCountsR = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ countForumsM user_id



getForumStatsR :: Handler Value
getForumStatsR = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getForumStatsM user_id



getForumStatR :: ForumId -> Handler Value
getForumStatR forum_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getForumStatM user_id forum_id








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

getForumsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Forum]
getForumsM m_sp user_id = do

  case (lookupSpMay m_sp spOrganizationId, lookupSpMay m_sp spUserId) of

    (Just org_id, _)         -> getForums_ByOrganizationIdM m_sp user_id org_id

    (_, Just lookup_user_id) -> getForums_ByUserIdM m_sp user_id lookup_user_id

    _                        -> left Error_NotImplemented



getForums_ByOrganizationIdM :: Maybe StandardParams -> UserId -> OrganizationId -> HandlerErrorEff [Entity Forum]
getForums_ByOrganizationIdM m_sp _ org_id = do

  selectListDbMay m_sp [ForumOrgId ==. org_id, ForumActive ==. True] [] ForumId



getForums_ByOrganizationId_KeysM :: Maybe StandardParams -> UserId -> OrganizationId -> HandlerErrorEff [Key Forum]
getForums_ByOrganizationId_KeysM m_sp _ org_id = do

  selectKeysListDbMay m_sp [ForumOrgId ==. org_id, ForumActive ==. True] [] ForumId




getForums_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity Forum]
getForums_ByUserIdM m_sp _ lookup_user_id = do

  selectListDbMay m_sp [ForumUserId ==. lookup_user_id, ForumActive ==. True] [] ForumId



getForum_ByOrganizationIdMH :: Maybe StandardParams -> UserId -> Text -> OrganizationId -> HandlerErrorEff (Entity Forum)
getForum_ByOrganizationIdMH m_sp user_id forum_name org_id = do

  selectFirstDbMay m_sp [ForumOrgId ==. org_id, ForumName ==. forum_name, ForumActive ==. True] []



getForumM :: UserId -> ForumId -> HandlerErrorEff (Entity Forum)
getForumM _ forum_id = do

  selectFirstDbMay [ForumId ==. forum_id, ForumActive ==. True] []



getForumMH :: Maybe StandardParams -> UserId -> Text -> HandlerErrorEff (Entity Forum)
getForumMH m_sp user_id forum_name = do

  case (lookupSpMay m_sp spOrganizationId) of

    Just org_id -> getForum_ByOrganizationIdMH m_sp user_id forum_name org_id
    _           -> left Error_NotImplemented



getWithForumM :: Bool -> UserId -> ForumId -> HandlerErrorEff (Maybe (Entity Forum))
getWithForumM False _ _              = right Nothing
getWithForumM True user_id forum_id  = fmap Just <$> getForumM user_id forum_id



insertForumM :: Maybe StandardParams -> UserId -> ForumRequest -> HandlerErrorEff (Entity Forum)
insertForumM m_sp user_id forum_request = do

  case (lookupSpMay m_sp spOrganizationId) of
    Just org_id -> insertForum_ByOrganizationIdM m_sp user_id org_id forum_request
    _           -> left Error_NotImplemented



insertForum_ByOrganizationIdM :: UserId -> OrganizationId -> ForumRequest -> HandlerErrorEff (Entity Forum)
insertForum_ByOrganizationIdM user_id org_id forum_request = do

  ts <- timestampH'

  let
    forum = (forumRequestToForum user_id org_id forum_request) { forumCreatedAt = Just ts }

  insertEntityDb forum



updateForumM :: UserId -> ForumId -> ForumRequest -> HandlerErrorEff (Entity Forum)
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

  selectFirstDbEither [ForumUserId ==. user_id, ForumId ==. forum_id, ForumActive ==. True] []



deleteForumM :: UserId -> ForumId -> HandlerErrorEff ()
deleteForumM user_id forum_id = do
  deleteWhereDbEither [ForumUserId ==. user_id, ForumId ==. forum_id, ForumActive ==. True]



countForumsM :: Maybe StandardParams -> UserId -> HandlerEff CountResponses
countForumsM m_sp _ = do

  case (lookupSpMay m_sp spOrganizationId) of

    Just org_id -> do
      n <- countDb [ForumOrgId ==. org_id, ForumActive ==. True]
      right $ CountResponses [CountResponse (keyToInt64 org_id) (fromIntegral n)]

    _ -> left Error_NotImplemented



getForumStatsM :: Maybe StandardParams -> UserId -> HandlerErrorEff ForumStatResponses
getForumStatsM m_sp _ = left Error_NotImplemented



getForumStatM :: UserId -> ForumId -> HandlerErrorEff ForumStatResponse
getForumStatM _ forum_id = do

  num_forum_boards  <- countDb [BoardForumId ==. forum_id, BoardActive ==. True]
  num_forum_threads <- countDb [ThreadForumId ==. forum_id, ThreadActive ==. True]
  num_forum_posts   <- countDb [ThreadPostForumId ==. forum_id, ThreadPostActive ==. True]

  return $ ForumStatResponse {
    forumStatResponseForumId     = keyToInt64 forum_id,
    forumStatResponseBoards      = fromIntegral num_forum_boards,
    forumStatResponseThreads     = fromIntegral num_forum_threads,
    forumStatResponseThreadPosts = fromIntegral num_forum_posts,
    forumStatResponseViews       = 0
  }
