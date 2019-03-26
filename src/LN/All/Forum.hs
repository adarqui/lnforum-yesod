{-# LANGUAGE RecordWildCards #-}

module LN.All.Forum (
  -- Handler
  getForumR,
  putForumR,
  deleteForumR,
  getForumStatR,

  -- Model/Function
  forumRequestToForum,
  forumToResponse,
  forumsToResponses,

  -- Model/Internal
  updateForumM,
  deleteForumM,
  getForumStatM,
) where



import           LN.All.Prelude
import           LN.All.Internal



--
-- Handler
--

getForumR :: ForumId -> Handler Value
getForumR forum_id = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON forumToResponse $ getForumM user_id forum_id



putForumR :: ForumId -> Handler Value
putForumR forum_id = run $ do
  user_id       <- _requireAuthId
  forum_request <- requireJsonBody
  errorOrJSON forumToResponse $ updateForumM user_id forum_id forum_request



deleteForumR :: ForumId -> Handler Value
deleteForumR forum_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteForumM user_id forum_id



getForumStatR :: ForumId -> Handler Value
getForumStatR forum_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getForumStatM user_id forum_id








--
-- Model/Function
--

forumRequestToForum :: UserId -> ForumRequest -> Forum
forumRequestToForum user_id ForumRequest{..} = Forum {
  forumUserId               = user_id,
  forumName                 = toSafeUrl forumRequestDisplayName,
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

-- getForumM :: Maybe StandardParams -> UserId -> ForumId -> HandlerErrorEff (Entity Forum)
-- getForumM m_sp user_id forum_id = do

--   selectFirstDbE [ForumId ==. forum_id, ForumActive ==. True] []



updateForumM :: UserId -> ForumId -> ForumRequest -> HandlerErrorEff (Entity Forum)
updateForumM user_id forum_id forum_request = do

  runEitherT $ do

    mustT $ mustBe_OwnerOf_ForumIdM user_id forum_id
    sanitized_forum_request <- mustT $ isValidAppM $ validateForumRequest forum_request
    ts                      <- lift timestampH'

    let
      Forum{..} = (forumRequestToForum user_id sanitized_forum_request) { forumModifiedAt = Just ts }

    mustT $ updateWhereDbE
      [ ForumId ==. forum_id ]
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

    mustT $ selectFirstDbE [ForumUserId ==. user_id, ForumId ==. forum_id, ForumActive ==. True] []



deleteForumM :: UserId -> ForumId -> HandlerErrorEff ()
deleteForumM user_id forum_id = do
  runEitherT $ do
    mustT $ mustBe_OwnerOf_ForumIdM user_id forum_id
    mustT $ deleteWhereDbE [ForumId ==. forum_id, ForumActive ==. True]



getForumStatM :: UserId -> ForumId -> HandlerErrorEff ForumStatResponse
getForumStatM _ forum_id = do

  num_forum_boards  <- countDb [{-BoardForumId ==. forum_id,-} BoardActive ==. True]
  num_forum_threads <- countDb [{-ThreadForumId ==. forum_id,-} ThreadActive ==. True]
  num_forum_posts   <- countDb [{-ThreadPostForumId ==. forum_id,-} ThreadPostActive ==. True]

  rightA $ ForumStatResponse {
    forumStatResponseBoards      = fromIntegral num_forum_boards,
    forumStatResponseThreads     = fromIntegral num_forum_threads,
    forumStatResponseThreadPosts = fromIntegral num_forum_posts,
    forumStatResponseViews       = 0
  }
