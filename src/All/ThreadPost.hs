{-# LANGUAGE RecordWildCards #-}

module All.ThreadPost (
  -- Handler
  getThreadPostsR,
  postThreadPostR0,
  getThreadPostR,
  putThreadPostR,
  deleteThreadPostR,
  getThreadPostsCountR,
  getThreadPostStatsR,
  getThreadPostStatR,

  -- Model/Function
  threadPostRequestToThreadPost,
  threadPostToResponse,
  threadPostsToResponses,

  -- Model/Internal
  getThreadPostsM,
  getThreadPosts_ByForumIdM,
  getThreadPosts_ByThreadIdM,
  getThreadPosts_ByThreadPostIdM,
  getThreadPostM,
  insertThreadPostM,
  updateThreadPostM,
  deleteThreadPostM,
  countThreadPostsM,
  getThreadPostStatsM,
  getThreadPostStatM
) where



import           All.Prelude
import qualified LN.T.Like                 as L



getThreadPostsR :: Handler Value
getThreadPostsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON threadPostsToResponses $ getThreadPostsM (pure sp) user_id



postThreadPostR0 :: Handler Value
postThreadPostR0 = run $ do
  user_id             <- _requireAuthId
  thread_post_request <- requireJsonBody :: HandlerEff ThreadPostRequest
  sp                  <- lookupStandardParams
  errorOrJSON threadPostToResponse $ insertThreadPostM (pure sp) user_id thread_post_request



getThreadPostR :: ThreadPostId -> Handler Value
getThreadPostR thread_post_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON threadPostToResponse $ getThreadPostM user_id thread_post_id



putThreadPostR :: ThreadPostId -> Handler Value
putThreadPostR thread_post_id = run $ do
  user_id             <- _requireAuthId
  thread_post_request <- requireJsonBody
  errorOrJSON threadPostToResponse $ updateThreadPostM user_id thread_post_id thread_post_request



deleteThreadPostR :: ThreadPostId -> Handler Value
deleteThreadPostR thread_post_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteThreadPostM user_id thread_post_id



getThreadPostsCountR :: Handler Value
getThreadPostsCountR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ countThreadPostsM (pure sp) user_id



getThreadPostStatsR :: Handler Value
getThreadPostStatsR = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getThreadPostStatsM user_id



getThreadPostStatR :: ThreadPostId -> Handler Value
getThreadPostStatR thread_post_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getThreadPostStatM user_id thread_post_id







--
-- Model/Function
--

threadPostRequestToThreadPost :: UserId -> OrganizationId -> ForumId -> BoardId -> ThreadId -> Maybe ThreadPostId -> ThreadPostRequest -> ThreadPost
threadPostRequestToThreadPost user_id org_id forum_id board_id thread_id _ ThreadPostRequest{..} = ThreadPost {
  threadPostUserId      = user_id,
  threadPostOrgId       = org_id,
  threadPostForumId     = forum_id,
  threadPostBoardId     = board_id,
  threadPostThreadId    = thread_id,
  threadPostParentId    = Nothing,
  threadPostTitle       = threadPostRequestTitle,
  threadPostBody        = encodeText threadPostRequestBody,
  threadPostTags        = threadPostRequestTags,
  threadPostPrivateTags = threadPostRequestPrivateTags,
  threadPostActive      = True,
  threadPostGuard       = threadPostRequestGuard,
  threadPostCreatedAt   = Nothing,
  threadPostModifiedBy  = Nothing,
  threadPostModifiedAt  = Nothing,
  threadPostActivityAt  = Nothing
}



threadPostToResponse :: Entity ThreadPost -> ThreadPostResponse
threadPostToResponse (Entity thread_post_id ThreadPost{..}) = ThreadPostResponse {
  threadPostResponseId          = keyToInt64 thread_post_id,
  threadPostResponseUserId      = keyToInt64 threadPostUserId,
  threadPostResponseOrgId       = keyToInt64 threadPostOrgId,
  threadPostResponseForumId     = keyToInt64 threadPostForumId,
  threadPostResponseBoardId     = keyToInt64 threadPostBoardId,
  threadPostResponseThreadId    = keyToInt64 threadPostThreadId,
  threadPostResponseParentId    = fmap keyToInt64 threadPostParentId,
  threadPostResponseTitle       = threadPostTitle,
  threadPostResponseBody        = maybe PostDataEmpty id $ decodeText threadPostBody,
  threadPostResponseTags        = threadPostTags,
  threadPostResponsePrivateTags = threadPostPrivateTags,
  threadPostResponseActive      = threadPostActive,
  threadPostResponseGuard       = threadPostGuard,
  threadPostResponseCreatedAt   = threadPostCreatedAt,
  threadPostResponseModifiedBy  = fmap keyToInt64 threadPostModifiedBy,
  threadPostResponseModifiedAt  = threadPostModifiedAt,
  threadPostResponseActivityAt  = threadPostActivityAt
}



threadPostsToResponses :: [Entity ThreadPost] -> ThreadPostResponses
threadPostsToResponses thread_posts = ThreadPostResponses {
  threadPostResponses = map threadPostToResponse thread_posts
}








--
-- Model/Internal
--

-- orderByToField :: forall typ record. OrderBy -> EntityField record typ
-- TODO FIXME, this type sig might cause problems
orderByToField :: Maybe OrderBy -> EntityField ThreadPost (Maybe UTCTime)
orderByToField Nothing      = ThreadPostCreatedAt
orderByToField (Just order) =
  case order of
    OrderBy_CreatedAt  -> ThreadPostCreatedAt
    OrderBy_ActivityAt -> ThreadPostActivityAt
    _                  -> ThreadPostCreatedAt



getThreadPostsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity ThreadPost]
getThreadPostsM m_sp user_id = do
  case (lookupSpMay m_sp spForumId, lookupSpMay m_sp spThreadId, lookupSpMay m_sp spThreadPostId) of
    (Just forum_id, _, _)       -> getThreadPosts_ByForumIdM m_sp user_id forum_id
    (_, Just thread_id, _)      -> getThreadPosts_ByThreadIdM m_sp user_id thread_id
    (_, _, Just thread_post_id) -> getThreadPosts_ByThreadPostIdM m_sp user_id thread_post_id
    _                           -> left $ Error_InvalidArguments "forum_id, thread_id, thread_post_id"



getThreadPosts_ByForumIdM :: Maybe StandardParams -> UserId -> ForumId -> HandlerErrorEff [Entity ThreadPost]
getThreadPosts_ByForumIdM m_sp _ forum_id = do
  selectListDbEither m_sp [ThreadPostForumId ==. forum_id, ThreadPostActive ==. True] [] (orderByToField $ lookupSpMay m_sp spOrder)



getThreadPosts_ByThreadIdM :: Maybe StandardParams -> UserId -> ThreadId -> HandlerErrorEff [Entity ThreadPost]
getThreadPosts_ByThreadIdM m_sp _ thread_id = do
  selectListDbEither m_sp [ThreadPostThreadId ==. thread_id, ThreadPostActive ==. True] [] ThreadPostId



getThreadPosts_ByThreadPostIdM :: Maybe StandardParams -> UserId -> ThreadPostId -> HandlerErrorEff [Entity ThreadPost]
getThreadPosts_ByThreadPostIdM m_sp _ parent_id = do
  selectListDbEither m_sp [ThreadPostParentId ==. Just parent_id, ThreadPostActive ==. True] [] ThreadPostId



getThreadPostM :: UserId -> ThreadPostId -> HandlerErrorEff (Entity ThreadPost)
getThreadPostM _ thread_post_id = do
  selectFirstDbEither [ThreadPostId ==. thread_post_id, ThreadPostActive ==. True] []



insertThreadPostM :: Maybe StandardParams -> UserId -> ThreadPostRequest -> HandlerErrorEff (Entity ThreadPost)
insertThreadPostM m_sp user_id thread_post_request = do

  case (lookupSpMay m_sp spThreadId, lookupSpMay m_sp spThreadPostId) of

    (Just thread_id, _)      -> insertThreadPost_ByThreadIdM user_id thread_id thread_post_request
    (_, Just thread_post_id) -> insertThreadPost_ByThreadPostIdM user_id thread_post_id thread_post_request

                             -- TODO FIXME: Error_InvalidArguments "Must supply a thread_id or thread_post_id"
    _                        -> left $ Error_InvalidArguments "thread_id, thread_post_id"



insertThreadPost_ByThreadIdM :: UserId -> ThreadId -> ThreadPostRequest -> HandlerErrorEff (Entity ThreadPost)
insertThreadPost_ByThreadIdM user_id thread_id thread_post_request = do

  e_thread <- selectFirstDbEither [ThreadId ==. thread_id, ThreadActive ==. True] []
  case e_thread of
    Left err                    -> left err
    Right (Entity _ Thread{..}) -> do

      ts <- timestampH'

      let
        thread_post =
          (threadPostRequestToThreadPost user_id threadOrgId threadForumId threadBoardId thread_id Nothing thread_post_request)
            { threadPostCreatedAt = Just ts, threadPostModifiedAt = Just ts }

      thread_post_entity <- insertEntityDb thread_post

      updateWhereDb
        [ ThreadId ==. thread_id ]
        [ ThreadActivityAt =. Just ts ]

      right thread_post_entity


      -- IMPORTANT: NEED TO UPDATE THREAD'S MODIFIED_AT
      --



insertThreadPost_ByThreadPostIdM :: UserId -> ThreadPostId -> ThreadPostRequest -> HandlerErrorEff (Entity ThreadPost)
insertThreadPost_ByThreadPostIdM user_id thread_post_id thread_post_request = do

  e_post <- selectFirstDbEither [ThreadPostId ==. thread_post_id, ThreadPostActive ==. True] []
  case e_post of
    Left err                         -> left err
    Right (Entity _ ThreadPost{..}) -> do

      ts <- timestampH'

      let
        thread_post =
          (threadPostRequestToThreadPost user_id threadPostOrgId threadPostForumId threadPostBoardId threadPostThreadId (Just thread_post_id) thread_post_request)
            { threadPostCreatedAt = Just ts, threadPostModifiedAt = Just ts }

      thread_post_entity <- insertEntityDb thread_post

      updateWhereDb
        [ ThreadId ==. threadPostThreadId ]
        [ ThreadActivityAt =. Just ts ]

      right thread_post_entity




updateThreadPostM :: UserId -> ThreadPostId -> ThreadPostRequest -> HandlerErrorEff (Entity ThreadPost)
updateThreadPostM user_id thread_post_id thread_post_request = do

  ts <- timestampH'

  let
    ThreadPost{..} = (threadPostRequestToThreadPost user_id dummyId dummyId dummyId dummyId Nothing thread_post_request) { threadPostModifiedAt = Just ts }
  updateWhereDb
    [ ThreadPostUserId ==. user_id, ThreadPostId ==. thread_post_id ]
    [ ThreadPostModifiedAt  =. threadPostModifiedAt
    , ThreadPostTitle       =. threadPostTitle
    , ThreadPostBody        =. threadPostBody
    , ThreadPostTags        =. threadPostTags
    , ThreadPostPrivateTags =. threadPostPrivateTags
    , ThreadPostGuard      +=. 1
    ]

  selectFirstDbEither [ThreadPostUserId ==. user_id, ThreadPostId ==. thread_post_id, ThreadPostActive ==. True] []



deleteThreadPostM :: UserId -> ThreadPostId -> HandlerErrorEff ()
deleteThreadPostM user_id thread_post_id = do
  deleteWhereDbEither [ThreadPostUserId ==. user_id, ThreadPostId ==. thread_post_id]




countThreadPostsM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countThreadPostsM m_sp _ = do

  case (lookupSpMay m_sp spThreadId) of

    Just thread_id -> do
      n <- countDb [ ThreadPostThreadId ==. thread_id ]
      right $ CountResponses [CountResponse (keyToInt64 thread_id) (fromIntegral n)]

    _              -> left $ Error_InvalidArguments "thread_id"



getThreadPostStatsM :: UserId -> HandlerErrorEff ThreadPostStatResponse
getThreadPostStatsM _ = left Error_NotImplemented



getThreadPostStatM :: UserId -> ThreadPostId -> HandlerErrorEff ThreadPostStatResponse
getThreadPostStatM _ thread_post_id = do

  -- get like counts
  likes <- selectListDbMay Nothing [LikeEntId ==. keyToInt64 thread_post_id, LikeActive ==. True] [] LikeId

  -- get star counts
-- TODO FIXME
--  stars <- selectListDb defaultStandardParams [ StarEntityId ==. keyToInt64 thread_post_id ] [] StarId

  let
    likes_flat = map (\(Entity _ Like{..}) -> likeOpt) likes

  right $ ThreadPostStatResponse {
    threadPostStatResponseThreadPostId = keyToInt64 thread_post_id,
    threadPostStatResponseLikes        = fromIntegral $ length $ filter (==L.Like) likes_flat,
    threadPostStatResponseNeutral      = fromIntegral $ length $ filter (==L.Neutral) likes_flat,
    threadPostStatResponseDislikes     = fromIntegral $ length $ filter (==L.Dislike) likes_flat,
    threadPostStatResponseStars        = 0, -- TODO FIXME fromIntegral $ length stars,
    threadPostStatResponseViews        = 0
  }

    -- threadPostStatResponseLikes = foldl' (+) 0 $ map likeOptToScore64 $ filter (==Like) likes_flat,
    -- threadPostStatResponseNeutral = foldl' (+) 0 $ map likeOptToScore64 $ filter (==Neutral) likes_flat,
    -- threadPostStatResponseDislikes = foldl' (+) 0 $ map likeOptToScore64 $ filter (==Dislike) likes_flat,
