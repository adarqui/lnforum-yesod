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
  (toJSON . threadPostsToResponses) <$> getThreadPostsM user_id



postThreadPostR0 :: Handler Value
postThreadPostR0 = run $ do
  user_id <- _requireAuthId
  thread_post_request <- requireJsonBody :: HandlerEff ThreadPostRequest
  (toJSON . threadPostToResponse) <$> insertThreadPostM user_id thread_post_request



getThreadPostR :: ThreadPostId -> Handler Value
getThreadPostR thread_post_id = run $ do
  user_id <- _requireAuthId
  (toJSON . threadPostToResponse) <$> getThreadPostM user_id thread_post_id



putThreadPostR :: ThreadPostId -> Handler Value
putThreadPostR thread_post_id = run $ do
  user_id <- _requireAuthId
  thread_post_request <- requireJsonBody
  (toJSON . threadPostToResponse) <$> updateThreadPostM user_id thread_post_id thread_post_request



deleteThreadPostR :: ThreadPostId -> Handler Value
deleteThreadPostR thread_post_id = run $ do
  user_id <- _requireAuthId
  void $ deleteThreadPostM user_id thread_post_id
  pure $ toJSON ()



getThreadPostsCountR :: Handler Value
getThreadPostsCountR = run $ do
  user_id <- _requireAuthId
  toJSON <$> countThreadPostsM user_id



getThreadPostStatsR :: Handler Value
getThreadPostStatsR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getThreadPostStatsM user_id



getThreadPostStatR :: ThreadPostId -> Handler Value
getThreadPostStatR thread_post_id = run $ do
  user_id <- _requireAuthId
  toJSON <$> getThreadPostStatM user_id thread_post_id







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

{-
getThreadPostsM :: UserId -> Maybe ThreadId -> Maybe ThreadPostId -> HandlerEff [Entity ThreadPost]
getThreadPostsM _ mthread_id mthread_post_id = do
  selectListDb query [] ThreadPostId
  where
  query = threads ++ posts
  threads = case mthread_id of
              Nothing -> []
              Just thread_id -> [ ThreadPostThreadId ==. thread_id ]
  posts = case mthread_post_id of
              Nothing -> []
              Just thread_post_id -> [ ThreadPostParentId ==. Just thread_post_id ]
              -}



getThreadPostsM :: UserId -> HandlerEff [Entity ThreadPost]
getThreadPostsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spThreadId, spThreadPostId) of

    (Just thread_id, _)      -> getThreadPosts_ByThreadIdM user_id thread_id sp

    (_, Just thread_post_id) -> getThreadPosts_ByThreadPostIdM user_id thread_post_id sp

    (_, _)                   -> notFound



getThreadPosts_ByThreadIdM :: UserId -> ThreadId -> StandardParams -> HandlerEff [Entity ThreadPost]
getThreadPosts_ByThreadIdM _ thread_id sp = do
  selectListDb sp [ThreadPostThreadId ==. thread_id] [] ThreadPostId



getThreadPosts_ByThreadPostIdM :: UserId -> ThreadPostId -> StandardParams -> HandlerEff [Entity ThreadPost]
getThreadPosts_ByThreadPostIdM _ parent_id sp = do
  selectListDb sp [ThreadPostParentId ==. Just parent_id] [] ThreadPostId



getThreadPostM :: UserId -> ThreadPostId -> HandlerEff (Entity ThreadPost)
getThreadPostM _ thread_post_id = do
  notFoundMaybe =<< selectFirstDb [ ThreadPostId ==. thread_post_id ] []



insertThreadPostM :: UserId -> ThreadPostRequest -> HandlerEff (Entity ThreadPost)
insertThreadPostM user_id thread_post_request = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spThreadId, spThreadPostId) of

    (Just thread_id, _)      -> insertThreadPost_ByThreadIdM user_id thread_id thread_post_request
    (_, Just thread_post_id) -> insertThreadPost_ByThreadPostIdM user_id thread_post_id thread_post_request
    (_, _)                   -> permissionDenied "Must supply a thread_id or thread_post_id"



insertThreadPost_ByThreadIdM :: UserId -> ThreadId -> ThreadPostRequest -> HandlerEff (Entity ThreadPost)
insertThreadPost_ByThreadIdM user_id thread_id thread_post_request = do

  (Entity _ Thread{..}) <- notFoundMaybe =<< selectFirstDb [ThreadId ==. thread_id] []

  ts <- timestampH'

  let
    thread_post =
      (threadPostRequestToThreadPost user_id threadOrgId threadForumId threadBoardId thread_id Nothing thread_post_request)
        { threadPostCreatedAt = Just ts, threadPostModifiedAt = Just ts }

  v <- insertEntityDb thread_post

  updateWhereDb
    [ ThreadId ==. thread_id ]
    [ ThreadActivityAt =. Just ts ]

  return v


  -- IMPORTANT: NEED TO UPDATE THREAD'S MODIFIED_AT
  --

insertThreadPost_ByThreadPostIdM :: UserId -> ThreadPostId -> ThreadPostRequest -> HandlerEff (Entity ThreadPost)
insertThreadPost_ByThreadPostIdM user_id thread_post_id thread_post_request = do

  (Entity _ ThreadPost{..}) <- notFoundMaybe =<< selectFirstDb [ThreadPostId ==. thread_post_id] []

  ts <- timestampH'

  let
    thread_post =
      (threadPostRequestToThreadPost user_id threadPostOrgId threadPostForumId threadPostBoardId threadPostThreadId (Just thread_post_id) thread_post_request)
        { threadPostCreatedAt = Just ts, threadPostModifiedAt = Just ts }

  v <- insertEntityDb thread_post

  updateWhereDb
    [ ThreadId ==. threadPostThreadId ]
    [ ThreadActivityAt =. Just ts ]

  return v




updateThreadPostM :: UserId -> ThreadPostId -> ThreadPostRequest -> HandlerEff (Entity ThreadPost)
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
  notFoundMaybe =<< selectFirstDb [ ThreadPostUserId ==. user_id, ThreadPostId ==. thread_post_id ] []



deleteThreadPostM :: UserId -> ThreadPostId -> HandlerEff ()
deleteThreadPostM user_id thread_post_id = do
  deleteWhereDb [ ThreadPostUserId ==. user_id, ThreadPostId ==. thread_post_id ]




countThreadPostsM :: UserId -> HandlerEff CountResponses
countThreadPostsM _ = do

  StandardParams{..} <- lookupStandardParams

--  case (spOrganizationId, spUserId, spForumId, spBoardId, spThreadId) of

  case spThreadId of

    Nothing -> notFound

    Just thread_id -> do
      n <- countDb [ ThreadPostThreadId ==. thread_id ]
      return $ CountResponses [CountResponse (keyToInt64 thread_id) (fromIntegral n)]



getThreadPostStatsM :: UserId -> HandlerEff ThreadPostStatResponse
getThreadPostStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spThreadId of

    Just _  -> notFound
    Nothing -> notFound



getThreadPostStatM :: UserId -> ThreadPostId -> HandlerEff ThreadPostStatResponse
getThreadPostStatM _ thread_post_id = do

  -- get like counts
  likes <- selectListDb defaultStandardParams [ LikeEntId ==. keyToInt64 thread_post_id ] [] LikeId

  -- get star counts
-- TODO FIXME
--  stars <- selectListDb defaultStandardParams [ StarEntityId ==. keyToInt64 thread_post_id ] [] StarId

  let
    likes_flat = map (\(Entity _ Like{..}) -> likeOpt) likes

  return $ ThreadPostStatResponse {
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
