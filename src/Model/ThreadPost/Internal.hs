{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.ThreadPost.Internal (
  getThreadPostsM,
  getThreadPostsBy_ThreadIdM,
  getThreadPostsBy_ThreadPostIdM,
  getThreadPostsBy_EverythingM,

  getThreadPostM,
  insertThreadPostM,
  updateThreadPostM,
  deleteThreadPostM,
  countThreadPostsM,
  getThreadPostStatsM,
  getThreadPostStatM
) where



import           Model.Prelude
import           Model.ThreadPost.Function



{-
getThreadPostsM :: UserId -> Maybe ThreadId -> Maybe ThreadPostId -> Handler [Entity ThreadPost]
getThreadPostsM _ mthread_id mthreadPost_id = do
  selectListDb query [] ThreadPostId
  where
  query = threads ++ posts
  threads = case mthread_id of
              Nothing -> []
              Just thread_id -> [ ThreadPostThreadId ==. thread_id ]
  posts = case mthreadPost_id of
              Nothing -> []
              Just thread_post_id -> [ ThreadPostParentId ==. Just thread_post_id ]
              -}



getThreadPostsM :: UserId -> Handler [Entity ThreadPost]
getThreadPostsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spOrganizationId, spUserId, spForumId, spBoardId, spThreadId, spThreadPostId) of

    (_, _, _, _, Just thread_id, _)      -> getThreadPostsBy_ThreadIdM user_id thread_id sp

    (_, _, _, _, _, Just thread_post_id) -> getThreadPostsBy_ThreadPostIdM user_id thread_post_id sp

    (_, _, _, _, _, _)                   -> getThreadPostsBy_EverythingM user_id sp



getThreadPostsBy_ThreadIdM :: UserId -> ThreadId -> StandardParams -> Handler [Entity ThreadPost]
getThreadPostsBy_ThreadIdM _ thread_id sp = do
  selectListDb sp [ThreadPostThreadId ==. thread_id] [] ThreadPostId



getThreadPostsBy_ThreadPostIdM :: UserId -> ThreadPostId -> StandardParams -> Handler [Entity ThreadPost]
getThreadPostsBy_ThreadPostIdM _ parent_id sp = do
  selectListDb sp [ThreadPostParentId ==. Just parent_id] [] ThreadPostId



getThreadPostsBy_EverythingM :: UserId -> StandardParams -> Handler [Entity ThreadPost]
getThreadPostsBy_EverythingM _ sp = do
  selectListDb sp [] [] ThreadPostId



getThreadPostM :: UserId -> ThreadPostId -> Handler (Entity ThreadPost)
getThreadPostM _ thread_post_id = do
  notFoundMaybe =<< selectFirstDb [ ThreadPostId ==. thread_post_id ] []



insertThreadPostM :: UserId -> Maybe ThreadId -> Maybe ThreadPostId -> ThreadPostRequest -> Handler (Entity ThreadPost)
insertThreadPostM user_id mthread_id mthread_post_id threadPost_request = do

  ts <- timestampH'

  liftIO $ print "hello"
  liftIO $ print mthread_id

  let
    thread_post = (threadPostRequestToThreadPost user_id (fromJust mthread_id) mthread_post_id threadPost_request) { threadPostCreatedAt = Just ts, threadPostModifiedAt = Just ts }

  v <- insertEntityDb thread_post

  updateWhereDb [ThreadId ==. (fromJust mthread_id)] [ThreadActivityAt =. Just ts]

  return v


  -- IMPORTANT: NEED TO UPDATE THREAD'S MODIFIED_AT
  --



updateThreadPostM :: UserId -> ThreadPostId -> ThreadPostRequest -> Handler (Entity ThreadPost)
updateThreadPostM user_id thread_post_id threadPost_request = do

  ts <- timestampH'

  let
    ThreadPost{..} = (threadPostRequestToThreadPost user_id dummyId Nothing threadPost_request) { threadPostModifiedAt = Just ts }
  updateWhereDb
    [ ThreadPostUserId ==. user_id, ThreadPostId ==. thread_post_id ]
    [ ThreadPostModifiedAt =. threadPostModifiedAt
    , ThreadPostTitle =. threadPostTitle
    , ThreadPostBody =. threadPostBody
    ]
  notFoundMaybe =<< selectFirstDb [ ThreadPostUserId ==. user_id, ThreadPostId ==. thread_post_id ] []



deleteThreadPostM :: UserId -> ThreadPostId -> Handler ()
deleteThreadPostM user_id thread_post_id = do
  deleteWhereDb [ ThreadPostUserId ==. user_id, ThreadPostId ==. thread_post_id ]




countThreadPostsM :: UserId -> Handler CountResponses
countThreadPostsM _ = do

  StandardParams{..} <- lookupStandardParams

--  case (spOrganizationId, spUserId, spForumId, spBoardId, spThreadId) of

  case spThreadId of

    Nothing -> notFound

    Just thread_id -> do
      n <- countDb [ ThreadPostThreadId ==. thread_id ]
      return $ CountResponses [CountResponse (keyToInt64 thread_id) (fromIntegral n)]



getThreadPostStatsM :: UserId -> Handler ThreadPostStatResponse
getThreadPostStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spThreadId of

    Just _  -> notFound
    Nothing -> notFound



getThreadPostStatM :: UserId -> ThreadPostId -> Handler ThreadPostStatResponse
getThreadPostStatM _ thread_post_id = do
  -- get like counts
  likes <- selectListDb defaultStandardParams [ ThreadPostLikeThreadPostId ==. thread_post_id ] [] ThreadPostLikeId
  let
    likes_flat = map (\(Entity _ ThreadPostLike{..}) -> threadPostLikeOpt) likes
  return $ ThreadPostStatResponse {
    threadPostStatResponseThreadPostId = keyToInt64 thread_post_id,
    threadPostStatResponseLikes = foldl' (+) 0 $ map likeOptToScore64 $ filter (==Like) likes_flat,
    threadPostStatResponseDislikes = foldl' (+) 0 $ map likeOptToScore64 $ filter (==DontLike) likes_flat,
    threadPostStatResponseStarred = 0,
    threadPostStatResponseViews = 0
  }
