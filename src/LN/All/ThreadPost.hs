{-# LANGUAGE RecordWildCards #-}

module LN.All.ThreadPost (
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
  getThreadPosts_ByBoardIdM,
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



import           LN.All.Prelude
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
  case (lookupSpMay m_sp spForumId
       ,lookupSpMay m_sp spBoardId
       ,lookupSpMay m_sp spThreadId
       ,lookupSpMay m_sp spThreadPostId) of
    (Just forum_id, _, _, _)       -> getThreadPosts_ByForumIdM m_sp user_id forum_id
    (_, Just board_id, _, _)       -> getThreadPosts_ByBoardIdM m_sp user_id board_id
    (_, _, Just thread_id, _)      -> getThreadPosts_ByThreadIdM m_sp user_id thread_id
    (_, _, _, Just thread_post_id) -> getThreadPosts_ByThreadPostIdM m_sp user_id thread_post_id
    _                              -> left $ Error_InvalidArguments "forum_id, thread_id, thread_post_id"



getThreadPosts_ByForumIdM :: Maybe StandardParams -> UserId -> ForumId -> HandlerErrorEff [Entity ThreadPost]
getThreadPosts_ByForumIdM m_sp _ forum_id = do
  selectListDbE m_sp [ThreadPostForumId ==. forum_id, ThreadPostActive ==. True] [] (orderByToField $ lookupSpMay m_sp spOrder)



getThreadPosts_ByBoardIdM :: Maybe StandardParams -> UserId -> BoardId -> HandlerErrorEff [Entity ThreadPost]
getThreadPosts_ByBoardIdM m_sp _ board_id = do
  selectListDbE m_sp [ThreadPostBoardId ==. board_id, ThreadPostActive ==. True] [] (orderByToField $ lookupSpMay m_sp spOrder)



getThreadPosts_ByThreadIdM :: Maybe StandardParams -> UserId -> ThreadId -> HandlerErrorEff [Entity ThreadPost]
getThreadPosts_ByThreadIdM m_sp _ thread_id = do
  selectListDbE m_sp [ThreadPostThreadId ==. thread_id, ThreadPostActive ==. True] [] ThreadPostId



getThreadPosts_ByThreadPostIdM :: Maybe StandardParams -> UserId -> ThreadPostId -> HandlerErrorEff [Entity ThreadPost]
getThreadPosts_ByThreadPostIdM m_sp _ parent_id = do
  selectListDbE m_sp [ThreadPostParentId ==. Just parent_id, ThreadPostActive ==. True] [] ThreadPostId



getThreadPostM :: UserId -> ThreadPostId -> HandlerErrorEff (Entity ThreadPost)
getThreadPostM _ thread_post_id = do
  selectFirstDbE [ThreadPostId ==. thread_post_id, ThreadPostActive ==. True] []



insertThreadPostM :: Maybe StandardParams -> UserId -> ThreadPostRequest -> HandlerErrorEff (Entity ThreadPost)
insertThreadPostM m_sp user_id thread_post_request = do

  case (lookupSpMay m_sp spThreadId, lookupSpMay m_sp spThreadPostId) of

    (Just thread_id, _)      -> insertThreadPost_ByThreadIdM user_id thread_id thread_post_request
    (_, Just thread_post_id) -> insertThreadPost_ByThreadPostIdM user_id thread_post_id thread_post_request

                             -- TODO FIXME: Error_InvalidArguments "Must supply a thread_id or thread_post_id"
    _                        -> left $ Error_InvalidArguments "thread_id, thread_post_id"



insertThreadPost_ByThreadIdM :: UserId -> ThreadId -> ThreadPostRequest -> HandlerErrorEff (Entity ThreadPost)
insertThreadPost_ByThreadIdM user_id thread_id thread_post_request = do

  runEitherT $ do
    --  see mustBe_MemberOf_OrganizationIdM below
    sanitized_thread_post_request <- isT $ isValidAppM $ validateThreadPostRequest thread_post_request
    (Entity _ Thread{..})         <- isT $ selectFirstDbE [ThreadId ==. thread_id, ThreadActive ==. True] []
    isT $ mustBe_MemberOf_OrganizationIdM user_id threadOrgId
    ts                            <- lift timestampH'

    let
      thread_post =
        (threadPostRequestToThreadPost user_id threadOrgId threadForumId threadBoardId thread_id Nothing sanitized_thread_post_request)
          { threadPostCreatedAt = Just ts, threadPostModifiedAt = Just ts }

    thread_post_entity <- isT $ insertEntityDbE thread_post

    isT $ updateWhereDbE
      [ ThreadId ==. thread_id ]
      [ ThreadActivityAt =. Just ts ]

    rightT thread_post_entity


    -- IMPORTANT: NEED TO UPDATE THREAD'S MODIFIED_AT
    --



insertThreadPost_ByThreadPostIdM :: UserId -> ThreadPostId -> ThreadPostRequest -> HandlerErrorEff (Entity ThreadPost)
insertThreadPost_ByThreadPostIdM user_id thread_post_id thread_post_request = do

  runEitherT $ do
    -- see mustBe_MemberOf_OrganizationIdM below
    sanitized_thread_post_request <- isT $ isValidAppM $ validateThreadPostRequest thread_post_request
    (Entity _ ThreadPost{..})     <- isT $ selectFirstDbE [ThreadPostId ==. thread_post_id, ThreadPostActive ==. True] []
    isT $ mustBe_MemberOf_OrganizationIdM user_id threadPostOrgId
    ts                            <- lift timestampH'

    let
      thread_post =
        (threadPostRequestToThreadPost user_id threadPostOrgId threadPostForumId threadPostBoardId threadPostThreadId (Just thread_post_id) sanitized_thread_post_request)
          { threadPostCreatedAt = Just ts, threadPostModifiedAt = Just ts }

    thread_post_entity <- isT $ insertEntityDbE thread_post

    isT $ updateWhereDbE
      [ ThreadId ==. threadPostThreadId ]
      [ ThreadActivityAt =. Just ts ]

    rightT thread_post_entity




updateThreadPostM :: UserId -> ThreadPostId -> ThreadPostRequest -> HandlerErrorEff (Entity ThreadPost)
updateThreadPostM user_id thread_post_id thread_post_request = do

  runEitherT $ do
    isT $ mustBe_OwnerOf_ThreadPostIdM user_id thread_post_id
    sanitized_thread_post_request <- isT $ isValidAppM $ validateThreadPostRequest thread_post_request
    ts                            <- lift timestampH'

    let
      ThreadPost{..} = (threadPostRequestToThreadPost user_id dummyId dummyId dummyId dummyId Nothing sanitized_thread_post_request) { threadPostModifiedAt = Just ts }
    isT $ updateWhereDbE
      [ ThreadPostUserId ==. user_id, ThreadPostId ==. thread_post_id ]
      [ ThreadPostModifiedAt  =. threadPostModifiedAt
      , ThreadPostTitle       =. threadPostTitle
      , ThreadPostBody        =. threadPostBody
      , ThreadPostTags        =. threadPostTags
      , ThreadPostPrivateTags =. threadPostPrivateTags
      , ThreadPostGuard      +=. 1
      ]

    isT $ selectFirstDbE [ThreadPostUserId ==. user_id, ThreadPostId ==. thread_post_id, ThreadPostActive ==. True] []



deleteThreadPostM :: UserId -> ThreadPostId -> HandlerErrorEff ()
deleteThreadPostM user_id thread_post_id = do
  runEitherT $ do
    isT $ mustBe_OwnerOf_ThreadPostIdM user_id thread_post_id
    isT $ deleteWhereDbE [ThreadPostId ==. thread_post_id]




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
  likes <- selectListDb Nothing [LikeEntId ==. keyToInt64 thread_post_id, LikeActive ==. True] [] LikeId

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
