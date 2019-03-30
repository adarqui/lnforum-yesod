{-# LANGUAGE RecordWildCards #-}

module LN.All.Thread (
  -- Handler
  getThreadsR,
  postThreadsR,
  getThreadR,
  getThreadH,
  putThreadR,
  deleteThreadR,
  getThreadsCountR,
  getThreadStatsR,
  getThreadStatR,

  -- Model/Function
  threadRequestToThread,
  threadToResponse,
  threadsToResponses,

  -- Model/Internal
  getThreadsM,
  getThreadMH,
  getThreads_ByBoardIdM,
  getThreads_ByBoardId_KeysM,
  getThreads_ByUserIdM,
  getWithThreadM,
  insertThreadM,
  updateThreadM,
  deleteThreadM,
  countThreadsM,
  getThreadStatsM,
  getThreadStatM
) where



import           LN.All.Internal
import           LN.All.Prelude
-- import           LN.All.View



--
-- Handler
--

getThreadsR :: Handler Value
getThreadsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON threadsToResponses $ getThreadsM (pure sp) user_id



postThreadsR :: Handler Value
postThreadsR = run $ do
  user_id        <- _requireAuthId
  thread_request <- requireJsonBody
  sp             <- lookupStandardParams
  errorOrJSON threadToResponse $ insertThreadM (pure sp) user_id thread_request



getThreadR :: ThreadId -> Handler Value
getThreadR thread_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON threadToResponse $ getThreadM user_id thread_id



getThreadH :: Text -> Handler Value
getThreadH thread_name = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON threadToResponse $ getThreadMH (pure sp) user_id thread_name



putThreadR :: ThreadId -> Handler Value
putThreadR thread_id = run $ do
  user_id        <- _requireAuthId
  thread_request <- requireJsonBody
  errorOrJSON threadToResponse $ updateThreadM user_id thread_id thread_request



deleteThreadR :: ThreadId -> Handler Value
deleteThreadR thread_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteThreadM user_id thread_id



getThreadsCountR :: Handler Value
getThreadsCountR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ countThreadsM (pure sp) user_id



getThreadStatsR :: Handler Value
getThreadStatsR = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getThreadStatsM user_id



getThreadStatR :: ThreadId -> Handler Value
getThreadStatR thread_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getThreadStatM user_id thread_id








--
-- Model/Function
--

threadRequestToThread :: UserId -> BoardId -> ThreadRequest -> Thread
threadRequestToThread user_id board_id ThreadRequest{..} = Thread {
  threadUserId      = user_id,
  threadBoardId     = board_id,
  threadName        = toSafeUrl threadRequestDisplayName,
  threadDisplayName = threadRequestDisplayName,
  threadDescription = threadRequestDescription,
  threadSticky      = threadRequestSticky,
  threadLocked      = threadRequestLocked,
  threadPoll        = threadRequestPoll,
  threadIcon        = threadRequestIcon,
  threadTags        = threadRequestTags,
  threadActive      = True,
  threadGuard       = threadRequestGuard,
  threadCreatedAt   = Nothing,
  threadModifiedBy  = Nothing,
  threadModifiedAt  = Nothing,
  threadActivityAt  = Nothing
}



threadToResponse :: Entity Thread -> ThreadResponse
threadToResponse (Entity thread_id Thread{..}) = ThreadResponse {
  threadResponseId          = keyToInt64 thread_id,
  threadResponseUserId      = keyToInt64 threadUserId,
  threadResponseBoardId     = keyToInt64 threadBoardId,
  threadResponseName        = threadName,
  threadResponseDisplayName = threadDisplayName,
  threadResponseDescription = threadDescription,
  threadResponseSticky      = threadSticky,
  threadResponseLocked      = threadLocked,
  threadResponsePoll        = threadPoll,
  threadResponseIcon        = threadIcon,
  threadResponseTags        = threadTags,
  threadResponseActive      = threadActive,
  threadResponseGuard       = threadGuard,
  threadResponseCreatedAt   = threadCreatedAt,
  threadResponseModifiedBy  = fmap keyToInt64 threadModifiedBy,
  threadResponseModifiedAt  = threadModifiedAt,
  threadResponseActivityAt  = threadActivityAt
}



threadsToResponses :: [Entity Thread] -> ThreadResponses
threadsToResponses threads = ThreadResponses {
  threadResponses = map threadToResponse threads
}









--
-- Model/Internal
--

-- orderByToField :: forall typ record. OrderBy -> EntityField record typ
-- TODO FIXME, this type sig might cause problems
orderByToField :: Maybe OrderBy -> EntityField Thread (Maybe UTCTime)
orderByToField Nothing      = ThreadActivityAt
orderByToField (Just order) =
  case order of
    OrderBy_CreatedAt  -> ThreadCreatedAt
    OrderBy_ActivityAt -> ThreadActivityAt
    _                  -> ThreadActivityAt



getThreadsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Thread]
getThreadsM m_sp user_id = do

  case (lookupSpMay m_sp spBoardId, lookupSpMay m_sp spUserId) of
    (Just board_id, _)       -> getThreads_ByBoardIdM m_sp user_id board_id
    (_, Just lookup_user_id) -> getThreads_ByUserIdM m_sp user_id lookup_user_id
    _                        -> leftA $ Error_InvalidArguments "user_id, board_id"



getThreads_ByBoardIdM :: Maybe StandardParams -> UserId -> BoardId -> HandlerErrorEff [Entity Thread]
getThreads_ByBoardIdM m_sp _ board_id = do
  selectListDbE m_sp [ThreadBoardId ==. board_id, ThreadActive ==. True] [] (orderByToField $ lookupSpMay m_sp spOrder)



getThreads_ByBoardId_KeysM :: Maybe StandardParams -> UserId -> BoardId -> HandlerErrorEff [Key Thread]
getThreads_ByBoardId_KeysM m_sp _ board_id = do
  selectKeysListDbE m_sp [ThreadBoardId ==. board_id, ThreadActive ==. True] [] (orderByToField $ lookupSpMay m_sp spOrder)



getThreads_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity Thread]
getThreads_ByUserIdM m_sp _ lookup_user_id = do
  selectListDbE m_sp [ThreadUserId ==. lookup_user_id, ThreadActive ==. True] [] ThreadId



getThreadMH :: Maybe StandardParams -> UserId -> Text -> HandlerErrorEff (Entity Thread)
getThreadMH m_sp _ thread_name = do

  case (lookupSpMay m_sp spBoardId) of

    Just board_id -> do
      selectFirstDbE [ThreadName ==. thread_name, ThreadBoardId ==. board_id, ThreadActive ==. True] []

    _             -> leftA $ Error_InvalidArguments "board_id"



getWithThreadM :: Bool -> UserId -> ThreadId -> HandlerErrorEff (Maybe (Entity Thread))
getWithThreadM False _ _              = rightA Nothing
getWithThreadM True user_id thread_id = fmap Just <$> getThreadM user_id thread_id



insertThreadM :: Maybe StandardParams -> UserId -> ThreadRequest -> HandlerErrorEff (Entity Thread)
insertThreadM m_sp user_id thread_request = do

  case (lookupSpMay m_sp spBoardId) of
    Just board_id -> insertThread_ByBoardIdM user_id board_id thread_request
    _             -> leftA $ Error_InvalidArguments "board_id"



insertThread_ByBoardIdM :: UserId -> BoardId -> ThreadRequest -> HandlerErrorEff (Entity Thread)
insertThread_ByBoardIdM user_id board_id thread_request = do

  runEitherT $ do
    -- see mustBe_MemberOf_OrganizationIdM below:
    sanitized_thread_request <- mustT $ isValidAppM $ validateThreadRequest thread_request
    (Entity _ Board{..})     <- mustT $ selectFirstDbE [BoardId ==. board_id, BoardActive ==. True] []
    -- TODO FIXME: perms
    -- mustT $ mustBe_MemberOf_OrganizationIdM user_id boardOrgId
    ts                       <- lift timestampH'
    let
      thread = (threadRequestToThread user_id board_id sanitized_thread_request) { threadCreatedAt = Just ts, threadActivityAt = Just ts }

    v <- mustT $ insertEntityDbE thread
    let (Entity thread_id _) = v

    -- Create view entry in the background
    --
    -- TODO FIXME:
    -- void $ fork $ void $ lift $ insertView_ByEntM user_id Ent_Thread (keyToInt64 thread_id)
    pure v



updateThreadM :: UserId -> ThreadId -> ThreadRequest -> HandlerErrorEff (Entity Thread)
updateThreadM user_id thread_id thread_request = do

  runEitherT $ do
    -- TODO FIXME: perms
    -- mustT $ mustBe_OwnerOf_ThreadIdM user_id thread_id
    sanitized_thread_request <- mustT $ isValidAppM $ validateThreadRequest thread_request
    ts                       <- lift timestampH'

    let
      Thread{..} = (threadRequestToThread user_id dummyId sanitized_thread_request) { threadModifiedAt = Just ts, threadActivityAt = Just ts }

    mustT $ updateWhereDbE
      [ ThreadUserId ==. user_id, ThreadId ==. thread_id ]
      [ ThreadModifiedAt  =. threadModifiedAt
      , ThreadActivityAt  =. threadActivityAt
      , ThreadName        =. threadName
      , ThreadDisplayName =. threadDisplayName
      , ThreadDescription =. threadDescription
      , ThreadSticky      =. threadSticky
      , ThreadLocked      =. threadLocked
      , ThreadPoll        =. threadPoll
      , ThreadIcon        =. threadIcon
      , ThreadTags        =. threadTags
      , ThreadGuard      +=. 1
      ]

    mustT $ selectFirstDbE [ThreadUserId ==. user_id, ThreadId ==. thread_id, ThreadActive ==. True] []



deleteThreadM :: UserId -> ThreadId -> HandlerErrorEff ()
deleteThreadM user_id thread_id = do
  runEitherT $ do
    -- TODO FIXME: perms
    -- mustT $ mustBe_OwnerOf_ThreadIdM user_id thread_id
    mustT $ deleteWhereDbE [ThreadId ==. thread_id, ThreadActive ==. True]



countThreadsM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countThreadsM m_sp _ = do

  case (lookupSpMay m_sp spBoardId) of

    Just board_id -> do
      n <- countDb [ ThreadBoardId ==. board_id ]
      rightA $ CountResponses [CountResponse (keyToInt64 board_id) (fromIntegral n)]

    _             -> leftA $ Error_InvalidArguments "board_id"



getThreadStatsM :: UserId -> HandlerErrorEff ThreadStatResponses
getThreadStatsM _ = leftA Error_NotImplemented



getThreadStatM :: UserId -> ThreadId -> HandlerErrorEff ThreadStatResponse
getThreadStatM user_id thread_id = do
  lr <- runEitherT $ do

    num_thread_posts <- lift $ countDb [ThreadPostThreadId ==. thread_id, ThreadPostActive ==. True]
    -- TODO FIXME: views
    -- (Entity _ views) <- mustT $ getView_ByEntM user_id Ent_Thread (keyToInt64 thread_id)

    pure num_thread_posts

  rehtie lr leftA $ \(num_thread_posts) -> do

    rightA $ ThreadStatResponse {
      threadStatResponseThreadId    = keyToInt64 thread_id,
      threadStatResponseThreadPosts = fromIntegral $ num_thread_posts,
      threadStatResponseViews       = 0 -- TODO FIXME: view count
    }

  -- rehtie lr leftA $ \(num_thread_posts, View{..}) -> do

  --   rightA $ ThreadStatResponse {
  --     threadStatResponseThreadId    = keyToInt64 thread_id,
  --     threadStatResponseThreadPosts = fromIntegral $ num_thread_posts,
  --     threadStatResponseViews       = viewCount
  --   }
