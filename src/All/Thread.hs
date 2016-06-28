{-# LANGUAGE RecordWildCards #-}

module All.Thread (
  -- Handler
  getThreadsR,
  postThreadR0,
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
  getThreadM,
  getThreadMH,
  getThreads_ByOrganizationIdM,
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



import           All.Prelude
import           Database.Esqueleto     ((^.))
import qualified Database.Esqueleto     as E



--
-- Handler
--

getThreadsR :: Handler Value
getThreadsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON threadsToResponses $ getThreadsM (pure sp) user_id



postThreadR0 :: Handler Value
postThreadR0 = run $ do
  user_id        <- _requireAuthId
  thread_request <- requireJsonBody :: HandlerEff ThreadRequest
  sp             <- lookupStandardParams
  errorOrJSON threadToResponse $ insertThreadM (pure sp) user_id thread_request



getThreadR :: ThreadId -> Handler Value
getThreadR thread_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON threadToResponse $ getThreadM user_id thread_id



getThreadH :: Text -> Handler Value
getThreadH thread_name = run $ do
  user_id <- _requireAuthId
  errorOrJSON threadToResponse $ getThreadMH user_id thread_name



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

threadRequestToThread :: UserId -> OrganizationId -> ForumId -> BoardId -> ThreadRequest -> Thread
threadRequestToThread user_id org_id forum_id board_id ThreadRequest{..} = Thread {
  threadUserId      = user_id,
  threadOrgId       = org_id,
  threadForumId     = forum_id,
  threadBoardId     = board_id,
  threadName        = toPrettyUrl threadRequestDisplayName,
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
  threadResponseOrgId       = keyToInt64 threadOrgId,
  threadResponseForumId     = keyToInt64 threadForumId,
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
  case (lookupSpMay m_sp spOrganizationId, lookupSpMay m_sp spBoardId, lookupSpMay m_sp spUserId) of
    (Just org_id, _, _)         -> getThreads_ByOrganizationIdM m_sp user_id org_id
    (_, Just board_id, _)       -> getThreads_ByBoardIdM m_sp user_id board_id
    (_, _, Just lookup_user_id) -> getThreads_ByUserIdM m_sp user_id lookup_user_id
    _                           -> left Error_NotImplemented



getThreads_ByOrganizationIdM :: Maybe StandardParams -> UserId -> OrganizationId -> HandlerErrorEff [Entity Thread]
getThreads_ByOrganizationIdM m_sp _ org_id = do
  selectListDbEither m_sp [ThreadOrgId ==. org_id, ThreadActive ==. True] [] (orderByToField $ lookupSpMay m_sp spOrder)



getThreads_ByBoardIdM :: Maybe StandardParams -> UserId -> BoardId -> HandlerErrorEff [Entity Thread]
getThreads_ByBoardIdM m_sp _ board_id = do
  selectListDbEither m_sp [ThreadBoardId ==. board_id, ThreadActive ==. True] [] (orderByToField $ lookupSpMay m_sp spOrder)



getThreads_ByBoardId_KeysM :: Maybe StandardParams -> UserId -> BoardId -> HandlerErrorEff [Key Thread]
getThreads_ByBoardId_KeysM m_sp _ board_id = do
  selectKeysListDbEither m_sp [ThreadBoardId ==. board_id, ThreadActive ==. True] [] (orderByToField $ lookupSpMay m_sp spOrder)



getThreads_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity Thread]
getThreads_ByUserIdM m_sp _ lookup_user_id = do
  selectListDbEither m_sp [ThreadUserId ==. lookup_user_id, ThreadActive ==. True] [] ThreadId



getThreadM :: UserId -> ThreadId -> HandlerErrorEff (Entity Thread)
getThreadM _ thread_id = do
  selectFirstDbEither [ThreadId ==. thread_id, ThreadActive ==. True] []



getThreadMH :: Maybe StandardParams -> UserId -> Text -> HandlerErrorEff (Entity Thread)
getThreadMH m_sp _ thread_name = do

  case (lookupSpMay m_sp spBoardId) of

    Just board_id -> do
      selectFirstDbEither [ThreadName ==. thread_name, ThreadBoardId ==. board_id, ThreadActive ==. True] []

    _             -> left Error_NotImplemented



getWithThreadM :: Bool -> UserId -> ThreadId -> HandlerErrorEff (Maybe (Entity Thread))
getWithThreadM False _ _              = left Error_Empty
getWithThreadM True user_id thread_id = do
  fmap Just <$> getThreadM user_id thread_id



insertThreadM :: Maybe StandardParams -> UserId -> ThreadRequest -> HandlerErrorEff (Entity Thread)
insertThreadM m_sp user_id thread_request = do
  case (lookupSpMay m_sp spBoardId) of
    Just board_id -> insertThread_ByBoardIdM m_sp user_id board_id thread_request
    _             -> left Error_NotImplemented



insertThread_ByBoardIdM :: UserId -> BoardId -> ThreadRequest -> HandlerErrorEff (Entity Thread)
insertThread_ByBoardIdM user_id board_id thread_request = do
  e_board <- selectFirstDbEither [BoardId ==. board_id, BoardActive ==. True] []
  case e_board of
    Left err                   -> left err
    Right (Entity _ Board{..}) -> do
      ts <- timestampH'
      let
        thread = (threadRequestToThread user_id boardOrgId boardForumId board_id thread_request) { threadCreatedAt = Just ts, threadActivityAt = Just ts }
      Right <$> insertEntityDb thread



updateThreadM :: UserId -> ThreadId -> ThreadRequest -> HandlerErrorEff (Entity Thread)
updateThreadM user_id thread_id thread_request = do

  ts <- timestampH'

  let
    Thread{..} = (threadRequestToThread user_id dummyId dummyId dummyId thread_request) { threadModifiedAt = Just ts, threadActivityAt = Just ts }

  updateWhereDb
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

  selectFirstDbEither [ThreadUserId ==. user_id, ThreadId ==. thread_id, ThreadActive ==. True] []



deleteThreadM :: UserId -> ThreadId -> HandlerEff ()
deleteThreadM user_id thread_id = do
  deleteWhereDb [ThreadUserId ==. user_id, ThreadId ==. thread_id, ThreadActive ==. True]



countThreadsM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countThreadsM m_sp _ = do

  case (lookupSpMay m_sp spBoardId) of

    Just board_id -> do
      n <- countDb [ ThreadBoardId ==. board_id ]
      right $ CountResponses [CountResponse (keyToInt64 board_id) (fromIntegral n)]

    _ -> left Error_NotImplemented



getThreadStatsM :: UserId -> HandlerErrorEff ThreadStatResponses
getThreadStatsM _ = left Error_NotImplemented



getThreadStatM :: UserId -> ThreadId -> HandlerErrorEff ThreadStatResponse
getThreadStatM _ thread_id = do
  num_thread_posts <- countDb [ThreadPostThreadId ==. thread_id, ThreadPostActive ==. True]
  right $ ThreadStatResponse {
    threadStatResponseThreadId    = keyToInt64 thread_id,
    threadStatResponseThreadPosts = fromIntegral $ num_thread_posts,
    threadStatResponseViews       = 0
  }
