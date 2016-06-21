{-# LANGUAGE RecordWildCards #-}

module All.Thread (
  -- Handler
  getThreadsR,
  postThreadR0,
  getThreadR,
  getThreadH,
  putThreadR,
  deleteThreadR,
  getCountThreadsR,
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
  getThreads_ByEverythingM,
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
getThreadsR = do

  user_id <- requireAuthId

  (toJSON . threadsToResponses) <$> getThreadsM user_id



postThreadR0 :: Handler Value
postThreadR0 = do

  user_id <- requireAuthId

  sp <- lookupStandardParams

  case (spBoardId sp) of
    Nothing -> notFound
    (Just board_id) -> do

      thread_request <- requireJsonBody :: Handler ThreadRequest
      (toJSON . threadToResponse) <$> insertThreadM user_id board_id thread_request



getThreadR :: ThreadId -> Handler Value
getThreadR thread_id = do
  user_id <- requireAuthId
  (toJSON . threadToResponse) <$> getThreadM user_id thread_id



getThreadH :: Text -> Handler Value
getThreadH thread_name = do
  user_id <- requireAuthId
  (toJSON . threadToResponse) <$> getThreadMH user_id thread_name



putThreadR :: ThreadId -> Handler Value
putThreadR thread_id = do
  user_id <- requireAuthId
  thread_request <- requireJsonBody
  (toJSON . threadToResponse) <$> updateThreadM user_id thread_id thread_request



deleteThreadR :: ThreadId -> Handler Value
deleteThreadR thread_id = do
  user_id <- requireAuthId
  void $ deleteThreadM user_id thread_id
  pure $ toJSON ()



getCountThreadsR :: Handler Value
getCountThreadsR = do
  user_id <- requireAuthId
  toJSON <$> countThreadsM user_id



getThreadStatsR :: Handler Value
getThreadStatsR = do
  user_id <- requireAuthId
  toJSON <$> getThreadStatsM user_id



getThreadStatR :: ThreadId -> Handler Value
getThreadStatR thread_id = do
  user_id <- requireAuthId
  toJSON <$> getThreadStatM user_id thread_id



getThreadPacksR :: Handler Value
getThreadPacksR = do
  user_id <- requireAuthId
  toJSON <$> getThreadPacksM user_id



getThreadPackR :: ThreadId -> Handler Value
getThreadPackR thread_id = do
  user_id <- requireAuthId
  toJSON <$> getThreadPackM user_id thread_id



getThreadPackH :: Text -> Handler Value
getThreadPackH thread_name = do
  user_id <- requireAuthId
  toJSON <$> getThreadPackMH user_id thread_name








--
-- Model/Function
--

threadRequestToThread :: UserId -> BoardId -> ThreadRequest -> Thread
threadRequestToThread user_id board_id ThreadRequest{..} = Thread {
  threadUserId      = user_id,
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
orderByToField Nothing = ThreadActivityAt
orderByToField (Just order) =
  case order of
    OrderBy_CreatedAt -> ThreadCreatedAt
    OrderBy_ActivityAt -> ThreadActivityAt
    _                  -> ThreadActivityAt



{-
getThreadsM :: UserId -> Handler [Entity Thread]
getThreadsM _ = do

  sp@StandardParams{..} <- lookupStandardParams

  case spOrganizationId of
    Just org_id -> do
      runDB
        $ E.select
        $ E.from $ \(thread `E.InnerJoin` board `E.InnerJoin` forum `E.InnerJoin` org) -> do
          E.on $ forum ^. ForumOrgId E.==. org ^. OrganizationId
          E.on $ board ^. BoardForumId E.==. forum ^. ForumId
          E.on $ thread ^. ThreadBoardId E.==. board ^. BoardId
          E.where_ $ org ^. OrganizationId E.==. E.val org_id
          spToSelectE sp
          return thread
    Nothing       ->

      case spBoardId of
        -- IMPORTANT: need to specify something other than ThreadId, because of ordering

        Just board_id -> selectListDb [ThreadBoardId ==. board_id] [] (orderByToField spOrder)

        Nothing ->

          case spUserId of
            Just user_id' -> boop [ ThreadUserId ==. user_id' ]
            Nothing -> boop []

  where
  boop k = selectListDb k [] ThreadId
  -}




getThreadsM :: UserId -> Handler [Entity Thread]
getThreadsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spOrganizationId, spBoardId, spUserId) of
    (Just org_id, _, _)         -> getThreads_ByOrganizationIdM user_id org_id sp
    (_, Just board_id, _)       -> getThreads_ByBoardIdM user_id board_id sp
    (_, _, Just lookup_user_id) -> getThreads_ByUserIdM user_id lookup_user_id sp
    (_, _, _)                   -> getThreads_ByEverythingM user_id sp



getThreads_ByOrganizationIdM :: UserId -> OrganizationId -> StandardParams -> Handler [Entity Thread]
getThreads_ByOrganizationIdM _ org_id sp = do

  runDB
    $ E.select
    $ E.from $ \(thread `E.InnerJoin` board `E.InnerJoin` forum `E.InnerJoin` org) -> do
      E.on $ forum ^. ForumOrgId E.==. org ^. OrganizationId
      E.on $ board ^. BoardForumId E.==. forum ^. ForumId
      E.on $ thread ^. ThreadBoardId E.==. board ^. BoardId
      E.where_ $ org ^. OrganizationId E.==. E.val org_id
      spToSelectE sp
      return thread



getThreads_ByBoardIdM :: UserId -> BoardId -> StandardParams -> Handler [Entity Thread]
getThreads_ByBoardIdM _ board_id sp@StandardParams{..} = do

  selectListDb sp [ThreadBoardId ==. board_id] [] (orderByToField spOrder)



getThreads_ByBoardId_KeysM :: UserId -> BoardId -> StandardParams -> Handler [Key Thread]
getThreads_ByBoardId_KeysM _ board_id sp@StandardParams{..} = do

  selectKeysListDb sp [ThreadBoardId ==. board_id] [] (orderByToField spOrder)



getThreads_ByUserIdM :: UserId -> UserId -> StandardParams -> Handler [Entity Thread]
getThreads_ByUserIdM _ lookup_user_id sp = do

  selectListDb sp [ThreadUserId ==. lookup_user_id ] [] ThreadId



getThreads_ByEverythingM :: UserId -> StandardParams -> Handler [Entity Thread]
getThreads_ByEverythingM _ sp = do

  selectListDb sp [] [] ThreadId




getThreadM :: UserId -> ThreadId -> Handler (Entity Thread)
getThreadM _ thread_id = do
  notFoundMaybe =<< selectFirstDb [ ThreadId ==. thread_id ] []



getThreadMH :: UserId -> Text -> Handler (Entity Thread)
getThreadMH _ thread_name = do

  StandardParams{..} <- lookupStandardParams

  case spBoardId of

    Just board_id -> do
      notFoundMaybe =<< selectFirstDb [ ThreadName ==. thread_name, ThreadBoardId ==. board_id ] []

    Nothing -> notFound



insertThreadM :: UserId -> BoardId -> ThreadRequest -> Handler (Entity Thread)
insertThreadM user_id board_id thread_request = do

  ts <- timestampH'

  let
    thread = (threadRequestToThread user_id board_id thread_request) { threadCreatedAt = Just ts, threadActivityAt = Just ts }

  insertEntityDb thread



updateThreadM :: UserId -> ThreadId -> ThreadRequest -> Handler (Entity Thread)
updateThreadM user_id thread_id thread_request = do

  ts <- timestampH'

  let
    Thread{..} = (threadRequestToThread user_id dummyId thread_request) { threadModifiedAt = Just ts, threadActivityAt = Just ts }

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

  notFoundMaybe =<< selectFirstDb [ ThreadUserId ==. user_id, ThreadId ==. thread_id ] []



deleteThreadM :: UserId -> ThreadId -> Handler ()
deleteThreadM user_id thread_id = do
  deleteWhereDb [ ThreadUserId ==. user_id, ThreadId ==. thread_id ]



countThreadsM :: UserId -> Handler CountResponses
countThreadsM _ = do

  StandardParams{..} <- lookupStandardParams

--  case (spOrganizationId, spUserId, spForumId, spBoardId, spThreadId) of

  case spBoardId of

    Nothing -> notFound

    Just board_id -> do
      n <- countDb [ ThreadBoardId ==. board_id ]
      return $ CountResponses [CountResponse (keyToInt64 board_id) (fromIntegral n)]




getThreadStatsM :: UserId -> Handler ThreadStatResponses
getThreadStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spBoardId of

    Just _  -> notFound
    Nothing -> notFound




getThreadStatM :: UserId -> ThreadId -> Handler ThreadStatResponse
getThreadStatM _ thread_id = do
  -- get posts count
  num_thread_posts <- countDb [ ThreadPostThreadId ==. thread_id ]
  return $ ThreadStatResponse {
    threadStatResponseThreadId = keyToInt64 thread_id,
    threadStatResponseThreadPosts = fromIntegral $ num_thread_posts,
    threadStatResponseViews = 0
  }