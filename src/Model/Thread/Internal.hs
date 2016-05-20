{-# LANGUAGE RecordWildCards #-}

module Model.Thread.Internal (
  getThreadsM,

  getThreadM,
  getThreadMH,
  getThreadsBy_OrganizationIdM,
  getThreadsBy_BoardIdM,
  getThreadsBy_BoardId_KeysM,
  getThreadsBy_UserIdM,
  getThreadsBy_EverythingM,

  insertThreadM,
  updateThreadM,
  deleteThreadM,

  countThreadsM,

  getThreadStatsM,
  getThreadStatM
) where



import           Database.Esqueleto     ((^.))
import qualified Database.Esqueleto     as E
import           Model.Prelude
import           Model.Thread.Function



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
    (Just org_id, _, _)         -> getThreadsBy_OrganizationIdM user_id org_id sp
    (_, Just board_id, _)       -> getThreadsBy_BoardIdM user_id board_id sp
    (_, _, Just lookup_user_id) -> getThreadsBy_UserIdM user_id lookup_user_id sp
    (_, _, _)                   -> getThreadsBy_EverythingM user_id sp



getThreadsBy_OrganizationIdM :: UserId -> OrganizationId -> StandardParams -> Handler [Entity Thread]
getThreadsBy_OrganizationIdM _ org_id sp = do

  runDB
    $ E.select
    $ E.from $ \(thread `E.InnerJoin` board `E.InnerJoin` forum `E.InnerJoin` org) -> do
      E.on $ forum ^. ForumOrgId E.==. org ^. OrganizationId
      E.on $ board ^. BoardForumId E.==. forum ^. ForumId
      E.on $ thread ^. ThreadBoardId E.==. board ^. BoardId
      E.where_ $ org ^. OrganizationId E.==. E.val org_id
      spToSelectE sp
      return thread



getThreadsBy_BoardIdM :: UserId -> BoardId -> StandardParams -> Handler [Entity Thread]
getThreadsBy_BoardIdM _ board_id sp@StandardParams{..} = do

  selectListDb sp [ThreadBoardId ==. board_id] [] (orderByToField spOrder)



getThreadsBy_BoardId_KeysM :: UserId -> BoardId -> StandardParams -> Handler [Key Thread]
getThreadsBy_BoardId_KeysM _ board_id sp@StandardParams{..} = do

  selectKeysListDb sp [ThreadBoardId ==. board_id] [] (orderByToField spOrder)



getThreadsBy_UserIdM :: UserId -> UserId -> StandardParams -> Handler [Entity Thread]
getThreadsBy_UserIdM _ lookup_user_id sp = do

  selectListDb sp [ThreadUserId ==. lookup_user_id ] [] ThreadId



getThreadsBy_EverythingM :: UserId -> StandardParams -> Handler [Entity Thread]
getThreadsBy_EverythingM _ sp = do

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
    [ ThreadModifiedAt =. threadModifiedAt
    , ThreadActivityAt =. threadActivityAt
    , ThreadName =. threadName
    , ThreadDescription =. threadDescription
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
