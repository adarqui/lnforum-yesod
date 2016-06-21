{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module All.Board (
  getBoardsR,
  postBoardR0,
  getBoardR,
  getBoardH,
  putBoardR,
  deleteBoardR,

  getBoardStatsR,
  getBoardStatR,

  getBoardPacksR,
  getBoardPackR,
  getBoardPackH,

  -- Model/Function
  boardRequestToBoard,
  boardToResponse,
  boardsToResponses,

  -- Model/Internal
  getBoardsM,
  getBoards_ByOrganizationIdM,
  getBoards_ByOrganizationNameM,
  getBoards_ByForumIdM,
  getBoards_ByForumId_KeysM,
  getBoards_ByForumNameM,
  getBoards_ByBoardParentIdM,
  getBoards_ByEverythingM,

  getBoardM,
  getBoardMH,
  insertBoardM,
  updateBoardM,
  deleteBoardM,

  getBoardStatsM,
  getBoardStatM,
) where




import           Handler.Prelude
import           Database.Esqueleto     ((^.))
import qualified Database.Esqueleto     as E
import           Model.Prelude
import           All.Forum



getBoardsR :: Handler Value
getBoardsR = do
  user_id <- requireAuthId
  (toJSON . boardsToResponses) <$> getBoardsM user_id



postBoardR0 :: Handler Value
postBoardR0 = do

  user_id <- requireAuthId

  sp <- lookupStandardParams

  case (spForumId sp, spBoardId sp) of

    (Nothing, Nothing) -> permissionDenied "Must supply a forum_id or board_id"

    _ -> do
      board_request <- requireJsonBody :: Handler BoardRequest
      (toJSON . boardToResponse)  <$> insertBoardM user_id (spForumId sp) (spBoardId sp) board_request



getBoardR :: BoardId -> Handler Value
getBoardR board_id = do
  user_id <- requireAuthId
  (toJSON . boardToResponse) <$> getBoardM user_id board_id



getBoardH :: Text -> Handler Value
getBoardH board_name = do
  user_id <- requireAuthId
  (toJSON . boardToResponse) <$> getBoardMH user_id board_name



putBoardR :: BoardId -> Handler Value
putBoardR board_id = do
  user_id <- requireAuthId
  board_request <- requireJsonBody
  (toJSON . boardToResponse) <$> updateBoardM user_id board_id board_request



deleteBoardR :: BoardId -> Handler Value
deleteBoardR board_id = do
  user_id <- requireAuthId
  void $ deleteBoardM user_id board_id
  pure $ toJSON ()



getBoardStatsR :: Handler Value
getBoardStatsR = notFound



getBoardPacksR :: Handler Value
getBoardPacksR = do
  user_id <- requireAuthId
  toJSON <$> getBoardPacksM user_id



getBoardStatR :: BoardId -> Handler Value
getBoardStatR board_id = do
  user_id <- requireAuthId
  toJSON <$> getBoardStatM user_id board_id



getBoardPackR :: BoardId -> Handler Value
getBoardPackR board_id = do
  user_id <- requireAuthId
  toJSON <$> getBoardPackM user_id board_id



getBoardPackH :: Text -> Handler Value
getBoardPackH board_name = do
  user_id <- requireAuthId
  toJSON <$> getBoardPackMH user_id board_name




--
-- Model/Function
--

boardRequestToBoard :: UserId -> ForumId -> Maybe BoardId -> BoardRequest -> Board
boardRequestToBoard user_id forum_id m_board_id BoardRequest{..} = Board {
  boardUserId             = user_id,
  boardForumId            = forum_id,
  boardParentId           = m_board_id,
  boardName               = toPrettyUrl boardRequestDisplayName,
  boardDisplayName        = boardRequestDisplayName,
  boardDescription        = boardRequestDescription,
  boardIsAnonymous        = boardRequestIsAnonymous,
  boardCanCreateSubBoards = boardRequestCanCreateSubBoards,
  boardCanCreateThreads   = boardRequestCanCreateThreads,
  boardSuggestedTags      = boardRequestSuggestedTags,
  boardIcon               = boardRequestIcon,
  boardTags               = boardRequestTags,
  boardActive             = True,
  boardGuard              = boardRequestGuard,
  boardCreatedAt          = Nothing,
  boardModifiedBy         = Nothing,
  boardModifiedAt         = Nothing,
  boardActivityAt         = Nothing
}



boardToResponse :: Entity Board -> BoardResponse
boardToResponse (Entity board_id Board{..}) = BoardResponse {
  boardResponseId                 = keyToInt64 board_id,
  boardResponseUserId             = keyToInt64 boardUserId,
  boardResponseForumId            =  keyToInt64 boardForumId,
  boardResponseParentId           = fmap keyToInt64 boardParentId,
  boardResponseName               = boardName,
  boardResponseDisplayName        = boardDisplayName,
  boardResponseDescription        = boardDescription,
  boardResponseIsAnonymous        = boardIsAnonymous,
  boardResponseCanCreateSubBoards = boardCanCreateSubBoards,
  boardResponseCanCreateThreads   = boardCanCreateThreads,
  boardResponseSuggestedTags      = boardSuggestedTags,
  boardResponseIcon               = boardIcon,
  boardResponseTags               = boardTags,
  boardResponseActive             = boardActive,
  boardResponseGuard              = boardGuard,
  boardResponseCreatedAt          = boardCreatedAt,
  boardResponseModifiedBy         = fmap keyToInt64 boardModifiedBy,
  boardResponseModifiedAt         = boardModifiedAt,
  boardResponseActivityAt         = boardActivityAt
}



boardsToResponses :: [Entity Board] -> BoardResponses
boardsToResponses boards = BoardResponses {
  boardResponses = map boardToResponse boards
}





--
-- Model/Internal
--

getBoardsM :: UserId -> Handler [Entity Board]
getBoardsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spOrganizationId, spOrganizationName, spForumId, spForumName, spParentId, spParentName) of

    (J org_id, N, N, N, N, N)              -> getBoards_ByOrganizationIdM user_id org_id sp

    (N, J org_name, N, N, N, N)            -> getBoards_ByOrganizationNameM user_id org_name sp

    (N, N, Just forum_id, N, N, N)         -> getBoards_ByForumIdM user_id forum_id sp

--    (N, J org_name, N, J forum_name, N, N) -> getBoards_ByOrganizationName_ForumNameM user_id org_name forum_name sp

    (N, N, N, N, J board_parent_id, N)     -> getBoards_ByBoardParentIdM user_id (int64ToKey' board_parent_id) sp

--    (N, J org_name, N, J forum_name, N, J board_name) -> getBoards_ByOrganizationName_ForumName_BoardParentNameM user_id org_name forum_name board_name sp

    (_, _, _, _, _, _)                     -> getBoards_ByEverythingM user_id sp




getBoards_ByOrganizationIdM :: UserId -> OrganizationId -> StandardParams -> Handler [Entity Board]
getBoards_ByOrganizationIdM user_id org_id sp = do

  -- TODO FIXME: move this to esqueleto
   forums <- getForums_ByOrganizationIdM user_id org_id sp
   boards <- mapM (\(Entity forum_id _) -> getBoards_ByForumIdM user_id forum_id sp) forums
   return $ concat boards



getBoards_ByOrganizationNameM :: UserId -> Text -> StandardParams -> Handler [Entity Board]
getBoards_ByOrganizationNameM user_id org_name sp = do

  -- TODO FIXME: move this to esqueleto
   forums <- getForums_ByOrganizationNameM user_id org_name sp
   boards <- mapM (\(Entity forum_id _) -> getBoards_ByForumIdM user_id forum_id sp) forums
   return $ concat boards



getBoards_ByForumIdM :: UserId -> ForumId -> StandardParams -> Handler [Entity Board]
getBoards_ByForumIdM _ forum_id sp = do

  selectListDb sp [BoardForumId ==. forum_id] [] BoardId



getBoards_ByForumId_KeysM :: UserId -> ForumId -> StandardParams -> Handler [Key Board]
getBoards_ByForumId_KeysM _ forum_id sp = do

  selectKeysListDb sp [BoardForumId ==. forum_id] [] BoardId



getBoards_ByForumNameM :: UserId -> Text -> StandardParams -> Handler [Entity Board]
getBoards_ByForumNameM user_id forum_name sp = do

  (Entity forum_id _) <- getForumMH user_id forum_name
  getBoards_ByForumIdM user_id forum_id sp



-- getBoards_ByOrganizationName_ForumNameM :: UserId -> Text -> Text -> StandardParams -> Handler [Entity Board]
-- getBoards_ByOrganizationName_ForumNameM user_id org_name forum_name sp = do

--   (Entity forum_id _) <- getForum_ByOrganizationName_ForumNameM user_id org_name forum_name sp
--   selectListDb sp [BoardForumId ==. forum_id] [] BoardId



getBoards_ByBoardParentIdM :: UserId -> BoardId -> StandardParams -> Handler [Entity Board]
getBoards_ByBoardParentIdM _ board_parent_id sp = do

  selectListDb sp [BoardParentId ==. Just board_parent_id] [] BoardId



-- getBoards_ByOrganizationName_ForumName_BoardParentNameM :: UserId -> Text -> Text -> Text -> StandardParams -> Handler [Entity Board]
-- getBoards_ByOrganizationName_ForumName_BoardParentNameM user_id org_name forum_name parent_name sp = do

--   (Entity forum_id _) <- getForum_ByOrganizationName_ForumNameM user_id org_name forum_name sp
--   selectListDb sp [BoardName ==. parent_name, BoardForumId ==. forum_id] [] BoardId



getBoards_ByEverythingM :: UserId -> StandardParams -> Handler [Entity Board]
getBoards_ByEverythingM _ sp = do

  selectListDb sp [] [] BoardId



getBoardM :: UserId -> BoardId -> Handler (Entity Board)
getBoardM _ board_id = do
  notFoundMaybe =<< selectFirstDb [ BoardId ==. board_id ] []



getBoardMH :: UserId -> Text -> Handler (Entity Board)
getBoardMH _ board_name = do

  StandardParams{..} <- lookupStandardParams

  case spForumId of

    Just forum_id -> do
      notFoundMaybe =<< selectFirstDb [ BoardName ==. board_name, BoardForumId ==. forum_id ] []

    Nothing -> notFound



insertBoardM :: UserId -> Maybe ForumId -> Maybe BoardId -> BoardRequest -> Handler (Entity Board)
insertBoardM user_id forum_id _ board_request = do

  ts <- timestampH'

  sp@StandardParams{..} <- lookupStandardParams

  case (spForumId, spBoardId) of

    (Nothing, Nothing) -> permissionDenied "Must supply a forum_id or board_id"

    (Just forum_id, _) -> do

      insertEntityDb $ (boardRequestToBoard user_id forum_id Nothing board_request) { boardCreatedAt = Just ts }

    (_, Just board_id) -> do

      (Entity board_id Board{..}) <- notFoundMaybe =<< selectFirstDb [ BoardId ==. board_id ] []
      insertEntityDb $ (boardRequestToBoard user_id boardForumId (Just board_id) board_request) { boardCreatedAt = Just ts }



updateBoardM :: UserId -> BoardId -> BoardRequest -> Handler (Entity Board)
updateBoardM user_id board_id board_request = do

  ts <- timestampH'

  let
    Board{..} = (boardRequestToBoard user_id dummyId Nothing board_request) { boardModifiedAt = Just ts }

  updateWhereDb
    [ BoardUserId ==. user_id, BoardId ==. board_id ]
    [ BoardModifiedAt         =. boardModifiedAt
    , BoardActivityAt         =. Just ts
    , BoardName               =. boardName
    , BoardDisplayName        =. boardDisplayName
    , BoardDescription        =. boardDescription
    , BoardIsAnonymous        =. boardIsAnonymous
    , BoardCanCreateSubBoards =. boardCanCreateSubBoards
    , BoardCanCreateThreads   =. boardCanCreateThreads
    , BoardSuggestedTags      =. boardSuggestedTags
    , BoardIcon               =. boardIcon
    , BoardTags               =. boardTags
    , BoardGuard             +=. 1
    ]

  notFoundMaybe =<< selectFirstDb [ BoardUserId ==. user_id, BoardId ==. board_id ] []



deleteBoardM :: UserId -> BoardId -> Handler ()
deleteBoardM user_id board_id = do
  deleteWhereDb [ BoardUserId ==. user_id, BoardId ==. board_id ]





getBoardStatsM :: UserId -> Handler Value
getBoardStatsM _ = do
  StandardParams{..} <- lookupStandardParams

  case spForumId of

    Nothing -> notFound

    Just _ -> do
      notFound




getBoardStatM :: UserId -> BoardId -> Handler BoardStatResponse
getBoardStatM _ board_id = do

{-
 - select COUNT(DISTINCT board.id), COUNT(DISTINCT thread.id), COUNT(DISTINCT thread_post.id) from board LEFT JOIN thread ON (board.id=thread.board_id) LEFT JOIN thread_post ON (thread.id=thread_post.thread_id) where board.id=1394;
 -  count | count | count
 -  -------+-------+-------
 -       1 |   443 |    99
 -       (1 row)
-}

  stats <- qBoardStats board_id

  let ((_,threads,thread_posts):[]) = fmap (\(x,y,z) -> (E.unValue x, E.unValue y, E.unValue z)) stats

  return $ BoardStatResponse {
    boardStatResponseBoardId = keyToInt64 board_id,
    boardStatResponseThreads = threads,
    boardStatResponseThreadPosts = thread_posts,
    boardStatResponseViews = 0
  }




qBoardStats :: forall site.
     (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
     Key Board -> HandlerT site IO [(E.Value Int64, E.Value Int64, E.Value Int64)]
qBoardStats board_id = do
  runDB
    $ E.select
    $ E.from $ \((thread_post :: E.SqlExpr (Entity ThreadPost)) `E.InnerJoin` (thread :: E.SqlExpr (Entity Thread)) `E.InnerJoin` (board :: E.SqlExpr (Entity Board))) -> do
      E.on $ thread ^. ThreadBoardId E.==. board ^. BoardId
      E.on $ thread_post ^. ThreadPostThreadId E.==. thread ^. ThreadId
      E.where_ $
        board ^. BoardId E.==. E.val board_id
      let _ = thread_post ^. ThreadPostId
      let _ = thread ^. ThreadId
      let _ = board ^. BoardId
      return (E.countDistinct $ board ^. BoardId, E.countDistinct $ thread ^. ThreadId, E.countDistinct $ thread_post ^. ThreadPostId)
