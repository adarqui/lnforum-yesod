{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.Board.Internal (
  getBoardsM,
  getBoardsBy_OrganizationIdM,
  getBoardsBy_OrganizationNameM,
  getBoardsBy_ForumIdM,
  getBoardsBy_ForumId_KeysM,
  getBoardsBy_ForumNameM,
  getBoardsBy_OrganizationName_ForumNameM,
  getBoardsBy_BoardParentIdM,
  getBoardsBy_OrganizationName_ForumName_BoardParentNameM,
  getBoardsBy_EverythingM,

  getBoardM,
  getBoardMH,
  insertBoardM,
  updateBoardM,
  deleteBoardM,

  getBoardStatsM,
  getBoardStatM,
) where



import           Database.Esqueleto     ((^.))
import qualified Database.Esqueleto     as E
import           Model.Prelude
import           Model.Board.Function
import           Model.Forum.Internal




getBoardsM :: UserId -> Handler [Entity Board]
getBoardsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spOrganizationId, spOrganizationName, spForumId, spForumName, spParentId, spParentName) of

    (J org_id, N, N, N, N, N)              -> getBoardsBy_OrganizationIdM user_id org_id sp

    (N, J org_name, N, N, N, N)            -> getBoardsBy_OrganizationNameM user_id org_name sp

    (N, N, Just forum_id, N, N, N)         -> getBoardsBy_ForumIdM user_id forum_id sp

    (N, J org_name, N, J forum_name, N, N) -> getBoardsBy_OrganizationName_ForumNameM user_id org_name forum_name sp

    (N, N, N, N, J board_parent_id, N)     -> getBoardsBy_BoardParentIdM user_id (int64ToKey' board_parent_id) sp

    (N, J org_name, N, J forum_name, N, J board_name) -> getBoardsBy_OrganizationName_ForumName_BoardParentNameM user_id org_name forum_name board_name sp

    (_, _, _, _, _, _)                     -> getBoardsBy_EverythingM user_id sp




getBoardsBy_OrganizationIdM :: UserId -> OrganizationId -> StandardParams -> Handler [Entity Board]
getBoardsBy_OrganizationIdM user_id org_id sp = do

  -- TODO FIXME: move this to esqueleto
   forums <- getForumsBy_OrganizationIdM user_id org_id sp
   boards <- mapM (\(Entity forum_id _) -> getBoardsBy_ForumIdM user_id forum_id sp) forums
   return $ concat boards



getBoardsBy_OrganizationNameM :: UserId -> Text -> StandardParams -> Handler [Entity Board]
getBoardsBy_OrganizationNameM user_id org_name sp = do

  -- TODO FIXME: move this to esqueleto
   forums <- getForumsBy_OrganizationNameM user_id org_name sp
   boards <- mapM (\(Entity forum_id _) -> getBoardsBy_ForumIdM user_id forum_id sp) forums
   return $ concat boards



getBoardsBy_ForumIdM :: UserId -> ForumId -> StandardParams -> Handler [Entity Board]
getBoardsBy_ForumIdM _ forum_id sp = do

  selectListDb sp [BoardForumId ==. forum_id] [] BoardId



getBoardsBy_ForumId_KeysM :: UserId -> ForumId -> StandardParams -> Handler [Key Board]
getBoardsBy_ForumId_KeysM _ forum_id sp = do

  selectKeysListDb sp [BoardForumId ==. forum_id] [] BoardId



getBoardsBy_ForumNameM :: UserId -> Text -> StandardParams -> Handler [Entity Board]
getBoardsBy_ForumNameM user_id forum_name sp = do

  (Entity forum_id _) <- getForumMH user_id forum_name
  getBoardsBy_ForumIdM user_id forum_id sp



getBoardsBy_OrganizationName_ForumNameM :: UserId -> Text -> Text -> StandardParams -> Handler [Entity Board]
getBoardsBy_OrganizationName_ForumNameM user_id org_name forum_name sp = do

  (Entity forum_id _) <- getForumBy_OrganizationName_ForumNameM user_id org_name forum_name sp
  selectListDb sp [BoardForumId ==. forum_id] [] BoardId



getBoardsBy_BoardParentIdM :: UserId -> BoardId -> StandardParams -> Handler [Entity Board]
getBoardsBy_BoardParentIdM _ board_parent_id sp = do

  selectListDb sp [BoardParentId ==. Just board_parent_id] [] BoardId



getBoardsBy_OrganizationName_ForumName_BoardParentNameM :: UserId -> Text -> Text -> Text -> StandardParams -> Handler [Entity Board]
getBoardsBy_OrganizationName_ForumName_BoardParentNameM user_id org_name forum_name parent_name sp = do

  (Entity forum_id _) <- getForumBy_OrganizationName_ForumNameM user_id org_name forum_name sp
  selectListDb sp [BoardName ==. parent_name, BoardForumId ==. forum_id] [] BoardId



getBoardsBy_EverythingM :: UserId -> StandardParams -> Handler [Entity Board]
getBoardsBy_EverythingM _ sp = do

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
