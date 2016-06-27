{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module All.Board (
  -- Handler
  getBoardsR,
  postBoardR0,
  getBoardR,
  getBoardH,
  putBoardR,
  deleteBoardR,
  getBoardStatsR,
  getBoardStatR,

  -- Model/Function
  boardRequestToBoard,
  boardToResponse,
  boardsToResponses,

  -- Model/Internal
  getBoardsM,
  getBoards_ByOrganizationIdM,
  getBoards_ByForumIdM,
  getBoards_ByForumId_KeysM,
  getBoards_ByBoardParentIdM,
  getBoardM,
  getBoardMH,
  getWithBoardM,
  insertBoardM,
  updateBoardM,
  deleteBoardM,
  getBoardStatsM,
  getBoardStatM,
) where




import           All.Prelude
import           Database.Esqueleto     ((^.))
import qualified Database.Esqueleto     as E
import           All.Forum



getBoardsR :: Handler Value
getBoardsR = run $ do
  user_id <- _requireAuthId
  (toJSON . boardsToResponses) <$> getBoardsM user_id



postBoardR0 :: Handler Value
postBoardR0 = run $ do
  user_id <- _requireAuthId
  board_request <- requireJsonBody :: HandlerEff BoardRequest
  (toJSON . boardToResponse) <$> insertBoardM user_id board_request



getBoardR :: BoardId -> Handler Value
getBoardR board_id = run $ do
  user_id <- _requireAuthId
  (toJSON . boardToResponse) <$> getBoardM user_id board_id



getBoardH :: Text -> Handler Value
getBoardH board_name = run $ do
  user_id <- _requireAuthId
  (toJSON . boardToResponse) <$> getBoardMH user_id board_name



putBoardR :: BoardId -> Handler Value
putBoardR board_id = run $ do
  user_id <- _requireAuthId
  board_request <- requireJsonBody
  (toJSON . boardToResponse) <$> updateBoardM user_id board_id board_request



deleteBoardR :: BoardId -> Handler Value
deleteBoardR board_id = run $ do
  user_id <- _requireAuthId
  void $ deleteBoardM user_id board_id
  pure $ toJSON ()



getBoardStatsR :: Handler Value
getBoardStatsR = notFound



getBoardStatR :: BoardId -> Handler Value
getBoardStatR board_id = run $ do
  user_id <- _requireAuthId
  toJSON <$> getBoardStatM user_id board_id








--
-- Model/Function
--

boardRequestToBoard :: UserId -> OrganizationId -> ForumId -> Maybe BoardId -> BoardRequest -> Board
boardRequestToBoard user_id org_id forum_id m_board_id BoardRequest{..} = Board {
  boardUserId             = user_id,
  boardOrgId              = org_id,
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
  boardResponseOrgId              = keyToInt64 boardOrgId,
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

getBoardsM :: UserId -> HandlerEff [Entity Board]
getBoardsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spOrganizationId, spForumId, spParentId) of

    (J org_id, N, N)              -> getBoards_ByOrganizationIdM user_id org_id sp

    (N, Just forum_id, N)         -> getBoards_ByForumIdM user_id forum_id sp

    (N, N, J board_parent_id)     -> getBoards_ByBoardParentIdM user_id (int64ToKey' board_parent_id) sp

    (_, _, _)                     -> notFound




getBoards_ByOrganizationIdM :: UserId -> OrganizationId -> StandardParams -> HandlerEff [Entity Board]
getBoards_ByOrganizationIdM user_id org_id sp = do

  -- TODO FIXME: move this to esqueleto
   forums <- getForums_ByOrganizationIdM user_id org_id sp
   boards <- mapM (\(Entity forum_id _) -> getBoards_ByForumIdM user_id forum_id sp) forums
   return $ concat boards



getBoards_ByForumIdM :: UserId -> ForumId -> StandardParams -> HandlerEff [Entity Board]
getBoards_ByForumIdM _ forum_id sp = do

  selectListDb sp [BoardForumId ==. forum_id] [] BoardId



getBoards_ByForumId_KeysM :: UserId -> ForumId -> StandardParams -> HandlerEff [Key Board]
getBoards_ByForumId_KeysM _ forum_id sp = do

  selectKeysListDb sp [BoardForumId ==. forum_id] [] BoardId



getBoards_ByBoardParentIdM :: UserId -> BoardId -> StandardParams -> HandlerEff [Entity Board]
getBoards_ByBoardParentIdM _ board_parent_id sp = do

  selectListDb sp [BoardParentId ==. Just board_parent_id] [] BoardId



getBoardM :: UserId -> BoardId -> HandlerEff (Entity Board)
getBoardM _ board_id = do
  notFoundMaybe =<< selectFirstDb [ BoardId ==. board_id ] []



getBoardMH :: UserId -> Text -> HandlerEff (Entity Board)
getBoardMH _ board_name = do

  StandardParams{..} <- lookupStandardParams

  case spForumId of

    Just forum_id -> do
      notFoundMaybe =<< selectFirstDb [ BoardName ==. board_name, BoardForumId ==. forum_id ] []

    Nothing -> notFound



getWithBoardM :: Bool -> UserId -> BoardId -> HandlerEff (Maybe (Entity Board))
getWithBoardM False _ _ = pure Nothing
getWithBoardM True user_id board_id = do
  selectFirstDb [BoardId ==. board_id] []



insertBoardM :: UserId -> BoardRequest -> HandlerEff (Entity Board)
insertBoardM user_id board_request = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spForumId, spBoardId) of
    (Just forum_id, _) -> insertBoard_ByForumId user_id forum_id board_request
    (_, Just board_id) -> insertBoard_ByBoardId user_id board_id board_request
    (_, _)             -> permissionDenied "Must supply a forum_id or board_id"



insertBoard_ByForumId :: UserId -> ForumId -> BoardRequest -> HandlerEff (Entity Board)
insertBoard_ByForumId user_id forum_id board_request = do
  ts <- timestampH'
  (Entity _ Forum{..}) <- notFoundMaybe =<< selectFirstDb [ForumId ==. forum_id] []
  insertEntityDb $ (boardRequestToBoard user_id forumOrgId forum_id Nothing board_request) { boardCreatedAt = Just ts }



insertBoard_ByBoardId :: UserId -> BoardId -> BoardRequest -> HandlerEff (Entity Board)
insertBoard_ByBoardId user_id board_id board_request = do
  ts <- timestampH'
  (Entity board_id Board{..}) <- notFoundMaybe =<< selectFirstDb [ BoardId ==. board_id ] []
  insertEntityDb $ (boardRequestToBoard user_id boardOrgId boardForumId (Just board_id) board_request) { boardCreatedAt = Just ts }



updateBoardM :: UserId -> BoardId -> BoardRequest -> HandlerEff (Entity Board)
updateBoardM user_id board_id board_request = do

  ts <- timestampH'

  let
    Board{..} = (boardRequestToBoard user_id dummyId dummyId Nothing board_request) { boardModifiedAt = Just ts }

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



deleteBoardM :: UserId -> BoardId -> HandlerEff ()
deleteBoardM user_id board_id = do
  deleteWhereDb [ BoardUserId ==. user_id, BoardId ==. board_id ]





getBoardStatsM :: UserId -> HandlerEff Value
getBoardStatsM _ = do
  StandardParams{..} <- lookupStandardParams

  case spForumId of

    Nothing -> notFound

    Just _ -> do
      notFound




getBoardStatM :: UserId -> BoardId -> HandlerEff BoardStatResponse
getBoardStatM _ board_id = do

  threads      <- countDb [ThreadActive !=. False]
  thread_posts <- countDb [ThreadPostActive !=. False]

  return $ BoardStatResponse {
    boardStatResponseBoardId     = keyToInt64 board_id,
    boardStatResponseThreads     = fromIntegral threads,
    boardStatResponseThreadPosts = fromIntegral thread_posts,
    boardStatResponseViews       = 0
  }
