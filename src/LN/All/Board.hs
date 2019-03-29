{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module LN.All.Board (
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
  getBoardMH,
  getWithBoardM,
  insertBoardM,
  updateBoardM,
  deleteBoardM,
  getBoardStatsM,
  getBoardStatM,
) where




import           LN.All.Internal
import           LN.All.Prelude
import           LN.All.Forum
import LN.T.Visibility



--
-- Handler
--

getBoardsR :: Handler Value
getBoardsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON boardsToResponses $ getBoardsM (pure sp) user_id



postBoardR0 :: Handler Value
postBoardR0 = run $ do
  user_id       <- _requireAuthId
  board_request <- requireJsonBody :: HandlerEff BoardRequest
  sp            <- lookupStandardParams
  errorOrJSON boardToResponse $ insertBoardM (pure sp) user_id board_request



getBoardR :: BoardId -> Handler Value
getBoardR board_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON boardToResponse $ getBoardM user_id board_id



getBoardH :: Text -> Handler Value
getBoardH board_name = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON boardToResponse $ getBoardMH (pure sp) user_id board_name



putBoardR :: BoardId -> Handler Value
putBoardR board_id = run $ do
  user_id       <- _requireAuthId
  board_request <- requireJsonBody
  errorOrJSON boardToResponse $ updateBoardM user_id board_id board_request



deleteBoardR :: BoardId -> Handler Value
deleteBoardR board_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteBoardM user_id board_id



getBoardStatsR :: Handler Value
getBoardStatsR = notFound



getBoardStatR :: BoardId -> Handler Value
getBoardStatR board_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getBoardStatM user_id board_id








--
-- Model/Function
--

boardRequestToBoard :: UserId -> BoardRequest -> Board
boardRequestToBoard user_id BoardRequest{..} = Board {
  boardUserId             = user_id,
  boardName               = toSafeUrl boardRequestDisplayName,
  boardDisplayName        = boardRequestDisplayName,
  boardDescription        = boardRequestDescription,
  boardIsAnonymous        = boardRequestIsAnonymous,
  boardCanCreateBoards = boardRequestCanCreateBoards,
  boardCanCreateThreads   = boardRequestCanCreateThreads,
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
  boardResponseName               = boardName,
  boardResponseDisplayName        = boardDisplayName,
  boardResponseDescription        = boardDescription,
  -- TODO FIXME
  boardResponseBoardType          = FixMe,
  boardResponseVisibility         = Public,
  boardResponseIsAnonymous        = boardIsAnonymous,
  boardResponseCanCreateBoards = boardCanCreateBoards,
  boardResponseCanCreateThreads   = boardCanCreateThreads,
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

getBoardsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Board]
getBoardsM m_sp user_id = do

  selectListDbE m_sp [BoardActive ==. True] [] BoardId



getBoardMH :: Maybe StandardParams -> UserId -> Text -> HandlerErrorEff (Entity Board)
getBoardMH m_sp _ board_name = do

      selectFirstDbE [BoardName ==. board_name, BoardActive ==. True] []



getWithBoardM :: Bool -> UserId -> BoardId -> HandlerErrorEff (Maybe (Entity Board))
getWithBoardM False _ _             = rightA Nothing
getWithBoardM True user_id board_id = fmap Just <$> getBoardM user_id board_id



insertBoardM :: Maybe StandardParams -> UserId -> BoardRequest -> HandlerErrorEff (Entity Board)
insertBoardM m_sp user_id board_request = do

  runEitherT $ do
    -- TODO FIXME: perms
    -- mustT $ mustBe_OwnerOf_ForumIdM user_id forum_id
    sanitized_board_request <- mustT $ isValidAppM $ validateBoardRequest board_request
    ts                      <- lift timestampH'
    mustT $ insertEntityDbE $ (boardRequestToBoard user_id sanitized_board_request) { boardCreatedAt = Just ts }



updateBoardM :: UserId -> BoardId -> BoardRequest -> HandlerErrorEff (Entity Board)
updateBoardM user_id board_id board_request = do

  runEitherT $ do
    -- TODO FIXME: perms
    -- mustT $ mustBe_OwnerOf_BoardIdM user_id board_id
    sanitized_board_request <- mustT $ isValidAppM $ validateBoardRequest board_request
    ts                      <- lift timestampH'

    let
      Board{..} = (boardRequestToBoard user_id sanitized_board_request) { boardModifiedAt = Just ts }

    mustT $ updateWhereDbE
      [BoardId ==. board_id, BoardActive ==. True]
      [BoardModifiedAt         =. boardModifiedAt
      ,BoardActivityAt         =. Just ts
      ,BoardName               =. boardName
      ,BoardDisplayName        =. boardDisplayName
      ,BoardDescription        =. boardDescription
      ,BoardIsAnonymous        =. boardIsAnonymous
      ,BoardCanCreateBoards =. boardCanCreateBoards
      ,BoardCanCreateThreads   =. boardCanCreateThreads
      ,BoardIcon               =. boardIcon
      ,BoardTags               =. boardTags
      ,BoardGuard             +=. 1
      ]

    mustT $ selectFirstDbE [BoardUserId ==. user_id, BoardId ==. board_id, BoardActive ==. True] []



deleteBoardM :: UserId -> BoardId -> HandlerErrorEff ()
deleteBoardM user_id board_id = do
  runEitherT $ do
    -- TODO FIXME: perms
    -- mustT $ mustBe_OwnerOf_BoardIdM user_id board_id
    mustT $ deleteWhereDbE [BoardId ==. board_id, BoardActive ==. True]





getBoardStatsM :: Maybe StandardParams -> UserId -> HandlerErrorEff Value
getBoardStatsM _ _ = leftA Error_NotImplemented



getBoardStatM :: UserId -> BoardId -> HandlerErrorEff BoardStatResponse
getBoardStatM _ board_id = do

  num_threads      <- countDb [ThreadBoardId ==. board_id, ThreadActive ==. True]
  num_thread_posts <- countDb [ThreadPostBoardId ==. board_id, ThreadPostActive ==. True]

  rightA $ BoardStatResponse {
    boardStatResponseBoardId     = keyToInt64 board_id,
    boardStatResponseThreads     = fromIntegral num_threads,
    boardStatResponseThreadPosts = fromIntegral num_thread_posts,
    -- TODO FIXME: views
    boardStatResponseViews       = 0
  }
