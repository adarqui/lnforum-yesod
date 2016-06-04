{-# LANGUAGE RecordWildCards #-}

module Model.Board.Function (
  boardRequestToBoard,
  boardToResponse,
  boardsToResponses,
) where



import           Model.Prelude



boardRequestToBoard :: UserId -> ForumId -> Maybe BoardId -> BoardRequest -> Board
boardRequestToBoard user_id forum_id m_board_id BoardRequest{..} = Board {
  boardUserId      = user_id,
  boardForumId     = forum_id,
  boardParentId    = m_board_id,
  boardName        = boardRequestName,
  boardDescription = boardRequestDescription,
  boardIcon        = boardRequestIcon,
  boardTags        = boardRequestTags,
  boardActive      = True,
  boardCreatedAt   = Nothing,
  boardModifiedBy  = Nothing,
  boardModifiedAt  = Nothing
}



boardToResponse :: Entity Board -> BoardResponse
boardToResponse (Entity board_id Board{..}) = BoardResponse {
  boardResponseId          = keyToInt64 board_id,
  boardResponseUserId      = keyToInt64 boardUserId,
  boardResponseForumId     = keyToInt64 boardForumId,
  boardResponseParentId    = fmap keyToInt64 boardParentId,
  boardResponseName        = boardName,
  boardResponseDescription = boardDescription,
  boardResponseIcon        = boardIcon,
  boardResponseTags        = boardTags,
  boardResponseCreatedAt   = boardCreatedAt,
  boardResponseModifiedBy  = fmap keyToInt64 boardModifiedBy,
  boardResponseModifiedAt  = boardModifiedAt
}



boardsToResponses :: [Entity Board] -> BoardResponses
boardsToResponses boards = BoardResponses {
  boardResponses = map boardToResponse boards
}
