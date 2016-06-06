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
  boardName        = toPrettyUrl boardRequestDisplayName,
  boardDisplayName = boardRequestDisplayName,
  boardDescription = boardRequestDescription,
  boardIcon        = boardRequestIcon,
  boardTags        = boardRequestTags,
  boardActive      = True,
  boardGuard       = boardRequestGuard,
  boardCreatedAt   = Nothing,
  boardModifiedBy  = Nothing,
  boardModifiedAt  = Nothing,
  boardActivityAt  = Nothing
}



boardToResponse :: Entity Board -> BoardResponse
boardToResponse (Entity board_id Board{..}) = BoardResponse {
  boardResponseId          = keyToInt64 board_id,
  boardResponseUserId      = keyToInt64 boardUserId,
  boardResponseForumId     = keyToInt64 boardForumId,
  boardResponseParentId    = fmap keyToInt64 boardParentId,
  boardResponseName        = boardName,
  boardResponseDisplayName = boardDisplayName,
  boardResponseDescription = boardDescription,
  boardResponseIcon        = boardIcon,
  boardResponseTags        = boardTags,
  boardResponseActive      = boardActive,
  boardResponseGuard       = boardGuard,
  boardResponseCreatedAt   = boardCreatedAt,
  boardResponseModifiedBy  = fmap keyToInt64 boardModifiedBy,
  boardResponseModifiedAt  = boardModifiedAt,
  boardResponseActivityAt  = boardActivityAt
}



boardsToResponses :: [Entity Board] -> BoardResponses
boardsToResponses boards = BoardResponses {
  boardResponses = map boardToResponse boards
}
