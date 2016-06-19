module Handler.Board (
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
  getBoardPackH
) where




import           Handler.Prelude
import           Model.Board



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
