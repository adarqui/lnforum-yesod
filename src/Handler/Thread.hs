module Handler.Thread (
  getThreadsR,
  postThreadR0,
  getThreadR,
  getThreadH,
  putThreadR,
  deleteThreadR,

  getCountThreadsR,

  getThreadStatsR,
  getThreadStatR,

  getThreadPacksR,
  getThreadPackR,
  getThreadPackH,
) where



import           Handler.Prelude
import           Model.Thread



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
