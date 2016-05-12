module Handler.ThreadPost (
  getThreadPostsR,
  postThreadPostR0,
  getThreadPostR,
  putThreadPostR,
  deleteThreadPostR,

  getCountThreadPostsR,

  getThreadPostStatsR,
  getThreadPostStatR,

  getThreadPostPacksR,
  getThreadPostPackR
) where



import           Handler.Prelude
import           Model.ThreadPost
import           Model.Pack.ThreadPost



getThreadPostsR :: Handler Value
getThreadPostsR = do
  user_id <- requireAuthId
  (toJSON . threadPostsToResponses) <$> getThreadPostsM user_id



postThreadPostR0 :: Handler Value
postThreadPostR0 = do

  user_id <- requireAuthId

  sp <- lookupStandardParams

  case (spThreadId sp, spThreadPostId sp) of

    (Nothing, Nothing) -> permissionDenied "Must supply a thread_id or parent_id"

    _ -> do
      threadPost_request <- requireJsonBody :: Handler ThreadPostRequest
      (toJSON . threadPostToResponse) <$> insertThreadPostM user_id (spThreadId sp) (spThreadPostId sp) threadPost_request



getThreadPostR :: ThreadPostId -> Handler Value
getThreadPostR threadPost_id = do
  user_id <- requireAuthId
  (toJSON . threadPostToResponse) <$> getThreadPostM user_id threadPost_id



putThreadPostR :: ThreadPostId -> Handler Value
putThreadPostR threadPost_id = do
  user_id <- requireAuthId
  threadPost_request <- requireJsonBody
  (toJSON . threadPostToResponse) <$> updateThreadPostM user_id threadPost_id threadPost_request



deleteThreadPostR :: ThreadPostId -> Handler Value
deleteThreadPostR threadPost_id = do
  user_id <- requireAuthId
  void $ deleteThreadPostM user_id threadPost_id
  sendResponseStatus status200 ("DELETED" :: Text)



getCountThreadPostsR :: Handler Value
getCountThreadPostsR = do
  user_id <- requireAuthId
  toJSON <$> countThreadPostsM user_id



getThreadPostStatsR :: Handler Value
getThreadPostStatsR = do
  user_id <- requireAuthId
  toJSON <$> getThreadPostStatsM user_id



getThreadPostStatR :: ThreadPostId -> Handler Value
getThreadPostStatR thread_post_id = do
  user_id <- requireAuthId
  toJSON <$> getThreadPostStatM user_id thread_post_id



getThreadPostPacksR :: Handler Value
getThreadPostPacksR = do
  user_id <- requireAuthId
  toJSON <$> getThreadPostPacksM user_id



getThreadPostPackR :: ThreadPostId -> Handler Value
getThreadPostPackR thread_post_id = do
  user_id <- requireAuthId
  toJSON <$> getThreadPostPackM user_id thread_post_id
