module Handler.ThreadPostLike (
  getThreadPostLikesR,
  postThreadPostLikeR0,
  getThreadPostLikeR,
  putThreadPostLikeR,
  deleteThreadPostLikeR
) where



import           Handler.Prelude
import           Model.ThreadPostLike



getThreadPostLikesR :: Handler Value
getThreadPostLikesR = do
  user_id <- requireAuthId
  (toJSON . threadPostLikesToResponses) <$> getThreadPostLikesM user_id



postThreadPostLikeR0 :: Handler Value
postThreadPostLikeR0 = do

  user_id <- requireAuthId

  sp <- lookupStandardParams

  case (spThreadPostId sp) of

    Nothing -> permissionDenied "Must supply a thread_post_id"
    (Just thread_post_id) -> do
      like_request <- requireJsonBody :: Handler ThreadPostLikeRequest
      (toJSON . threadPostLikeToResponse) <$> insertThreadPostLikeM user_id thread_post_id like_request




getThreadPostLikeR :: ThreadPostLikeId -> Handler Value
getThreadPostLikeR thread_post_like_id = do
  user_id <- requireAuthId
  (toJSON . threadPostLikeToResponse) <$> getThreadPostLikeM user_id thread_post_like_id



putThreadPostLikeR :: ThreadPostLikeId -> Handler Value
putThreadPostLikeR thread_post_like_id = do
  user_id <- requireAuthId
  like_request <- requireJsonBody :: Handler ThreadPostLikeRequest
  (toJSON . threadPostLikeToResponse) <$> updateThreadPostLikeM user_id thread_post_like_id like_request



deleteThreadPostLikeR :: ThreadPostLikeId -> Handler Value
deleteThreadPostLikeR thread_post_like_id = do
  user_id <- requireAuthId
  void $ deleteThreadPostLikeM user_id thread_post_like_id
  sendResponseStatus status200 ("DELETED" :: Text)
