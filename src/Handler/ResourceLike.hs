module Handler.ResourceLike (
  getResourceLikesR,
  postResourceLikeR0,
  getResourceLikeR,
  putResourceLikeR,
  deleteResourceLikeR
) where



import           Handler.Prelude
import           Model.ResourceLike



getResourceLikesR :: Handler Value
getResourceLikesR = do
  user_id <- requireAuthId
  (toJSON . resourceLikesToResponses) <$> getResourceLikesM user_id



postResourceLikeR0 :: Handler Value
postResourceLikeR0 = do

  user_id <- requireAuthId

  sp <- lookupStandardParams

  case (spResourceId sp) of

    Nothing -> permissionDenied "Must supply a thread_post_id"
    (Just thread_post_id) -> do
      like_request <- requireJsonBody :: Handler ResourceLikeRequest
      (toJSON . resourceLikeToResponse) <$> insertResourceLikeM user_id thread_post_id like_request




getResourceLikeR :: ResourceLikeId -> Handler Value
getResourceLikeR thread_post_like_id = do
  user_id <- requireAuthId
  (toJSON . resourceLikeToResponse) <$> getResourceLikeM user_id thread_post_like_id



putResourceLikeR :: ResourceLikeId -> Handler Value
putResourceLikeR thread_post_like_id = do
  user_id <- requireAuthId
  like_request <- requireJsonBody :: Handler ResourceLikeRequest
  (toJSON . resourceLikeToResponse) <$> updateResourceLikeM user_id thread_post_like_id like_request



deleteResourceLikeR :: ResourceLikeId -> Handler Value
deleteResourceLikeR thread_post_like_id = do
  user_id <- requireAuthId
  void $ deleteResourceLikeM user_id thread_post_like_id
  sendResponseStatus status200 ("DELETED" :: Text)
