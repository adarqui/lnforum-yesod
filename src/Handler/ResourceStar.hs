module Handler.ResourceStar (
  getResourceStarsR,
  postResourceStarR0,
  getResourceStarR,
  putResourceStarR,
  deleteResourceStarR
) where



import           Handler.Prelude
import           Model.ResourceStar



getResourceStarsR :: Handler Value
getResourceStarsR = do
  user_id <- requireAuthId
  (toJSON . resourceStarsToResponses) <$> getResourceStarsM user_id



postResourceStarR0 :: Handler Value
postResourceStarR0 = do

  user_id <- requireAuthId

  sp <- lookupStandardParams

  case (spResourceId sp) of

    Nothing -> permissionDenied "Must supply a thread_post_id"
    (Just thread_post_id) -> do
      star_request <- requireJsonBody :: Handler ResourceStarRequest
      (toJSON . resourceStarToResponse) <$> insertResourceStarM user_id thread_post_id star_request




getResourceStarR :: ResourceStarId -> Handler Value
getResourceStarR thread_post_star_id = do
  user_id <- requireAuthId
  (toJSON . resourceStarToResponse) <$> getResourceStarM user_id thread_post_star_id



putResourceStarR :: ResourceStarId -> Handler Value
putResourceStarR thread_post_star_id = do
  user_id <- requireAuthId
  star_request <- requireJsonBody :: Handler ResourceStarRequest
  (toJSON . resourceStarToResponse) <$> updateResourceStarM user_id thread_post_star_id star_request



deleteResourceStarR :: ResourceStarId -> Handler Value
deleteResourceStarR thread_post_star_id = do
  user_id <- requireAuthId
  void $ deleteResourceStarM user_id thread_post_star_id
  return $ toJSON ()
--  sendResponseStatus status200 ("DELETED" :: Text)
