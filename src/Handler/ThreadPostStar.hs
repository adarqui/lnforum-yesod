module Handler.ThreadPostStar (
  getThreadPostStarsR,
  postThreadPostStarR0,
  getThreadPostStarR,
  putThreadPostStarR,
  deleteThreadPostStarR
) where



import           Handler.Prelude
import           Model.ThreadPostStar



getThreadPostStarsR :: Handler Value
getThreadPostStarsR = do
  user_id <- requireAuthId
  (toJSON . threadPostStarsToResponses) <$> getThreadPostStarsM user_id



postThreadPostStarR0 :: Handler Value
postThreadPostStarR0 = do

  user_id <- requireAuthId

  sp <- lookupStandardParams

  case (spThreadPostId sp) of

    Nothing -> permissionDenied "Must supply a thread_post_id"
    (Just thread_post_id) -> do
      star_request <- requireJsonBody :: Handler ThreadPostStarRequest
      (toJSON . threadPostStarToResponse) <$> insertThreadPostStarM user_id thread_post_id star_request




getThreadPostStarR :: ThreadPostStarId -> Handler Value
getThreadPostStarR thread_post_star_id = do
  user_id <- requireAuthId
  (toJSON . threadPostStarToResponse) <$> getThreadPostStarM user_id thread_post_star_id



putThreadPostStarR :: ThreadPostStarId -> Handler Value
putThreadPostStarR thread_post_star_id = do
  user_id <- requireAuthId
  star_request <- requireJsonBody :: Handler ThreadPostStarRequest
  (toJSON . threadPostStarToResponse) <$> updateThreadPostStarM user_id thread_post_star_id star_request



deleteThreadPostStarR :: ThreadPostStarId -> Handler Value
deleteThreadPostStarR thread_post_star_id = do
  user_id <- requireAuthId
  void $ deleteThreadPostStarM user_id thread_post_star_id
  return $ toJSON ()
--  sendResponseStatus status200 ("DELETED" :: Text)
