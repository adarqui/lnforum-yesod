{-# LANGUAGE RecordWildCards #-}

module Handler.Like (
  getLikesR,
  postLikeR0,
  getLikeR,
  putLikeR,
  deleteLikeR
) where



import           Handler.Prelude
import           Model.Like



{-
getThreadPostLikes :: Handler Value
getThreadPostLikes = getLikesR Ent_ThreadPost

postThreadPostLikesR :: Handler Value
postThreadPostLikesR = postLikesR Ent_ThreadPost
-}



getLikesR :: Handler Value
getLikesR = do
  user_id <- requireAuthId
  (toJSON . likesToResponses) <$> getLikesM user_id



postLikeR0 :: Handler Value
postLikeR0 = do

  sp <- lookupStandardParams

  case (lookupLikeEntity sp) of

    Nothing            -> permissionDenied "Must supply a entity information"

    Just (ent, ent_id) -> do

      user_id <- requireAuthId
      like_request <- requireJsonBody
      (toJSON . likeToResponse) <$> insertLikeM user_id ent ent_id like_request



getLikeR :: LikeId -> Handler Value
getLikeR like_id = do
  user_id <- requireAuthId
  (toJSON . likeToResponse) <$> getLikeM user_id like_id



putLikeR :: LikeId -> Handler Value
putLikeR like_id = do
  user_id <- requireAuthId
  like_request <- requireJsonBody
  (toJSON . likeToResponse) <$> updateLikeM user_id like_id like_request



deleteLikeR :: LikeId -> Handler Value
deleteLikeR like_id = do
  user_id <- requireAuthId
  void $ deleteLikeM user_id like_id
  sendResponseStatus status200 ("DELETED" :: Text)
