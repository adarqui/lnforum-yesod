{-# LANGUAGE RecordWildCards #-}

module Model.Pack.ThreadPost (
  getThreadPostPacksM,
  getThreadPostPackM
) where



import           Model.Prelude
import           Model.ThreadPost.Function
import           Model.ThreadPost.Internal
import           Model.ThreadPostLike.Function
import           Model.ThreadPostLike.Internal
import           Model.ThreadPostStar.Function
import           Model.ThreadPostStar.Internal
import           Model.User.Function
import           Model.User.Internal2



getThreadPostPacksM :: UserId -> Handler ThreadPostPackResponses
getThreadPostPacksM user_id = do

  sp <- lookupStandardParams

  thread_posts <- getThreadPostsM user_id
  thread_post_packs <- mapM (\thread_post -> getThreadPostPack_ByThreadPostM user_id thread_post sp) thread_posts

  return $ ThreadPostPackResponses {
    threadPostPackResponses = thread_post_packs
  }



getThreadPostPackM :: UserId -> ThreadPostId -> Handler ThreadPostPackResponse
getThreadPostPackM user_id thread_post_id = do

  sp <- lookupStandardParams

  thread_post <- getThreadPostM user_id thread_post_id
  getThreadPostPack_ByThreadPostM user_id thread_post (sp { spLimit = Just 1 })



getThreadPostPack_ByThreadPostM :: UserId -> Entity ThreadPost -> StandardParams -> Handler ThreadPostPackResponse
getThreadPostPack_ByThreadPostM user_id thread_post@(Entity thread_post_id ThreadPost{..}) _ = do

  thread_post_user <- getUserM user_id threadPostUserId
  thread_post_stat <- getThreadPostStatM user_id thread_post_id
  thread_post_like <- getThreadPostLike_ByThreadPostM user_id thread_post
  thread_post_star <- getThreadPostStar_ByThreadPostM user_id thread_post

  return $ ThreadPostPackResponse {
    threadPostPackResponseThreadPost = threadPostToResponse thread_post,
    threadPostPackResponseUser = userToSanitizedResponse thread_post_user,
    threadPostPackResponseStat = thread_post_stat,
    threadPostPackResponseLike = fmap threadPostLikeToResponse thread_post_like,
    threadPostPackResponseStar = fmap threadPostStarToResponse thread_post_star
  }

