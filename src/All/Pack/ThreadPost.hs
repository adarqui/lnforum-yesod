{-# LANGUAGE RecordWildCards #-}

module All.Pack.ThreadPost (
  -- Handler
  getThreadPostPacksR,
  getThreadPostPackR,

  -- Model
  getThreadPostPacksM,
  getThreadPostPackM
) where



import           All.Like
import           All.Prelude
import           All.Star
import           All.ThreadPost
import           All.User

import Control.Monad.Trans.State



--
-- Handler
--

getThreadPostPacksR :: Handler Value
getThreadPostPacksR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getThreadPostPacksM user_id



getThreadPostPackR :: ThreadPostId -> Handler Value
getThreadPostPackR thread_post_id = run $ do
  user_id <- _requireAuthId
  toJSON <$> getThreadPostPackM user_id thread_post_id






--
-- Model
--
getThreadPostPacksM :: UserId -> HandlerEff ThreadPostPackResponses
getThreadPostPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spForumId, spThreadId, spThreadPostId) of

    (Just forum_id, _, _)       -> getThreadPostPacks_ByForumIdM user_id forum_id sp

    (_, Just thread_id, _)      -> getThreadPostPacks_ByThreadIdM user_id thread_id sp

    (_, _, Just thread_post_id) -> getThreadPostPacks_ByThreadPostIdM user_id thread_post_id sp



getThreadPostPacks_ByForumIdM :: UserId -> ForumId -> StandardParams -> HandlerEff ThreadPostPackResponses
getThreadPostPacks_ByForumIdM user_id forum_id sp = do

  thread_posts <- getThreadPosts_ByForumIdM user_id forum_id sp
  thread_post_packs <- mapM (\thread_post -> getThreadPostPack_ByThreadPostM user_id thread_post sp) thread_posts

  return $ ThreadPostPackResponses {
    threadPostPackResponses = thread_post_packs
  }



getThreadPostPacks_ByThreadIdM :: UserId -> ThreadId -> StandardParams -> HandlerEff ThreadPostPackResponses
getThreadPostPacks_ByThreadIdM user_id thread_id sp = do

  thread_posts      <- getThreadPosts_ByThreadIdM user_id thread_id sp
  thread_post_packs <- mapM (\thread_post -> getThreadPostPack_ByThreadPostM user_id thread_post sp) thread_posts

  return $ ThreadPostPackResponses {
    threadPostPackResponses = thread_post_packs
  }



getThreadPostPacks_ByThreadPostIdM :: UserId -> ThreadPostId -> StandardParams -> HandlerEff ThreadPostPackResponses
getThreadPostPacks_ByThreadPostIdM user_id thread_post_id sp = do

  thread_posts      <- getThreadPosts_ByThreadPostIdM user_id thread_post_id sp
  thread_post_packs <- mapM (\thread_post -> getThreadPostPack_ByThreadPostM user_id thread_post sp) thread_posts

  return $ ThreadPostPackResponses {
    threadPostPackResponses = thread_post_packs
  }



getThreadPostPackM :: UserId -> ThreadPostId -> HandlerEff ThreadPostPackResponse
getThreadPostPackM user_id thread_post_id = do

  sp <- lookupStandardParams

  thread_post <- getThreadPostM user_id thread_post_id
  getThreadPostPack_ByThreadPostM user_id thread_post (sp { spLimit = Just 1 })



getThreadPostPack_ByThreadPostM :: UserId -> Entity ThreadPost -> StandardParams -> HandlerEff ThreadPostPackResponse
getThreadPostPack_ByThreadPostM user_id thread_post@(Entity thread_post_id ThreadPost{..}) StandardParams{..} = do

  thread_post_user <- getUserM user_id threadPostUserId
  thread_post_stat <- getThreadPostStatM user_id thread_post_id
  thread_post_like <- getLike_ByThreadPostIdM user_id thread_post_id

--  thread_post_star <- getThreadPostStar_ByThreadPostM user_id thread_post

  -- TODO FIXME: this needs to be a function argument
  org <- (if spWithOrganization
             then pure Nothing
             else pure Nothing)

  user_perms_by_thread_post <- userPermissions_ByThreadPostIdM user_id (entityKey thread_post)

  return $ ThreadPostPackResponse {
    threadPostPackResponseThreadPost       = threadPostToResponse thread_post,
    threadPostPackResponseThreadPostId     = keyToInt64 thread_post_id,
    threadPostPackResponseUser             = userToSanitizedResponse thread_post_user,
    threadPostPackResponseUserId           = entityKeyToInt64 thread_post_user,
    threadPostPackResponseStat             = thread_post_stat,
    threadPostPackResponseLike             = fmap likeToResponse thread_post_like,
    threadPostPackResponseStar             = Nothing,
    threadPostPackResponseWithOrganization = org,
    threadPostPackResponseWithForum        = Nothing,
    threadPostPackResponseWithBoard        = Nothing,
    threadPostPackResponseWithThread       = Nothing,
    threadPostPackResponsePermissions      = user_perms_by_thread_post
  }
