{-# LANGUAGE RecordWildCards #-}

module All.Pack.ThreadPost (
  -- Handler
  getThreadPostPacksR,
  getThreadPostPackR,

  -- Model
  getThreadPostPacksM,
  getThreadPostPackM
) where



import           All.Board
import           All.Forum
import           All.Like
import           All.Organization
import           All.Prelude
import           All.Thread
import           All.ThreadPost
import           All.User



--
-- Handler
--

getThreadPostPacksR :: Handler Value
getThreadPostPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getThreadPostPacksM (pure sp) user_id



getThreadPostPackR :: ThreadPostId -> Handler Value
getThreadPostPackR thread_post_id = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getThreadPostPackM (pure sp) user_id thread_post_id






--
-- Model
--
getThreadPostPacksM :: Maybe StandardParams -> UserId -> HandlerEff (ErrorEff ThreadPostPackResponses)
getThreadPostPacksM m_sp user_id = do

  case (lookupSpMay m_sp spForumId, lookupSpMay m_sp spThreadId, lookupSpMay m_sp spThreadPostId) of

    (Just forum_id, _, _)       -> getThreadPostPacks_ByForumIdM m_sp user_id forum_id

    (_, Just thread_id, _)      -> getThreadPostPacks_ByThreadIdM m_sp user_id thread_id

    (_, _, Just thread_post_id) -> getThreadPostPacks_ByThreadPostIdM m_sp user_id thread_post_id

    (_, _, _)                   -> left Error_NotImplemented



getThreadPostPacks_ByForumIdM :: Maybe StandardParams -> UserId -> ForumId -> HandlerEff (ErrorEff ThreadPostPackResponses)
getThreadPostPacks_ByForumIdM m_sp user_id forum_id = do

  thread_posts <- getThreadPosts_ByForumIdM m_sp user_id forum_id
  thread_post_packs <- rights <$> mapM (\thread_post -> getThreadPostPack_ByThreadPostM m_sp user_id thread_post) thread_posts

  right $ ThreadPostPackResponses {
    threadPostPackResponses = thread_post_packs
  }



getThreadPostPacks_ByThreadIdM :: Maybe StandardParams -> UserId -> ThreadId -> HandlerEff (ErrorEff ThreadPostPackResponses)
getThreadPostPacks_ByThreadIdM m_sp user_id thread_id = do

  thread_posts      <- getThreadPosts_ByThreadIdM m_sp user_id thread_id
  thread_post_packs <- rights <$> mapM (\thread_post -> getThreadPostPack_ByThreadPostM m_sp user_id thread_post) thread_posts

  right $ ThreadPostPackResponses {
    threadPostPackResponses = thread_post_packs
  }



getThreadPostPacks_ByThreadPostIdM :: Maybe StandardParams -> UserId -> ThreadPostId -> HandlerEff (ErrorEff ThreadPostPackResponses)
getThreadPostPacks_ByThreadPostIdM m_sp user_id thread_post_id = do

  thread_posts      <- getThreadPosts_ByThreadPostIdM m_sp user_id thread_post_id
  thread_post_packs <- rights <$> mapM (\thread_post -> getThreadPostPack_ByThreadPostM m_sp user_id thread_post) thread_posts

  right $ ThreadPostPackResponses {
    threadPostPackResponses = thread_post_packs
  }



getThreadPostPackM :: Maybe StandardParams -> UserId -> ThreadPostId -> HandlerEff (ErrorEff ThreadPostPackResponse)
getThreadPostPackM m_sp user_id thread_post_id = do

  e_thread_post <- getThreadPostM user_id thread_post_id
  case e_thread_post of
    Left err          -> left err
    Right thread_post -> getThreadPostPack_ByThreadPostM m_sp user_id thread_post -- defaultStandardParams { spLimit = Just 1 })



getThreadPostPack_ByThreadPostM :: Maybe StandardParams -> UserId -> Entity ThreadPost -> HandlerEff (ErrorEff ThreadPostPackResponse)
getThreadPostPack_ByThreadPostM m_sp user_id thread_post@(Entity thread_post_id ThreadPost{..}) = do

  thread_post_user   <- getUserM user_id threadPostUserId
  e_thread_post_stat <- getThreadPostStatM user_id thread_post_id
  thread_post_like   <- getLike_ByThreadPostIdM user_id thread_post_id

--  thread_post_star <- getThreadPostStar_ByThreadPostM user_id thread_post

  user_perms_by_thread_post <- userPermissions_ByThreadPostIdM user_id (entityKey thread_post)

  e_m_org  <- getWithOrganizationM (lookupSpBool m_sp spWithOrganization) user_id threadPostOrgId
  m_forum  <- getWithForumM (lookupSpBool m_sp spWithForum) user_id threadPostForumId
  m_board  <- getWithBoardM (lookupSpBool m_sp spWithBoard) user_id threadPostBoardId
  m_thread <- getWithThreadM (lookupSpBool m_sp spWithThread) user_id threadPostThreadId

  case (e_m_org, e_thread_post_stat) of
    (Right m_org, Right thread_post_stat) -> do
      right $ ThreadPostPackResponse {
        threadPostPackResponseThreadPost       = threadPostToResponse thread_post,
        threadPostPackResponseThreadPostId     = keyToInt64 thread_post_id,
        threadPostPackResponseUser             = userToSanitizedResponse thread_post_user,
        threadPostPackResponseUserId           = entityKeyToInt64 thread_post_user,
        threadPostPackResponseStat             = thread_post_stat,
        threadPostPackResponseLike             = fmap likeToResponse thread_post_like,
        threadPostPackResponseStar             = Nothing,
        threadPostPackResponseWithOrganization = fmap organizationToResponse m_org,
        threadPostPackResponseWithForum        = fmap forumToResponse m_forum,
        threadPostPackResponseWithBoard        = fmap boardToResponse m_board,
        threadPostPackResponseWithThread       = fmap threadToResponse m_thread,
        threadPostPackResponsePermissions      = user_perms_by_thread_post
      }
    _   -> left Error_Unexpected
