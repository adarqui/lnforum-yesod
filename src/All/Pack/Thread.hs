{-# LANGUAGE RecordWildCards #-}

module All.Pack.Thread (
  -- Handler
  getThreadPacksR,
  getThreadPackR,
  getThreadPackH,

  -- Model
  getThreadPacksM,
  getThreadPackM,
  getThreadPackMH,
) where



import           All.Prelude
import           All.Thread
import           All.ThreadPost
import           All.User



--
-- Handler
--

getThreadPacksR :: Handler Value
getThreadPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getThreadPacksM (pure sp) user_id



getThreadPackR :: ThreadId -> Handler Value
getThreadPackR thread_id = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getThreadPackM (pure sp) user_id thread_id



getThreadPackH :: Text -> Handler Value
getThreadPackH thread_name = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getThreadPackMH (pure sp) user_id thread_name







--
-- Model
--

getThreadPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff ThreadPackResponses
getThreadPacksM m_sp user_id = do

  case (lookupSpMay m_sp spBoardId) of

    Just board_id -> getThreadPacks_ByBoardIdM m_sp user_id board_id
    _             -> left Error_NotImplemented



getThreadPackM :: Maybe StandardParams -> UserId -> ThreadId -> HandlerErrorEff ThreadPackResponse
getThreadPackM m_sp user_id thread_id = do

  e_thread <- getThreadM user_id thread_id
  rehtie e_thread left $ \thread -> do
    getThreadPack_ByThreadM m_sp user_id thread -- (sp { spLimit = Just 1 })



getThreadPackMH :: Maybe StandardParams -> UserId -> Text -> HandlerErrorEff ThreadPackResponse
getThreadPackMH m_sp user_id thread_name = do

  e_thread <- getThreadMH m_sp user_id thread_name
  rehtie e_thread left $ \thread -> do
    getThreadPack_ByThreadM m_sp user_id thread -- (sp { spLimit = Just 1 })



getThreadPacks_ByBoardIdM :: Maybe StandardParams -> UserId -> BoardId -> HandlerErrorEff ThreadPackResponses
getThreadPacks_ByBoardIdM m_sp user_id board_id = do

  e_threads_keys <- getThreads_ByBoardId_KeysM m_sp user_id board_id
  rehtie e_threads_keys left $ \threads_keys -> do
    threads_packs <- rights <$> mapM (\key -> getThreadPackM m_sp user_id key) threads_keys
    right $ ThreadPackResponses {
      threadPackResponses = threads_packs
    }



getThreadPack_ByThreadM :: Maybe StandardParams -> UserId -> Entity Thread -> HandlerErrorEff ThreadPackResponse
getThreadPack_ByThreadM m_sp user_id thread@(Entity thread_id Thread{..}) = do

  lr <- runEitherT $ do

    thread_user  <- isT $ getUserM user_id threadUserId
    thread_stats <- isT $ getThreadStatM user_id thread_id
    thread_posts <- isT $ getThreadPosts_ByThreadIdM m_sp user_id thread_id
    m_user       <- case (headMay thread_posts) of
      Nothing -> pure Nothing
      Just (Entity _ ThreadPost{..}) -> Just <$> isT $ getUserM user_id threadPostUserId

    user_perms_by_thread <- lift $ userPermissions_ByThreadIdM user_id (entityKey thread)

    pure (thread_user
         ,thread_stats
         ,thread_posts
         ,m_user
         ,user_perms_by_thread)

  rehtie lr left $
    \(thread_user, thread_stats, thread_posts, m_user, user_perms_by_thread) -> do

      right $ ThreadPackResponse {
        threadPackResponseThread               = threadToResponse thread,
        threadPackResponseThreadId             = keyToInt64 thread_id,
        threadPackResponseUser                 = userToSanitizedResponse thread_user,
        threadPackResponseUserId               = entityKeyToInt64 thread_user,
        threadPackResponseStat                 = thread_stats,
        threadPackResponseLike                 = Nothing,
        threadPackResponseStar                 = Nothing,
        threadPackResponseLatestThreadPost     = fmap threadPostToResponse $ headMay thread_posts,
        threadPackResponseLatestThreadPostUser = fmap userToSanitizedResponse m_user,
        threadPackResponseWithOrganization     = Nothing,
        threadPackResponseWithForum            = Nothing,
        threadPackResponseWithBoard            = Nothing,
        threadPackResponsePermissions          = user_perms_by_thread
      }
