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

getThreadPacksM :: Maybe StandardParams -> UserId -> HandlerEff (ErrorEff ThreadPackResponses)
getThreadPacksM m_sp user_id = do

  case (lookupSpMay m_sp spBoardId) of

    Just board_id -> getThreadPacks_ByBoardIdM m_sp user_id board_id
    _             -> left Error_NotImplemented



getThreadPackM :: Maybe StandardParams -> UserId -> ThreadId -> HandlerEff (ErrorEff ThreadPackResponse)
getThreadPackM m_sp user_id thread_id = do

  thread <- getThreadM user_id thread_id
  getThreadPack_ByThreadM m_sp user_id thread -- (sp { spLimit = Just 1 })



getThreadPackMH :: Maybe StandardParams -> UserId -> Text -> HandlerEff (ErrorEff ThreadPackResponse)
getThreadPackMH m_sp user_id thread_name = do

  thread <- getThreadMH user_id thread_name
  getThreadPack_ByThreadM m_sp user_id thread -- (sp { spLimit = Just 1 })




getThreadPacks_ByBoardIdM :: Maybe StandardParams -> UserId -> BoardId -> HandlerEff (ErrorEff ThreadPackResponses)
getThreadPacks_ByBoardIdM m_sp user_id board_id sp = do

  threads_keys <- getThreads_ByBoardId_KeysM m_sp user_id board_id
  threads_packs <- rights <$> mapM (\key -> getThreadPackM user_id key) threads_keys
  right $ ThreadPackResponses {
    threadPackResponses = threads_packs
  }



getThreadPack_ByThreadM :: Maybe StandardParams -> UserId -> Entity Thread -> HandlerEff (ErrorEff ThreadPackResponse)
getThreadPack_ByThreadM m_sp user_id thread@(Entity thread_id Thread{..}) = do

  thread_user    <- getUserM user_id threadUserId
  thread_stats   <- getThreadStatM user_id thread_id
  m_thread_posts <- getThreadPosts_ByThreadIdM m_sp user_id thread_id
  m_user          <- case (headMay m_thread_posts) of
    Nothing -> pure Nothing
    Just (Entity _ ThreadPost{..}) -> Just <$> getUserM user_id threadPostUserId

  user_perms_by_thread <- userPermissions_ByThreadIdM user_id (entityKey thread)

  right $ ThreadPackResponse {
    threadPackResponseThread               = threadToResponse thread,
    threadPackResponseThreadId             = keyToInt64 thread_id,
    threadPackResponseUser                 = userToSanitizedResponse thread_user,
    threadPackResponseUserId               = entityKeyToInt64 thread_user,
    threadPackResponseStat                 = thread_stats,
    threadPackResponseLike                 = Nothing,
    threadPackResponseStar                 = Nothing,
    threadPackResponseLatestThreadPost     = fmap threadPostToResponse $ headMay m_thread_posts,
    threadPackResponseLatestThreadPostUser = fmap userToSanitizedResponse m_user,
    threadPackResponseWithOrganization     = Nothing,
    threadPackResponseWithForum            = Nothing,
    threadPackResponseWithBoard            = Nothing,
    threadPackResponsePermissions          = user_perms_by_thread
  }
