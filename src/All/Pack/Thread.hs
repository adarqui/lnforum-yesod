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

getThreadPacksR :: HandlerEff Value
getThreadPacksR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getThreadPacksM user_id



getThreadPackR :: ThreadId -> HandlerEff Value
getThreadPackR thread_id = do
  user_id <- _requireAuthId
  toJSON <$> getThreadPackM user_id thread_id



getThreadPackH :: Text -> HandlerEff Value
getThreadPackH thread_name = do
  user_id <- _requireAuthId
  toJSON <$> getThreadPackMH user_id thread_name







--
-- Model
--

getThreadPacksM :: UserId -> HandlerEff ThreadPackResponses
getThreadPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spBoardId of

    Just board_id -> getThreadPacks_ByBoardIdM user_id board_id sp
    _             -> notFound



getThreadPackM :: UserId -> ThreadId -> HandlerEff ThreadPackResponse
getThreadPackM user_id thread_id = do

  sp <- lookupStandardParams

  thread <- getThreadM user_id thread_id
  getThreadPack_ByThreadM user_id thread (sp { spLimit = Just 1 })



getThreadPackMH :: UserId -> Text -> HandlerEff ThreadPackResponse
getThreadPackMH user_id thread_name = do

  sp <- lookupStandardParams

  thread <- getThreadMH user_id thread_name
  getThreadPack_ByThreadM user_id thread (sp { spLimit = Just 1 })




getThreadPacks_ByBoardIdM :: UserId -> BoardId -> StandardParams -> HandlerEff ThreadPackResponses
getThreadPacks_ByBoardIdM user_id board_id sp = do

  threads_keys <- getThreads_ByBoardId_KeysM user_id board_id sp
  threads_packs <- mapM (\key -> getThreadPackM user_id key) threads_keys
  return $ ThreadPackResponses {
    threadPackResponses = threads_packs
  }



getThreadPack_ByThreadM :: UserId -> Entity Thread -> StandardParams -> HandlerEff ThreadPackResponse
getThreadPack_ByThreadM user_id thread@(Entity thread_id Thread{..}) sp = do

  thread_user   <- getUserM user_id threadUserId
  thread_stats  <- getThreadStatM user_id thread_id
  mthread_posts <- getThreadPosts_ByThreadIdM user_id thread_id sp
  muser         <- case (headMay mthread_posts) of
    Nothing -> pure Nothing
    Just (Entity _ ThreadPost{..}) -> Just <$> getUserM user_id threadPostUserId

  return $ ThreadPackResponse {
    threadPackResponseThread               = threadToResponse thread,
    threadPackResponseThreadId             = keyToInt64 thread_id,
    threadPackResponseUser                 = userToSanitizedResponse thread_user,
    threadPackResponseUserId               = entityKeyToInt64 thread_user,
    threadPackResponseStat                 = thread_stats,
    threadPackResponseLike                 = Nothing,
    threadPackResponseStar                 = Nothing,
    threadPackResponseLatestThreadPost     = fmap threadPostToResponse $ headMay mthread_posts,
    threadPackResponseLatestThreadPostUser = fmap userToSanitizedResponse muser,
    threadPackResponseWithOrganization     = Nothing,
    threadPackResponseWithForum            = Nothing,
    threadPackResponseWithBoard            = Nothing,
    threadPackResponsePermissions          = emptyPermissions
  }
