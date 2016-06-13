module Model.Pack.Thread (
  getThreadPacksM,
  getThreadPackM,
  getThreadPackMH,
) where



import           Model.Prelude
import           Model.User.Function
import           Model.User.Internal2
import           Model.Thread.Function
import           Model.Thread.Internal
import           Model.ThreadPost.Function
import           Model.ThreadPost.Internal



getThreadPacksM :: UserId -> Handler ThreadPackResponses
getThreadPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spBoardId of

    Just board_id -> getThreadPacks_ByBoardIdM user_id board_id sp
    _             -> notFound



getThreadPackM :: UserId -> ThreadId -> Handler ThreadPackResponse
getThreadPackM user_id thread_id = do

  sp <- lookupStandardParams

  thread <- getThreadM user_id thread_id
  getThreadPack_ByThreadM user_id thread (sp { spLimit = Just 1 })



getThreadPackMH :: UserId -> Text -> Handler ThreadPackResponse
getThreadPackMH user_id thread_name = do

  sp <- lookupStandardParams

  thread <- getThreadMH user_id thread_name
  getThreadPack_ByThreadM user_id thread (sp { spLimit = Just 1 })




getThreadPacks_ByBoardIdM :: UserId -> BoardId -> StandardParams -> Handler ThreadPackResponses
getThreadPacks_ByBoardIdM user_id board_id sp = do

  threads_keys <- getThreads_ByBoardId_KeysM user_id board_id sp
  threads_packs <- mapM (\key -> getThreadPackM user_id key) threads_keys
  return $ ThreadPackResponses {
    threadPackResponses = threads_packs
  }



getThreadPack_ByThreadM :: UserId -> Entity Thread -> StandardParams -> Handler ThreadPackResponse
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
    threadPackResponseLatestThreadPostUser = fmap userToSanitizedResponse muser
  }
