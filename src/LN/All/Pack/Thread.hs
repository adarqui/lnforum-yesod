{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.Thread (
  -- Handler
  getThreadPacksR,
  getThreadPackR,
  getThreadPackH,

  -- Model
  getThreadPacksM,
  getThreadPackM,
  getThreadPackMH,
) where



import           LN.All.Internal
import           LN.All.Prelude
import           LN.All.Thread
import           LN.All.ThreadPost
import           LN.All.User



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
    _             -> leftA $ Error_InvalidArguments "board_id"



getThreadPackM :: Maybe StandardParams -> UserId -> ThreadId -> HandlerErrorEff ThreadPackResponse
getThreadPackM m_sp user_id thread_id = do

  e_thread <- getThreadM user_id thread_id
  rehtie e_thread leftA $ \thread -> do
    getThreadPack_ByThreadM m_sp user_id thread -- (sp { spLimit = Just 1 })



getThreadPackMH :: Maybe StandardParams -> UserId -> Text -> HandlerErrorEff ThreadPackResponse
getThreadPackMH m_sp user_id thread_name = do

  e_thread <- getThreadMH m_sp user_id thread_name
  rehtie e_thread leftA $ \thread -> do
    getThreadPack_ByThreadM m_sp user_id thread -- (sp { spLimit = Just 1 })



getThreadPacks_ByBoardIdM :: Maybe StandardParams -> UserId -> BoardId -> HandlerErrorEff ThreadPackResponses
getThreadPacks_ByBoardIdM m_sp user_id board_id = do

  e_threads <- getThreads_ByBoardIdM m_sp user_id board_id
  rehtie e_threads leftA $ \threads -> do
    thread_packs <- rights <$> forConcurrently threads (getThreadPack_ByThreadM m_sp user_id)
    rightA $ ThreadPackResponses {
      threadPackResponses = thread_packs
    }



getThreadPack_ByThreadM :: Maybe StandardParams -> UserId -> Entity Thread -> HandlerErrorEff ThreadPackResponse
getThreadPack_ByThreadM _ user_id thread@(Entity thread_id Thread{..}) = do

  let sp = defaultStandardParams {
      spSortOrder = Just SortOrderBy_Dsc,
      spOrder = Just OrderBy_CreatedAt,
      spLimit = Just 1
    }

  lr <- runEitherT $ do

    thread_user  <- mustT $ getUserM user_id threadUserId
    thread_stats <- mustT $ getThreadStatM user_id thread_id
    thread_posts <- mustT $ getThreadPosts_ByThreadIdM (Just sp) user_id thread_id

    -- If no thread posts exist, make sure latestThreadPostUser is the thread owner
    -- TODO FIXME: get rid of Maybe for latestThreadPostUser and rename it to LatestUser
    --
    m_user       <- case (headMay thread_posts) of
      Nothing                        -> pure $ Just thread_user
      Just (Entity _ ThreadPost{..}) -> Just <$> (mustT $ getUserM user_id threadPostUserId)

    -- TODO FIXME: perms
    -- user_perms_by_thread <- lift $ userPermissions_ByThreadIdM user_id (entityKey thread)
    let user_perms_by_thread = []

    pure (thread_user
         ,thread_stats
         ,thread_posts
         ,m_user
         ,user_perms_by_thread)

  rehtie lr leftA $
    \(thread_user, thread_stats, thread_posts, m_user, user_perms_by_thread) -> do

      rightA $ ThreadPackResponse {
        threadPackResponseThread               = threadToResponse thread,
        threadPackResponseThreadId             = keyToInt64 thread_id,
        threadPackResponseUser                 = userToSanitizedResponse thread_user,
        threadPackResponseUserId               = entityKeyToInt64 thread_user,
        threadPackResponseStat                 = thread_stats,
        threadPackResponseLike                 = Nothing,
        threadPackResponseLatestThreadPost     = fmap threadPostToResponse $ headMay thread_posts,
        threadPackResponseLatestThreadPostUser = fmap userToSanitizedResponse m_user,
        threadPackResponseWithBoard            = Nothing,
        threadPackResponsePermissions          = user_perms_by_thread
      }
