{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.Board (
  -- LN.Handler
  getBoardPacksR,
  getBoardPackR,
  getBoardPackH,

  -- LN.Model
  getBoardPacksM,
  getBoardPackM,
  getBoardPackMH,
) where



import           LN.All.Board
import           LN.All.Prelude
import           LN.All.Thread
import           LN.All.ThreadPost
import           LN.All.User



--
-- LN.Handler
--

getBoardPacksR :: Handler Value
getBoardPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getBoardPacksM (pure sp) user_id



getBoardPackR :: BoardId -> Handler Value
getBoardPackR board_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getBoardPackM user_id board_id



getBoardPackH :: Text -> Handler Value
getBoardPackH board_name = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getBoardPackMH (pure sp) user_id board_name








--
-- LN.Model
--

getBoardPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff BoardPackResponses
getBoardPacksM m_sp user_id = do

  case (lookupSpMay m_sp spForumId) of

    Just forum_id -> getBoardPacks_ByForumIdM m_sp user_id forum_id
    _             -> left $ LN.Error_InvalidArguments "forum_id"



getBoardPackM :: UserId -> BoardId -> HandlerErrorEff BoardPackResponse
getBoardPackM user_id board_id = do

  e_board <- getBoardM user_id board_id
  rehtie e_board left $ getBoardPack_ByBoardM user_id



getBoardPackMH :: Maybe StandardParams -> UserId -> Text -> HandlerErrorEff BoardPackResponse
getBoardPackMH m_sp user_id board_name = do

  e_board <- getBoardMH m_sp user_id board_name
  rehtie e_board left $ getBoardPack_ByBoardM user_id



getBoardPack_ByBoardM :: UserId -> Entity Board -> HandlerErrorEff BoardPackResponse
getBoardPack_ByBoardM user_id board@(Entity board_id Board{..}) = do

  let sp = defaultStandardParams {
      spSortOrder = Just SortOrderBy_Dsc,
      spOrder = Just OrderBy_ActivityAt,
      spLimit = Just 1
    }

  lr <- runEitherT $ do

    board_stats         <- isT $ getBoardStatM user_id board_id
    threads             <- isT $ getThreads_ByBoardIdM (Just sp) user_id board_id

    m_thread_posts      <- case (headMay threads) of
      Nothing                   -> pure []
      Just (Entity thread_id _) -> isT $ getThreadPosts_ByThreadIdM (Just sp) user_id thread_id

    m_user              <- case (headMay m_thread_posts) of
      Nothing                        -> pure Nothing
      Just (Entity _ ThreadPost{..}) -> Just <$> (isT $ getUserM user_id threadPostUserId)

    user_perms_by_board <- lift $ userPermissions_ByBoardIdM user_id (entityKey board)

    pure (board_stats
         ,threads
         ,m_thread_posts
         ,m_user
         ,user_perms_by_board)

  rehtie lr left $ \(board_stats, threads, m_thread_posts, m_user, user_perms_by_board) -> do
    right $ BoardPackResponse {
      boardPackResponseBoard                = boardToResponse board,
      boardPackResponseBoardId              = keyToInt64 board_id,
      boardPackResponseStat                 = board_stats,
      boardPackResponseLatestThread         = fmap threadToResponse $ headMay threads,
      boardPackResponseLatestThreadPost     = fmap threadPostToResponse $ headMay m_thread_posts,
      boardPackResponseLatestThreadPostUser = fmap userToSanitizedResponse m_user,
      boardPackResponseLike                 = Nothing,
      boardPackResponseStar                 = Nothing,
      boardPackResponseWithOrganization     = Nothing,
      boardPackResponseWithForum            = Nothing,
      boardPackResponsePermissions          = user_perms_by_board
    }



getBoardPacks_ByForumIdM :: Maybe StandardParams -> UserId -> ForumId -> HandlerErrorEff BoardPackResponses
getBoardPacks_ByForumIdM m_sp user_id forum_id = do

  e_board_keys <- getBoards_ByForumId_KeysM m_sp user_id forum_id
  rehtie e_board_keys left $ \board_keys -> do
    board_packs <- rights <$> mapM (\key -> getBoardPackM user_id key) board_keys
    right $ BoardPackResponses {
      boardPackResponses = board_packs
    }
