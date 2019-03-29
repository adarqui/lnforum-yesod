{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.Board (
  -- Handler
  getBoardPacksR,
  getBoardPackR,
  getBoardPackH,

  -- Model
  getBoardPacksM,
  getBoardPackM,
  getBoardPackMH
) where



import           LN.All.Board
import LN.All.Thread
import LN.All.ThreadPost
import           LN.All.Internal
import           LN.All.Prelude
import           LN.All.User



--
-- Handler
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
-- Model
--

getBoardPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff BoardPackResponses
getBoardPacksM m_sp user_id = do

    getBoardPacks_AsyncM m_sp user_id



getBoardPackM :: UserId -> BoardId -> HandlerErrorEff BoardPackResponse
getBoardPackM user_id board_id = do

  e_board <- getBoardM user_id board_id
  rehtie e_board leftA $ getBoardPack_ByBoardM user_id



getBoardPackMH :: Maybe StandardParams -> UserId -> Text -> HandlerErrorEff BoardPackResponse
getBoardPackMH m_sp user_id board_name = do

  e_board <- getBoardMH m_sp user_id board_name
  rehtie e_board leftA $ getBoardPack_ByBoardM user_id



getBoardPack_ByBoardM :: UserId -> Entity Board -> HandlerErrorEff BoardPackResponse
getBoardPack_ByBoardM user_id board@(Entity board_id Board{..}) = do

  let sp = defaultStandardParams {
      spSortOrder = Just SortOrderBy_Dsc,
      spOrder     = Just OrderBy_ActivityAt,
      spLimit     = Just 1
    }

  lr <- runEitherT $ do

    board_stats         <- mustT $ getBoardStatM user_id board_id

    thread_posts        <- mustT $ getThreadPosts_ByBoardIdM (Just sp) user_id board_id

    m_thread            <- case (headMay thread_posts) of
                                Nothing                       -> pure Nothing
                                Just (Entity _ ThreadPost{..}) -> Just <$> (mustT $ getThreadM user_id threadPostThreadId)

    m_user              <- case (headMay thread_posts) of
                                Nothing                        -> pure Nothing
                                Just (Entity _ ThreadPost{..}) -> Just <$> (mustT $ getUserM user_id threadPostUserId)

    -- (user_perms_by_board, thread_perms_by_board) <- lift $ userPermissions_ByBoardIdM user_id (entityKey board)
    let
      user_perms_by_board = []
      thread_perms_by_board = []

    pure (board_stats
         ,thread_posts
         ,m_thread
         ,m_user
         ,user_perms_by_board
         ,thread_perms_by_board)

  rehtie lr leftA $ \(board_stats, thread_posts, m_thread, m_user, user_perms_by_board, thread_perms_by_board) -> do
    rightA $ BoardPackResponse {
      boardPackResponseBoard                = boardToResponse board,
      boardPackResponseBoardId              = keyToInt64 board_id,
      boardPackResponseStat                 = board_stats,
      boardPackResponseLatestThread         = fmap threadToResponse m_thread,
      boardPackResponseLatestThreadPost     = fmap threadPostToResponse $ headMay thread_posts,
      boardPackResponseLatestThreadPostUser = fmap userToSanitizedResponse m_user,
      boardPackResponseLike                 = Nothing,
      boardPackResponsePermissions          = user_perms_by_board,
      boardPackResponseUser = fmap userToSanitizedResponse m_user,
      boardPackResponseUserId = Nothing -- todo fixme
      -- boardPackResponseThreadPermissions    = thread_perms_by_board
    }



getBoardPacks_AsyncM :: Maybe StandardParams -> UserId -> HandlerErrorEff BoardPackResponses
getBoardPacks_AsyncM m_sp user_id = do

  e_boards <- getBoardsM m_sp user_id
  rehtie e_boards leftA $ \boards -> do
    board_packs <- rights <$> forConcurrently boards (getBoardPack_ByBoardM user_id)
    rightA $ BoardPackResponses {
      boardPackResponses = board_packs
    }
