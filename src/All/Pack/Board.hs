{-# LANGUAGE RecordWildCards #-}

module All.Pack.Board (
  -- Handler
  getBoardPacksR,
  getBoardPackR,
  getBoardPackH,

  -- Model
  getBoardPacksM,
  getBoardPackM,
  getBoardPackMH,
) where



import           All.Board
import           All.Prelude
import           All.Thread
import           All.ThreadPost
import           All.User



--
-- Handler
--

getBoardPacksR :: HandlerEff Value
getBoardPacksR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getBoardPacksM user_id



getBoardPackR :: BoardId -> HandlerEff Value
getBoardPackR board_id = do
  user_id <- _requireAuthId
  toJSON <$> getBoardPackM user_id board_id



getBoardPackH :: Text -> HandlerEff Value
getBoardPackH board_name = do
  user_id <- _requireAuthId
  toJSON <$> getBoardPackMH user_id board_name








--
-- Model
--

getBoardPacksM :: UserId -> HandlerEff BoardPackResponses
getBoardPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spForumId of

    Just forum_id -> getBoardPacks_ByForumIdM user_id forum_id sp
    _             -> notFound



getBoardPackM :: UserId -> BoardId -> HandlerEff BoardPackResponse
getBoardPackM user_id board_id = do

  board         <- getBoardM user_id board_id
  getBoardPack_ByBoardM user_id board



getBoardPackMH :: UserId -> Text -> HandlerEff BoardPackResponse
getBoardPackMH user_id board_name = do

  board         <- getBoardMH user_id board_name
  getBoardPack_ByBoardM user_id board



getBoardPack_ByBoardM :: UserId -> Entity Board -> HandlerEff BoardPackResponse
getBoardPack_ByBoardM user_id board@(Entity board_id Board{..}) = do

  let sp = defaultStandardParams {
      spSortOrder = Just SortOrderBy_Dsc,
      spOrder = Just OrderBy_ActivityAt,
      spLimit = Just 1
    }

  board_stats   <- getBoardStatM user_id board_id
  mthreads      <- getThreads_ByBoardIdM user_id board_id sp
  mthread_posts <- case (headMay mthreads) of
    Nothing -> pure []
    Just (Entity thread_id _) -> getThreadPosts_ByThreadIdM user_id thread_id sp
  muser         <- case (headMay mthread_posts) of
    Nothing -> pure Nothing
    Just (Entity _ ThreadPost{..}) -> Just <$> getUserM user_id threadPostUserId

  return $ BoardPackResponse {
    boardPackResponseBoard                = boardToResponse board,
    boardPackResponseBoardId              = keyToInt64 board_id,
    boardPackResponseStat                 = board_stats,
    boardPackResponseLatestThread         = fmap threadToResponse $ headMay mthreads,
    boardPackResponseLatestThreadPost     = fmap threadPostToResponse $ headMay mthread_posts,
    boardPackResponseLatestThreadPostUser = fmap userToSanitizedResponse muser,
    boardPackResponseLike                 = Nothing,
    boardPackResponseStar                 = Nothing,
    boardPackResponseWithOrganization     = Nothing,
    boardPackResponseWithForum            = Nothing,
    boardPackResponsePermissions          = emptyPermissions
  }



getBoardPacks_ByForumIdM :: UserId -> ForumId -> StandardParams -> HandlerEff BoardPackResponses
getBoardPacks_ByForumIdM user_id forum_id sp = do

  boards_keys <- getBoards_ByForumId_KeysM user_id forum_id sp
  boards_packs <- mapM (\key -> getBoardPackM user_id key) boards_keys
  return $ BoardPackResponses {
    boardPackResponses = boards_packs
  }
