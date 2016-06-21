module Model.Pack.Board (
  getBoardPacksM,
  getBoardPackM,
  getBoardPackMH,
) where



import           Model.Prelude
import           Model.User.Function
import           Model.User.Internal2
import           Model.Board.Function
import           Model.Board.Internal
import           Model.Thread.Function
import           Model.Thread.Internal
import           Model.ThreadPost.Function
import           Model.ThreadPost.Internal



getBoardPacksM :: UserId -> Handler BoardPackResponses
getBoardPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spForumId of

    Just forum_id -> getBoardPacks_ByForumIdM user_id forum_id sp
    _             -> notFound



getBoardPackM :: UserId -> BoardId -> Handler BoardPackResponse
getBoardPackM user_id board_id = do

  board         <- getBoardM user_id board_id
  getBoardPack_ByBoardM user_id board



getBoardPackMH :: UserId -> Text -> Handler BoardPackResponse
getBoardPackMH user_id board_name = do

  board         <- getBoardMH user_id board_name
  getBoardPack_ByBoardM user_id board



getBoardPack_ByBoardM :: UserId -> Entity Board -> Handler BoardPackResponse
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
    boardPackResponseIsOwner              = False
  }



getBoardPacks_ByForumIdM :: UserId -> ForumId -> StandardParams -> Handler BoardPackResponses
getBoardPacks_ByForumIdM user_id forum_id sp = do

  boards_keys <- getBoards_ByForumId_KeysM user_id forum_id sp
  boards_packs <- mapM (\key -> getBoardPackM user_id key) boards_keys
  return $ BoardPackResponses {
    boardPackResponses = boards_packs
  }
