module Model.Pack.Board (
  getBoardPacksM,
  getBoardPackM
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

    Just forum_id -> getBoardPacksBy_ForumIdM user_id forum_id sp
    _             -> notFound



getBoardPacksBy_ForumIdM :: UserId -> ForumId -> StandardParams -> Handler BoardPackResponses
getBoardPacksBy_ForumIdM user_id forum_id sp = do

  boards_keys <- getBoardsBy_ForumId_KeysM user_id forum_id sp
  boards_packs <- mapM (\key -> getBoardPackM user_id key) boards_keys
  return $ BoardPackResponses {
    boardPackResponses = boards_packs
  }





getBoardPackM :: UserId -> BoardId -> Handler BoardPackResponse
getBoardPackM user_id board_id = do

  let sp = defaultStandardParams {
      spSortOrder = Just SortOrderBy_Dsc,
      spOrder = Just OrderBy_ActivityAt,
      spLimit = Just 1
    }

  board <- getBoardM user_id board_id
  board_stats <- getBoardStatM user_id board_id
  mthreads <- getThreadsBy_BoardIdM user_id board_id sp
  mthread_posts <- case (headMay mthreads) of
    Nothing -> pure []
    Just (Entity thread_id _) -> getThreadPostsBy_ThreadIdM user_id thread_id sp
  muser <- case (headMay mthread_posts) of
    Nothing -> pure Nothing
    Just (Entity _ ThreadPost{..}) -> Just <$> getUserM user_id threadPostUserId

  return $ BoardPackResponse {
    boardPackResponseBoard = boardToResponse board,
    boardPackResponseBoardId = keyToInt64 board_id,
    boardPackResponseStat = board_stats,
    boardPackResponseLatestThread = fmap threadToResponse $ headMay mthreads,
    boardPackResponseLatestThreadPost = fmap threadPostToResponse $ headMay mthread_posts,
    boardPackResponseLatestThreadPostUser = fmap userToSanitizedResponse muser,
    boardPackResponseLike = Nothing,
    boardPackResponseStar = Nothing
  }
