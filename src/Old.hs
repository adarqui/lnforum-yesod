{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

--
-- Old stuff.. that I want to keep for reference
--

module Old (
  getBoardStatM_qBoardStats,
  qBoardStats,
  getThreads_ByOrganizationIdM
) where




import           All.Prelude
import           Database.Esqueleto     ((^.))
import qualified Database.Esqueleto     as E
import           All.Forum



getBoardStatM_qBoardStats :: UserId -> BoardId -> HandlerEff BoardStatResponse
getBoardStatM_qBoardStats _ board_id = do

{-
 - select COUNT(DISTINCT board.id), COUNT(DISTINCT thread.id), COUNT(DISTINCT thread_post.id) from board LEFT JOIN thread ON (board.id=thread.board_id) LEFT JOIN thread_post ON (thread.id=thread_post.thread_id) where board.id=1394;
 -  count | count | count
 -  -------+-------+-------
 -       1 |   443 |    99
 -       (1 row)
-}

  stats <- qBoardStats board_id

  let ((_,threads,thread_posts):[]) = fmap (\(x,y,z) -> (E.unValue x, E.unValue y, E.unValue z)) stats

  return $ BoardStatResponse {
    boardStatResponseBoardId = keyToInt64 board_id,
    boardStatResponseThreads = threads,
    boardStatResponseThreadPosts = thread_posts,
    boardStatResponseViews = 0
  }




qBoardStats :: forall site.
     (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
     Key Board -> ControlMA (HandlerT site IO) [(E.Value Int64, E.Value Int64, E.Value Int64)]
qBoardStats board_id = do
  _runDB
    $ E.select
    $ E.from $ \((thread_post :: E.SqlExpr (Entity ThreadPost)) `E.InnerJoin` (thread :: E.SqlExpr (Entity Thread)) `E.InnerJoin` (board :: E.SqlExpr (Entity Board))) -> do
      E.on $ thread ^. ThreadBoardId E.==. board ^. BoardId
      E.on $ thread_post ^. ThreadPostThreadId E.==. thread ^. ThreadId
      E.where_ $
        board ^. BoardId E.==. E.val board_id
      let _ = thread_post ^. ThreadPostId
      let _ = thread ^. ThreadId
      let _ = board ^. BoardId
      return (E.countDistinct $ board ^. BoardId, E.countDistinct $ thread ^. ThreadId, E.countDistinct $ thread_post ^. ThreadPostId)



-- getThreadsM :: UserId -> HandlerEff [Entity Thread]
-- getThreadsM _ = do
--
--   sp@StandardParams{..} <- lookupStandardParams
--
--   case spOrganizationId of
--     Just org_id -> do
--       _runDB
--         $ E.select
--         $ E.from $ \(thread `E.InnerJoin` board `E.InnerJoin` forum `E.InnerJoin` org) -> do
--           E.on $ forum ^. ForumOrgId E.==. org ^. OrganizationId
--           E.on $ board ^. BoardForumId E.==. forum ^. ForumId
--           E.on $ thread ^. ThreadBoardId E.==. board ^. BoardId
--           E.where_ $ org ^. OrganizationId E.==. E.val org_id
--           spToSelectE sp
--           return thread
--     Nothing       ->
--
--       case spBoardId of
--         -- IMPORTANT: need to specify something other than ThreadId, because of ordering
--
--         Just board_id -> selectListDb [ThreadBoardId ==. board_id] [] (orderByToField spOrder)
--
--         Nothing ->
--
--           case spUserId of
--             Just user_id' -> boop [ ThreadUserId ==. user_id' ]
--             Nothing -> boop []
--
--   where
--   boop k = selectListDb k [] ThreadId
--
--



getThreads_ByOrganizationIdM :: UserId -> OrganizationId -> StandardParams -> HandlerEff [Entity Thread]
getThreads_ByOrganizationIdM _ org_id sp = do

  _runDB
    $ E.select
    $ E.from $ \(thread `E.InnerJoin` board `E.InnerJoin` forum `E.InnerJoin` org) -> do
      E.on $ forum ^. ForumOrgId E.==. org ^. OrganizationId
      E.on $ board ^. BoardForumId E.==. forum ^. ForumId
      E.on $ thread ^. ThreadBoardId E.==. board ^. BoardId
      E.where_ $ org ^. OrganizationId E.==. E.val org_id
      spToSelectE sp
      return thread
