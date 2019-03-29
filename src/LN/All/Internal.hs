{-# LANGUAGE RecordWildCards #-}

module LN.All.Internal (
  getUserM,
  getUserMaybeM,
  getForumM,
  getForumMaybeM,
  getBoardM,
  getBoardMaybeM,
  getThreadM,
  getThreadMaybeM,
  getThreadPostM,
  getThreadPostMaybeM,
) where



import           Data.Ebyam                 as A (ebyam)

import           LN.Cache                   as A
import           LN.Cache.Internal
import           LN.Control                 as A
import           LN.Db                      as A
import           LN.Import                  as A



getUserM :: UserId -> UserId -> HandlerErrorEff (Entity User)
getUserM user_id lookup_user_id = do
  maybe (leftA Error_NotFound) rightA =<< getUserMaybeM user_id lookup_user_id



getUserMaybeM :: UserId -> UserId -> HandlerEff (Maybe (Entity User))
getUserMaybeM _ lookup_user_id = do
  m_c_user <- getUserC lookup_user_id
  cacheRunMaybe' m_c_user $ do
    m_user <- selectFirstDb [UserId ==. lookup_user_id, UserActive ==. True] []
    ebyam
      m_user
      (putUserC lookup_user_id CacheMissing *> pure Nothing)
      (\user -> putUserC lookup_user_id (CacheEntry user) *> pure (Just user))



getForumM :: UserId -> ForumId -> HandlerErrorEff (Entity Forum)
getForumM user_id forum_id = do
  maybe (leftA Error_NotFound) rightA =<< getForumMaybeM user_id forum_id



getForumMaybeM :: UserId -> ForumId -> HandlerEff (Maybe (Entity Forum))
getForumMaybeM _ forum_id = do
  m_c_forum <- getForumC forum_id
  cacheRunMaybe' m_c_forum $ do
    m_forum <- selectFirstDb [ForumId ==. forum_id, ForumActive ==. True] []
    ebyam
      m_forum
      (putForumC forum_id CacheMissing *> pure Nothing)
      (\forum -> putForumC forum_id (CacheEntry forum) *> pure (Just forum))



getBoardM :: UserId -> BoardId -> HandlerErrorEff (Entity Board)
getBoardM user_id board_id = do
  maybe (leftA Error_NotFound) rightA =<< getBoardMaybeM user_id board_id



getBoardMaybeM :: UserId -> BoardId -> HandlerEff (Maybe (Entity Board))
getBoardMaybeM _ board_id = do
  m_c_board <- getBoardC board_id
  cacheRunMaybe' m_c_board $ do
    m_board <- selectFirstDb [BoardId ==. board_id, BoardActive ==. True] []
    ebyam
      m_board
      (putBoardC board_id CacheMissing *> pure Nothing)
      (\board -> putBoardC board_id (CacheEntry board) *> pure (Just board))



getThreadM :: UserId -> ThreadId -> HandlerErrorEff (Entity Thread)
getThreadM user_id thread_id = do
  maybe (leftA Error_NotFound) rightA =<< getThreadMaybeM user_id thread_id



getThreadMaybeM :: UserId -> ThreadId -> HandlerEff (Maybe (Entity Thread))
getThreadMaybeM _ thread_id = do
  m_c_thread <- getThreadC thread_id
  cacheRunMaybe' m_c_thread $ do
    m_thread <- selectFirstDb [ThreadId ==. thread_id, ThreadActive ==. True] []
    ebyam
      m_thread
      (putThreadC thread_id CacheMissing *> pure Nothing)
      (\thread -> putThreadC thread_id (CacheEntry thread) *> pure (Just thread))



getThreadPostM :: UserId -> ThreadPostId -> HandlerErrorEff (Entity ThreadPost)
getThreadPostM user_id post_id = do
  (maybe (leftA Error_NotFound) rightA) =<< getThreadPostMaybeM user_id post_id

  -- m_c_forum <- getThreadPostC forum_id
  -- cacheRun' m_c_forum $ do
  --   lr <- selectFirstDbE [ThreadPostId ==. forum_id, ThreadPostActive ==. True] []
  --   rehtie
  --     lr
  --     (\err   -> putThreadPostC forum_id CacheMissing *> leftA err)
  --     (\forum -> putThreadPostC forum_id (CacheEntry forum) *> rightA forum)



getThreadPostMaybeM :: UserId -> ThreadPostId -> HandlerEff (Maybe (Entity ThreadPost))
getThreadPostMaybeM _ post_id = do
  m_c_post <- getThreadPostC post_id
  cacheRunMaybe' m_c_post $ do
    m_post <- selectFirstDb [ThreadPostId ==. post_id, ThreadPostActive ==. True] []
    ebyam
      m_post
      (putThreadPostC post_id CacheMissing *> pure Nothing)
      (\post -> putThreadPostC post_id (CacheEntry post) *> pure (Just post))
