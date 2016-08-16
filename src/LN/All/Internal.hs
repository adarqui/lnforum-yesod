{-# LANGUAGE RecordWildCards #-}

module LN.All.Internal (
  getOrganizationM,
  getTeamM,
  getForumM,
  getBoardM,
  getThreadM,
  getThreadPostM,
  getThreadPostMaybeM
) where



import           Data.Ebyam                 as A (ebyam)
import           Data.Rehtie                as A (rehtie)

import           LN.Cache                   as A
import           LN.Cache.Internal
import           LN.Control                 as A
import           LN.Db                      as A
import           LN.Import                  as A




getOrganizationM :: UserId -> OrganizationId -> HandlerErrorEff (Entity Organization)
getOrganizationM _ org_id = do
  m_c_organization <- getOrganizationC org_id
  cacheRun' m_c_organization $ do
    lr <- selectFirstDbE [OrganizationId ==. org_id, OrganizationActive ==. True] []
    rehtie
      lr
      (\err          -> putOrganizationC org_id CacheMissing *> leftA err)
      (\organization -> putOrganizationC org_id (CacheEntry organization) *> rightA organization)



getTeamM :: UserId -> TeamId -> HandlerErrorEff (Entity Team)
getTeamM _ team_id = do
  m_c_team <- getTeamC team_id
  cacheRun' m_c_team $ do
    lr <- selectFirstDbE [TeamId ==. team_id, TeamActive ==. True] []
    rehtie
      lr
      (\err  -> putTeamC team_id CacheMissing *> leftA err)
      (\team -> putTeamC team_id (CacheEntry team) *> rightA team)



getForumM :: UserId -> ForumId -> HandlerErrorEff (Entity Forum)
getForumM _ forum_id = do
  m_c_forum <- getForumC forum_id
  cacheRun' m_c_forum $ do
    lr <- selectFirstDbE [ForumId ==. forum_id, ForumActive ==. True] []
    rehtie
      lr
      (\err   -> putForumC forum_id CacheMissing *> leftA err)
      (\forum -> putForumC forum_id (CacheEntry forum) *> rightA forum)



getBoardM :: UserId -> BoardId -> HandlerErrorEff (Entity Board)
getBoardM _ forum_id = do
  m_c_forum <- getBoardC forum_id
  cacheRun' m_c_forum $ do
    lr <- selectFirstDbE [BoardId ==. forum_id, BoardActive ==. True] []
    rehtie
      lr
      (\err   -> putBoardC forum_id CacheMissing *> leftA err)
      (\forum -> putBoardC forum_id (CacheEntry forum) *> rightA forum)



getThreadM :: UserId -> ThreadId -> HandlerErrorEff (Entity Thread)
getThreadM user_id thread_id = do
  maybe (leftA Error_NotFound) rightA =<< getThreadMaybeM user_id thread_id

  -- m_c_forum <- getThreadC forum_id
  -- cacheRun' m_c_forum $ do
  --   lr <- selectFirstDbE [ThreadId ==. forum_id, ThreadActive ==. True] []
  --   rehtie
  --     lr
  --     (\err   -> putThreadC forum_id CacheMissing *> leftA err)
  --     (\forum -> putThreadC forum_id (CacheEntry forum) *> rightA forum)

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
getThreadPostM user_id forum_id = do
  (maybe (leftA Error_NotFound) rightA) =<< getThreadPostMaybeM user_id forum_id

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
