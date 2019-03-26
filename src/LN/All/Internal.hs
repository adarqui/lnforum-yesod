{-# LANGUAGE RecordWildCards #-}

module LN.All.Internal (
  getUserM,
  getUserMaybeM,
  getForumM,
  getForumMaybeM,
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
