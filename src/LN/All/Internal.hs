{-# LANGUAGE RecordWildCards #-}

module LN.All.Internal (
  getUserM,
  getUserMaybeM
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
