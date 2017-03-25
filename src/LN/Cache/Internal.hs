{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RecordWildCards #-}

module LN.Cache.Internal (
  module A,
  cacheRun,
  cacheRun',
  cacheRunMaybe,
  cacheRunMaybe',
  modifyCache,
  getsCache,
  getUserC,
  putUserC
) where



import           Control.Monad.Trans.RWS
import qualified Data.Map                as Map
import           Haskell.Helpers.Either  as A

import           LN.Cache                as A
import           LN.Control
import           LN.Import



cacheRun
  :: Maybe (CacheEntry a)
  -> HandlerErrorEff a         -- ^ CacheMissing        - an entry was previously looked up, but not found
  -> (a -> HandlerErrorEff a)  -- ^ CacheEntry (Just a) - an entry exists
  -> HandlerErrorEff a         -- ^ CacheEntry Nothing  - an entry doesnt exist and was never looked up
  -> HandlerErrorEff a

cacheRun m_c_entry go_missing go_entry go_nothing =
  case m_c_entry of
    Just CacheMissing   -> go_missing
    Just (CacheEntry a) -> go_entry a
    Nothing             -> go_nothing



cacheRun'
  :: Maybe (CacheEntry a)
  -> HandlerErrorEff a         -- ^ CacheEntry Nothing  - an entry doesnt exist and was never looked up
  -> HandlerErrorEff a

cacheRun' m_c_entry go_nothing = cacheRun m_c_entry (leftA Error_NotFound) rightA go_nothing



cacheRunMaybe
  :: Maybe (CacheEntry a)
  -> HandlerEff (Maybe a)         -- ^ CacheMissing        - an entry was previously looked up, but not found
  -> (a -> HandlerEff (Maybe a))  -- ^ CacheEntry (Just a) - an entry exists
  -> HandlerEff (Maybe a)         -- ^ CacheEntry Nothing  - an entry doesnt exist and was never looked up
  -> HandlerEff (Maybe a)

cacheRunMaybe m_c_entry go_missing go_entry go_nothing =
  case m_c_entry of
    Just CacheMissing   -> go_missing
    Just (CacheEntry a) -> go_entry a
    Nothing             -> go_nothing



cacheRunMaybe'
  :: Maybe (CacheEntry a)
  -> HandlerEff (Maybe a)         -- ^ CacheEntry Nothing  - an entry doesnt exist and was never looked up
  -> HandlerEff (Maybe a)

cacheRunMaybe' m_c_entry go_nothing = cacheRunMaybe m_c_entry (pure Nothing) (pure . Just) go_nothing



modifyCache
  :: forall r w (m :: * -> *). (Monad m, Monoid w)
  => (Cache -> Cache)
  -> RWST r w InternalControlState m ()

modifyCache go = do
  modify (\st@InternalControlState{..} -> st{ cache = go cache })



getsCache
  :: forall b r w (m :: * -> *). (Monad m, Monoid w)
  => (Cache -> b)
  -> RWST r w InternalControlState m b

getsCache field = do
  field <$> gets cache



getUserC :: UserId -> HandlerEff (Maybe (CacheEntry (Entity User)))
getUserC user_id = do
  c_users <- getsCache cacheUsers
  let
    m_c_user = Map.lookup user_id c_users
  case m_c_user of
    Nothing     -> pure Nothing
    Just c_user -> pure $ Just c_user



putUserC :: UserId -> CacheEntry (Entity User) -> HandlerEff ()
putUserC user_id c_user = do
  modifyCache (\st@Cache{..}->st{ cacheUsers = Map.insert user_id c_user cacheUsers })
