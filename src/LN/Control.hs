{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RecordWildCards #-}

module LN.Control (
  module A,
  HandlerEff,
  HandlerErrorEff,
  ControlM,
  ControlMA,
  ControlReader,
  ControlWriter,
  ControlState,
  InternalControlState (..),
  run,
  ErrorEff,
  unknownError,
  ApplicationError (..), -- re-export
  cacheRun,
  modifyCache,
  getsCache,
  getUserC,
  putUserC
) where



import           Control.Monad.Trans.RWS
import qualified Data.Map                as Map
import           Haskell.Helpers.Either  as A

import           LN.Cache
import           LN.Import
import           LN.T.Error



type HandlerEff a      = ControlMA Handler a
type HandlerErrorEff a = HandlerEff (ErrorEff a)




type ControlM      = RWST
type ControlMA m a = ControlM ControlReader ControlWriter ControlState m a
type ControlReader = ()
type ControlWriter = ()
type ControlState  = InternalControlState



data InternalControlState = InternalControlState {
  cache :: Cache
}



defaultControlState :: InternalControlState
defaultControlState = InternalControlState {
  cache    = defaultCache
}



-- run ::
run :: forall b (f :: * -> *) b1. Monad f => RWST () b1 InternalControlState f b -> f b
run op = fst <$> evalRWST op () defaultControlState



type ErrorEff = Either ApplicationError



unknownError :: forall (f :: * -> *) b. Applicative f => f (Either ApplicationError b)
unknownError = leftA Error_Unexpected



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



putUserC :: UserId -> CacheEntry (Entity User) -> HandlerErrorEff ()
putUserC user_id c_user = do
  modifyCache (\st@Cache{..}->st{ cacheUsers = Map.insert user_id c_user cacheUsers })
  rightA ()
