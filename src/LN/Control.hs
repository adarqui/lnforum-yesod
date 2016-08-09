{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}

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
) where



import           Control.Monad.Trans.RWS
import           Haskell.Helpers.Either     as A
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
