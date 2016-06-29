{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}

module LN.Control (
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
  left,
  right,
  unknownError,
  ApplicationError (..),-- re-export
  leftT,
  rightT,
  isT
) where



import qualified Control.Monad.Trans.Either as Either
import           Control.Monad.Trans.RWS
import           LN.Cache
import           LN.Import
import           LN.T.Error



type HandlerEff a = ControlMA Handler a
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
run :: forall b (f :: * -> *) b1.  Monad f => RWST () b1 InternalControlState f b -> f b
run op = fst <$> evalRWST op () defaultControlState



type ErrorEff = Either ApplicationError



left :: forall a (f :: * -> *) b.  Applicative f => a -> f (Either a b)
left  = pure . Left



right :: forall a (f :: * -> *) a1.  Applicative f => a -> f (Either a1 a)
right = pure . Right



unknownError :: forall (f :: * -> *) b.  Applicative f => f (Either ApplicationError b)
unknownError = left Error_Unexpected



leftT :: forall e (m :: * -> *) a.  Monad m => e -> Either.EitherT e m a
leftT = Either.left



rightT :: forall a e (m :: * -> *).  Monad m => a -> Either.EitherT e m a
rightT = Either.right



isT :: forall b (m :: * -> *) e.  Monad m => m (Either e b) -> Either.EitherT e m b
isT go = do
  x <- lift go
  case x of
    Left err -> leftT err
    Right v  -> rightT v
