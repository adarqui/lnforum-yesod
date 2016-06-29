module LN.Control (
  LN.HandlerEff,
  LN.HandlerErrorEff,
  LN.ControlM,
  LN.ControlMA,
  LN.ControlReader,
  LN.ControlWriter,
  LN.ControlState,
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



import qualified LN.Control.Monad.Trans.Either as Either
import           LN.Control.Monad.Trans.RWS
import qualified Data.Map                as M

import           Import

import           Cache
import           LN.T.Error



type LN.HandlerEff a = LN.ControlMA LN.Handler a
type LN.HandlerErrorEff a = LN.HandlerEff (ErrorEff a)




type LN.ControlM      = RWST
type LN.ControlMA m a = LN.ControlM LN.ControlReader LN.ControlWriter LN.ControlState m a
type LN.ControlReader = ()
type LN.ControlWriter = ()
type LN.ControlState  = InternalControlState



data InternalControlState = InternalControlState {
  cache :: Cache
}



defaultControlState :: InternalControlState
defaultControlState = InternalControlState {
  cache    = defaultCache
}



-- run ::
run op = fst <$> evalRWST op () defaultControlState



type ErrorEff = Either ApplicationError



left  = pure . Left
right = pure . Right

unknownError = left Error_Unexpected



leftT = Either.left
rightT = Either.right

isT go = do
  x <- lift go
  case x of
    Left err -> leftT err
    Right v  -> rightT v
