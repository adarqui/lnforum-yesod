module LN.Control (
  HandlerEff,
  HandlerErrorEff,
  LN.ControlM,
  LN.ControlMA,
  LN.ControlReader,
  LN.ControlWriter,
  LN.ControlState,
  InternalControlState (..),
  run,
  LN.ErrorEff,
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
import qualified Data.Map                as M

import           Import

import           LN.Cache
import           LN.T.Error



type HandlerEff a = LN.ControlMA Handler a
type HandlerErrorEff a = HandlerEff (ErrorEff a)




type LN.ControlM      = RWST
type LN.ControlMA m a = LN.ControlM LN.ControlReader LN.ControlWriter LN.ControlState m a
type LN.ControlReader = ()
type LN.ControlWriter = ()
type LN.ControlState  = InternalControlState



data InternalControlState = InternalControlState {
  cache :: LN.Cache
}



defaultControlState :: InternalControlState
defaultControlState = InternalControlState {
  cache    = defaultCache
}



-- run ::
run op = fst <$> evalRWST op () defaultControlState



type LN.ErrorEff = Either ApplicationError



left  = pure . Left
right = pure . Right

unknownError = left LN.Error_Unexpected



leftT = Either.left
rightT = Either.right

isT go = do
  x <- lift go
  case x of
    Left err -> leftT err
    Right v  -> rightT v
