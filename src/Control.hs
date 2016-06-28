module Control (
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
  ApplicationError (..) -- re-export
) where



import           Control.Monad.Trans.RWS
import qualified Data.Map                as M

import           Import

import           Cache
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
run op = fst <$> evalRWST op () defaultControlState



type ErrorEff = Either ApplicationError



left  = pure . Left
right = pure . Right

unknownError = left Error_Unexpected
