module Control (
  HandlerEff,
  ControlM,
  ControlMA,
  ControlReader,
  ControlWriter,
  ControlState,
  InternalControlState (..),
  run,
  ErrorEff,
  left,
  right
) where



import           Control.Monad.Trans.RWS
import qualified Data.Map                as M

import           Import

import           Cache
import           LN.T.Error



type HandlerEff a = ControlMA Handler a



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
