module Control (
  HandlerEff,
  ControlM,
  ControlMA,
  ControlReader,
  ControlWriter,
  ControlState,
  run
) where



import Control.Monad.Trans.RWS

import Import



type HandlerEff a = ControlMA Handler a



type ControlM      = RWST
type ControlMA m a = ControlM ControlReader ControlWriter ControlState m a
type ControlReader = ()
type ControlWriter = ()
type ControlState  = ()



-- run ::
run op = fst <$> evalRWST op () ()
