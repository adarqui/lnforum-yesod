module LN.All.Empty (
  emptyM
) where



import           LN.All.Prelude



--
-- LN.Model/Internal
--

emptyM :: HandlerEff ()
emptyM = do
  pure ()
