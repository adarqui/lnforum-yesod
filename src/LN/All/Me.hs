module LN.All.Me (
  -- LN.Handler
  getMeR,

  -- Model/Internal
  getMeM
) where



import           LN.All.Prelude
import           LN.All.User



--
-- LN.Handler
--

getMeR :: LN.Handler Value
getMeR = run $ do
  user_id <- _requireAuthId
  (toJSON . userToResponse) <$> getMeM user_id







--
-- Model/Internal
--

getMeM :: UserId -> LN.HandlerEff (Entity User)
getMeM user_id = do
  notFoundMaybe =<< selectFirstDb [ UserId ==. user_id ] []
