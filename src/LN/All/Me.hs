module LN.All.Me (
  -- LN.Handler
  getMeR,

  -- LN.Model/Internal
  getMeM
) where



import           LN.All.Prelude
import           LN.All.User



--
-- LN.Handler
--

getMeR :: Handler Value
getMeR = run $ do
  user_id <- _requireAuthId
  (toJSON . userToResponse) <$> getMeM user_id







--
-- LN.Model/Internal
--

getMeM :: UserId -> HandlerEff (Entity User)
getMeM user_id = do
  notFoundMaybe =<< selectFirstDb [ UserId ==. user_id ] []
