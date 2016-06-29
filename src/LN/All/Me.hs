module LN.All.Me (
  -- Handler
  getMeR,

  -- Model/Internal
  getMeM
) where



import           LN.All.Prelude
import           LN.All.User



--
-- Handler
--

getMeR :: Handler Value
getMeR = run $ do
  user_id <- _requireAuthId
  (toJSON . userToResponse) <$> getMeM user_id







--
-- Model/Internal
--

getMeM :: UserId -> HandlerEff (Entity User)
getMeM user_id = do
  notFoundMaybe =<< selectFirstDb [ UserId ==. user_id ] []
