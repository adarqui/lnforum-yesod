module All.Me (
  -- Handler
  getMeR,

  -- Model/Internal
  getMeM
) where



import           Api.Params
import           Api.Response
import           Import
import           All.Prelude
import           Model.User.Function



--
-- Handler
--

getMeR :: Handler Value
getMeR = do
  user_id <- requireAuthId
  (toJSON . userToResponse) <$> getMeM user_id







--
-- Model/Internal
--

getMeM :: UserId -> Handler (Entity User)
getMeM user_id = do
  notFoundMaybe =<< selectFirstDb [ UserId ==. user_id ] []
