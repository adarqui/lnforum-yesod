module All.Me (
  -- Handler
  getMeR,

  -- Model/Internal
  getMeM
) where



import           All.Prelude
import           All.User
import           Api.Params
import           Api.Response
import           Import



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
