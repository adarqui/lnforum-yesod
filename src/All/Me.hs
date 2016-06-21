module All.Me (
  -- Handler
  getMeR,
  getMePackR,

  -- Model/Internal
  getMeM
) where



import           Api.Params
import           Api.Response
import           Import
import           All.Prelude



--
-- Handler
--

getMeR :: Handler Value
getMeR = do
  user_id <- requireAuthId
  (toJSON . userToResponse) <$> getMeM user_id



getMePackR :: Handler Value
getMePackR = do
  user_id <- requireAuthId
  toJSON <$> getUserPack_ByUserIdM user_id user_id defaultStandardParams





--
-- Model/Internal
--

getMeM :: UserId -> Handler (Entity User)
getMeM user_id = do
  notFoundMaybe =<< selectFirstDb [ UserId ==. user_id ] []
