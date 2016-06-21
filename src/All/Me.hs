module All.Me (
  getMeR,
  getMePackR,

  -- Model/Internal
  getMeM
) where



import           Api.Params
import           Api.Response
import           Import
import           Handler.Prelude
import           Model.Me.Internal
import           Model.Pack.User
import           Model.User.Function



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
