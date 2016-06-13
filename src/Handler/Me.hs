module Handler.Me (
  getMeR,
  getMePackR
) where



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
