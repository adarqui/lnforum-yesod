module Handler.Profile (
  getProfilesR,
  getProfileR,
  putProfileR,
) where



import           Handler.Prelude
import           Model.Profile



getProfilesR :: Handler Value
getProfilesR = do
  user_id <- requireAuthId
  (toJSON . profilesToResponses) <$> getProfilesM user_id



getProfileR :: ProfileId -> Handler Value
getProfileR profile_id = do
  user_id <- requireAuthId
  (toJSON . profileToResponse) <$> getProfileM user_id profile_id



putProfileR :: ProfileId -> Handler Value
putProfileR profile_id = do
  user_id <- requireAuthId
  profile_request <- requireJsonBody
  (toJSON . profileToResponse) <$> updateProfileM user_id profile_id profile_request
