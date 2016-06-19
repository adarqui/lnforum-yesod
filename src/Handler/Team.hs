module Handler.Team (
  getTeamsR,
  postTeamR0,
  getTeamR,
  putTeamR,
  deleteTeamR
) where



import           Handler.Prelude
import           Model.Team



getTeamsR :: Handler Value
getTeamsR = do
  user_id <- requireAuthId
  (toJSON . teamsToResponses) <$> getTeamsM user_id



postTeamR0 :: Handler Value
postTeamR0 = do

  user_id <- requireAuthId

  sp <- lookupStandardParams
  case (spOrganizationId sp) of

    Nothing -> notFound

    Just org_id -> do
      team_request <- requireJsonBody :: Handler TeamRequest
      (toJSON . teamToResponse) <$> insertTeamM user_id org_id team_request



getTeamR :: TeamId -> Handler Value
getTeamR team_id = do
  user_id <- requireAuthId
  (toJSON . teamToResponse) <$> getTeamM user_id team_id



putTeamR :: TeamId -> Handler Value
putTeamR team_id = do
  user_id <- requireAuthId
  team_request <- requireJsonBody
  (toJSON . teamToResponse) <$> updateTeamM user_id team_id team_request



deleteTeamR :: TeamId -> Handler Value
deleteTeamR team_id = do
  user_id <- requireAuthId
  void $ deleteTeamM user_id team_id
  pure $ toJSON ()
