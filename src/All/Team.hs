{-# LANGUAGE RecordWildCards #-}

module All.Team (
  -- Handler
  getTeamsR,
  postTeamR0,
  getTeamR,
  putTeamR,
  deleteTeamR,

  -- Model/Function
  teamRequestToTeam,
  teamToResponse,
  teamsToResponses,

  -- Model/Internal
  getTeamsM,
  getTeams_ByOrganizationIdM,
  getTeams_ByUserIdM,
  getTeams_ByEverythingM,
  getTeamM,
  insertTeamM,
  insertTeam_BypassM,
  updateTeamM,
  deleteTeamM,
  getTeamCountM
) where



import           All.Prelude



--
-- Handler
--

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








--
-- Model/Function
--

teamRequestToTeam :: UserId -> OrganizationId -> TeamRequest -> Team
teamRequestToTeam user_id org_id TeamRequest{..} = Team {
  teamUserId      = user_id,
  teamOrgId       = org_id,
  teamName        = toPrettyUrl teamRequestDisplayName,
  teamDisplayName = teamRequestDisplayName,
  teamDescription = teamRequestDescription,
  teamMembership  = teamRequestMembership,
  teamIcon        = teamRequestIcon,
  teamTags        = teamRequestTags,
  teamVisibility  = teamRequestVisibility,
  teamActive      = True,
  teamGuard       = teamRequestGuard,
  teamCreatedAt   = Nothing,
  teamModifiedBy  = Nothing,
  teamModifiedAt  = Nothing,
  teamActivityAt  = Nothing
}



teamToResponse :: Entity Team -> TeamResponse
teamToResponse (Entity team_id Team{..}) = TeamResponse {
  teamResponseId          = keyToInt64 team_id,
  teamResponseUserId      = keyToInt64 teamUserId,
  teamResponseOrgId       = keyToInt64 teamOrgId,
  teamResponseName        = teamName,
  teamResponseDisplayName = teamDisplayName,
  teamResponseDescription = teamDescription,
  teamResponseMembership  = teamMembership,
  teamResponseIcon        = teamIcon,
  teamResponseTags        = teamTags,
  teamResponseVisibility  = teamVisibility,
  teamResponseActive      = teamActive,
  teamResponseGuard       = teamGuard,
  teamResponseCreatedAt   = teamCreatedAt,
  teamResponseModifiedBy  = fmap keyToInt64 teamModifiedBy,
  teamResponseModifiedAt  = teamModifiedAt,
  teamResponseActivityAt  = teamActivityAt
}



teamsToResponses :: [Entity Team] -> TeamResponses
teamsToResponses teams = TeamResponses {
  teamResponses = map teamToResponse teams
}









--
-- Model/Internal
--

getTeamsM :: UserId -> Handler [Entity Team]
getTeamsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spOrganizationId, spUserId) of
    (Just org_id, Nothing)         -> getTeams_ByOrganizationIdM user_id org_id sp
    (Nothing, Just lookup_user_id) -> getTeams_ByUserIdM user_id lookup_user_id sp
    (_, _)                         -> notFound



getTeams_ByOrganizationIdM :: UserId -> OrganizationId -> StandardParams -> Handler [Entity Team]
getTeams_ByOrganizationIdM _ org_id sp = do
  selectListDb sp [TeamOrgId ==. org_id] [] TeamId



getTeams_ByUserIdM :: UserId -> UserId -> StandardParams -> Handler [Entity Team]
getTeams_ByUserIdM _ lookup_user_id sp = do
  selectListDb sp [TeamUserId ==. lookup_user_id] [] TeamId



getTeams_ByEverythingM :: UserId -> StandardParams -> Handler [Entity Team]
getTeams_ByEverythingM _ sp = do
  selectListDb sp [] [] TeamId



getTeamM :: UserId -> TeamId -> Handler (Entity Team)
getTeamM _ team_id = do
  notFoundMaybe =<< selectFirstDb [ TeamId ==. team_id ] []



insertTeamM :: UserId -> OrganizationId -> TeamRequest -> Handler (Entity Team)
insertTeamM user_id org_id team_request = do

--  sp <- lookupStandardParams

  ts <- timestampH'

  let
    team = (teamRequestToTeam user_id org_id team_request) { teamCreatedAt = Just ts }

  insertEntityDb team



insertTeam_BypassM :: UserId -> OrganizationId -> TeamRequest -> Handler (Entity Team)
insertTeam_BypassM user_id org_id team_request = do

  ts <- timestampH'

  let
    team = (teamRequestToTeam user_id org_id team_request) { teamCreatedAt = Just ts }

  insertEntityDb team



updateTeamM :: UserId -> TeamId -> TeamRequest -> Handler (Entity Team)
updateTeamM user_id team_id team_request = do

  ts <- timestampH'

  let
    Team{..} = (teamRequestToTeam user_id dummyId team_request) { teamModifiedAt = Just ts }

  updateWhereDb
    [ TeamUserId ==. user_id, TeamId ==. team_id ]
    [ TeamModifiedAt  =. teamModifiedAt
    , TeamName        =. teamName
    , TeamDisplayName =. teamDisplayName
    , TeamDescription =. teamDescription
    , TeamMembership  =. teamMembership
    , TeamIcon        =. teamIcon
    , TeamTags        =. teamTags
    , TeamVisibility  =. teamVisibility
    , TeamGuard      +=. teamGuard
    ]

  notFoundMaybe =<< selectFirstDb [ TeamUserId ==. user_id, TeamId ==. team_id ] []



deleteTeamM :: UserId -> TeamId -> Handler ()
deleteTeamM user_id team_id = do
  deleteWhereDb [ TeamUserId ==. user_id, TeamId ==. team_id ]



getTeamCountM :: Handler Int
getTeamCountM = do
  runDB $ count [ TeamName !=. "" ]
