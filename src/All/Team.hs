{-# LANGUAGE RecordWildCards #-}

module All.Team (
  -- Handler
  getTeamsR,
  getTeamR,
  putTeamR,

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
  insertTeam_InternalM,
  insert_SystemTeamsM,
  updateTeamM,
  deleteTeamM,
  getTeamCountM
) where



import           All.Prelude
import           All.TeamMember



--
-- Handler
--

getTeamsR :: Handler Value
getTeamsR = run $ do
  user_id <- _requireAuthId
  (toJSON . teamsToResponses) <$> getTeamsM user_id



-- INFO: Can't insert teams
--
-- postTeamR0 :: Handler Value
-- postTeamR0 = run $ do
--   user_id <- _requireAuthId
--   sp <- lookupStandardParams
--   case (spOrganizationId sp) of
--     Nothing -> notFound
--     Just org_id -> do
--       team_request <- requireJsonBody :: HandlerEff TeamRequest
--       (toJSON . teamToResponse) <$> insertTeamM user_id org_id team_request



getTeamR :: TeamId -> Handler Value
getTeamR team_id = run $ do
  user_id <- _requireAuthId
  (toJSON . teamToResponse) <$> getTeamM user_id team_id



putTeamR :: TeamId -> Handler Value
putTeamR team_id = run $ do
  user_id <- _requireAuthId
  team_request <- requireJsonBody
  (toJSON . teamToResponse) <$> updateTeamM user_id team_id team_request



-- INFO: Can't delete teams
--
-- deleteTeamR :: TeamId -> Handler Value
-- deleteTeamR team_id = run $ do
--   user_id <- _requireAuthId
--   void $ deleteTeamM user_id team_id
--   pure $ toJSON ()








--
-- Model/Function
--

teamRequestToTeam :: UserId -> OrganizationId -> TeamRequest -> Team
teamRequestToTeam user_id org_id TeamRequest{..} = Team {
  teamUserId      = user_id,
  teamOrgId       = org_id,
  teamName        = "",
  teamDisplayName = "",
  teamDescription = Nothing,
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

getTeamsM :: UserId -> HandlerEff [Entity Team]
getTeamsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spOrganizationId, spUserId) of
    (Just org_id, Nothing)         -> getTeams_ByOrganizationIdM user_id org_id sp
    (Nothing, Just lookup_user_id) -> getTeams_ByUserIdM user_id lookup_user_id sp
    (_, _)                         -> notFound



getTeams_ByOrganizationIdM :: UserId -> OrganizationId -> StandardParams -> HandlerEff [Entity Team]
getTeams_ByOrganizationIdM _ org_id sp = do
  selectListDb sp [TeamOrgId ==. org_id] [] TeamId



getTeams_ByUserIdM :: UserId -> UserId -> StandardParams -> HandlerEff [Entity Team]
getTeams_ByUserIdM _ lookup_user_id sp = do
  selectListDb sp [TeamUserId ==. lookup_user_id] [] TeamId



getTeams_ByEverythingM :: UserId -> StandardParams -> HandlerEff [Entity Team]
getTeams_ByEverythingM _ sp = do
  selectListDb sp [] [] TeamId



getTeamM :: UserId -> TeamId -> HandlerEff (Entity Team)
getTeamM _ team_id = do
  notFoundMaybe =<< selectFirstDb [ TeamId ==. team_id ] []



-- insertTeamM :: UserId -> OrganizationId -> TeamRequest -> HandlerEff (Entity Team)
-- insertTeamM user_id org_id team_request = do
-- --  sp <- lookupStandardParams
--   ts <- timestampH'
--   let
--     team = (teamRequestToTeam user_id org_id team_request) { teamCreatedAt = Just ts }
--   insertEntityDb team



insertTeam_InternalM :: UserId -> OrganizationId -> SystemTeam -> TeamRequest -> HandlerEff (Entity Team)
insertTeam_InternalM user_id org_id system_team team_request = do

  ts <- timestampH'

  let
    team = (teamRequestToTeam user_id org_id team_request) { teamCreatedAt = Just ts }

  insertEntityDb $ team { teamSystem = system_team }



insert_SystemTeamsM :: UserId -> OrganizationId -> HandlerEff ()
insert_SystemTeamsM user_id org_id = do

  -- bg job: Insert owners team
  (Entity owners_id team) <- insertTeam_InternalM user_id organization_id Team_Owners (TeamRequest Membership_InviteOnly Nothing [] Public 0)
  void $ insertTeamMember_InternalM user_id owners_id (TeamMemberRequest 0)

  -- bg job: Insert members team
  (Entity members_id team) <- insertTeam_InternalM user_id organization_id Team_Members (TeamRequest Membership_Join Nothing [] Public 0)
  void $ insertTeamMember_InternalM user_id members_id (TeamMemberRequest 0)

  return ()




updateTeamM :: UserId -> TeamId -> TeamRequest -> HandlerEff (Entity Team)
updateTeamM user_id team_id team_request = do

  ts <- timestampH'

  let
    Team{..} = (teamRequestToTeam user_id dummyId team_request) { teamModifiedAt = Just ts }

  updateWhereDb
    [ TeamUserId ==. user_id, TeamId ==. team_id ]
    [ TeamModifiedAt  =. teamModifiedAt
    , TeamMembership  =. teamMembership
    , TeamIcon        =. teamIcon
    , TeamTags        =. teamTags
    , TeamVisibility  =. teamVisibility
    , TeamGuard      +=. teamGuard
    ]

  notFoundMaybe =<< selectFirstDb [ TeamUserId ==. user_id, TeamId ==. team_id ] []



deleteTeamM :: UserId -> TeamId -> HandlerEff ()
deleteTeamM user_id team_id = do
  deleteWhereDb [ TeamUserId ==. user_id, TeamId ==. team_id ]



getTeamCountM :: HandlerEff Int
getTeamCountM = do
  _runDB $ count [ TeamName !=. "" ]
