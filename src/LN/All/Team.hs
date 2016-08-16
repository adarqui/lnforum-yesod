{-# LANGUAGE RecordWildCards #-}

module LN.All.Team (
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
--  getTeamM,
  getTeamMH,
  insertTeam_InternalM,
  insert_SystemTeamsM,
  updateTeamM,
  deleteTeamM,
  getTeamCountM,
  getTeamStatM
) where



import qualified Data.Text         as T

import           LN.All.Internal
import           LN.All.Prelude
import           LN.All.TeamMember
import           LN.T.Membership
import           LN.T.Visibility



--
-- Handler
--

getTeamsR :: Handler Value
getTeamsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON teamsToResponses $ getTeamsM (pure sp) user_id



getTeamR :: TeamId -> Handler Value
getTeamR team_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON teamToResponse $ getTeamM user_id team_id



putTeamR :: TeamId -> Handler Value
putTeamR team_id = run $ do
  user_id      <- _requireAuthId
  team_request <- requireJsonBody
  errorOrJSON teamToResponse $ updateTeamM user_id team_id team_request







--
-- Model/Function
--

teamRequestToTeam :: UserId -> OrganizationId -> TeamRequest -> Team
teamRequestToTeam user_id org_id TeamRequest{..} = Team {
  teamUserId      = user_id,
  teamOrgId       = org_id,
  teamSystem      = Team_Members,
  teamMembership  = teamRequestMembership,
  teamIcon        = teamRequestIcon,
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
  teamResponseSystem      = teamSystem,
  teamResponseMembership  = teamMembership,
  teamResponseIcon        = teamIcon,
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

getTeamsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Team]
getTeamsM m_sp user_id = do

  case (lookupSpMay m_sp spOrganizationId, lookupSpMay m_sp spUserId) of
    (Just org_id, Nothing)         -> getTeams_ByOrganizationIdM m_sp user_id org_id
    (Nothing, Just lookup_user_id) -> getTeams_ByUserIdM m_sp user_id lookup_user_id
    _                              -> leftA $ Error_InvalidArguments "org_id, user_id"



getTeams_ByOrganizationIdM :: Maybe StandardParams -> UserId -> OrganizationId -> HandlerErrorEff [Entity Team]
getTeams_ByOrganizationIdM m_sp _ org_id = do
  selectListDbE m_sp [TeamOrgId ==. org_id, TeamActive ==. True] [] TeamId



getTeams_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity Team]
getTeams_ByUserIdM m_sp _ lookup_user_id = do
  selectListDbE m_sp [TeamUserId ==. lookup_user_id, TeamActive ==. True] [] TeamId



getTeamMH :: UserId -> Text -> OrganizationId -> HandlerErrorEff (Entity Team)
getTeamMH _ team_sid org_id = do
  case m_system_team of
    Nothing          -> notFound
    Just system_team -> do
      selectFirstDbE [TeamOrgId ==. org_id, TeamSystem ==. system_team, TeamActive ==. True] []

  where
  m_system_team = readMay $ T.unpack team_sid



insertTeam_InternalM :: UserId -> OrganizationId -> SystemTeam -> TeamRequest -> HandlerErrorEff (Entity Team)
insertTeam_InternalM user_id org_id system_team team_request = do

  ts <- timestampH'

  let
    team = (teamRequestToTeam user_id org_id team_request) { teamCreatedAt = Just ts }

  insertEntityDbE $ team { teamSystem = system_team }



insert_SystemTeamsM :: UserId -> OrganizationId -> HandlerErrorEff ()
insert_SystemTeamsM user_id org_id = do

  void $ runEitherT $ do
    -- bg job: Insert owners team
    (Entity owners_id _)  <- mustT $ insertTeam_InternalM user_id org_id Team_Owners (TeamRequest Membership_InviteOnly Nothing Public 0)
    void $ mustT $ insertTeamMember_BypassM user_id org_id owners_id (TeamMemberRequest 0)

    -- bg job: Insert members team
    (Entity members_id _) <- mustT $ insertTeam_InternalM user_id org_id Team_Members (TeamRequest Membership_Join Nothing Public 0)
    void $ mustT $ insertTeamMember_BypassM user_id org_id members_id (TeamMemberRequest 0)

    pure ()

  rightA ()




updateTeamM :: UserId -> TeamId -> TeamRequest -> HandlerErrorEff (Entity Team)
updateTeamM user_id team_id team_request = do

  ts <- timestampH'

  let
    Team{..} = (teamRequestToTeam user_id dummyId team_request) { teamModifiedAt = Just ts }

  updateWhereDb
    [TeamUserId ==. user_id, TeamId ==. team_id, TeamActive ==. True]
    [ TeamModifiedAt  =. teamModifiedAt
    , TeamMembership  =. teamMembership
    , TeamIcon        =. teamIcon
    , TeamVisibility  =. teamVisibility
    , TeamGuard      +=. teamGuard
    ]

  selectFirstDbE [TeamUserId ==. user_id, TeamId ==. team_id, TeamActive ==. True] []



deleteTeamM :: UserId -> TeamId -> HandlerErrorEff ()
deleteTeamM user_id team_id = do
  deleteWhereDbE [TeamUserId ==. user_id, TeamId ==. team_id, TeamActive ==. True]



getTeamCountM :: HandlerErrorEff Int
getTeamCountM = rightA 2



getTeamStatM :: UserId -> TeamId -> HandlerErrorEff TeamStatResponse
getTeamStatM _ _ = do
  rightA $ TeamStatResponse {
    teamStatResponseMembers = 0 -- TODO FIXME
  }
