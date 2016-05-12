{-# LANGUAGE RecordWildCards #-}

module Model.Team.Internal (
  getTeamsM,
  getTeamsBy_OrganizationIdM,
  getTeamsBy_UserIdM,
  getTeamsBy_EverythingM,

  getTeamM,
  insertTeamM,
  updateTeamM,
  deleteTeamM,

  getTeamCountM
) where



import           Model.Prelude
import           Model.Team.Function



getTeamsM :: UserId -> Handler [Entity Team]
getTeamsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spOrganizationId, spUserId) of
    (Just org_id, Nothing)         -> getTeamsBy_OrganizationIdM user_id org_id sp
    (Nothing, Just lookup_user_id) -> getTeamsBy_UserIdM user_id lookup_user_id sp
    (_, _)                         -> notFound



getTeamsBy_OrganizationIdM :: UserId -> OrganizationId -> StandardParams -> Handler [Entity Team]
getTeamsBy_OrganizationIdM _ org_id sp = do
  selectListDb sp [TeamOrgId ==. org_id] [] TeamId



getTeamsBy_UserIdM :: UserId -> UserId -> StandardParams -> Handler [Entity Team]
getTeamsBy_UserIdM _ lookup_user_id sp = do
  selectListDb sp [TeamUserId ==. lookup_user_id] [] TeamId



getTeamsBy_EverythingM :: UserId -> StandardParams -> Handler [Entity Team]
getTeamsBy_EverythingM _ sp = do
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



updateTeamM :: UserId -> TeamId -> TeamRequest -> Handler (Entity Team)
updateTeamM user_id team_id team_request = do

  ts <- timestampH'

  let
    Team{..} = (teamRequestToTeam user_id dummyId team_request) { teamModifiedAt = Just ts }

  updateWhereDb
    [ TeamUserId ==. user_id, TeamId ==. team_id ]
    [ TeamModifiedAt =. teamModifiedAt
    , TeamName =. teamName
    , TeamDescription =. teamDescription
    ]

  notFoundMaybe =<< selectFirstDb [ TeamUserId ==. user_id, TeamId ==. team_id ] []



deleteTeamM :: UserId -> TeamId -> Handler ()
deleteTeamM user_id team_id = do
  deleteWhereDb [ TeamUserId ==. user_id, TeamId ==. team_id ]



getTeamCountM :: Handler Int
getTeamCountM = do
  runDB $ count [ TeamName !=. "" ]
