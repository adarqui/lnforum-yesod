{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.Team (
  -- Handler
  getTeamPacksR,
  getTeamPackR,
  getTeamPackH,

  -- Model
) where



import           LN.All.Internal
import           LN.All.Prelude
import           LN.All.Team
import           LN.All.User



--
-- Handler
--

getTeamPacksR :: Handler Value
getTeamPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getTeamPacksM (pure sp) user_id



getTeamPackR :: TeamId -> Handler Value
getTeamPackR team_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getTeamPackM user_id team_id



getTeamPackH :: Text -> Handler Value
getTeamPackH team_name = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getTeamPackMH (pure sp) user_id team_name







-- Model

getTeamPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff TeamPackResponses
getTeamPacksM m_sp user_id = do

  case (lookupSpMay m_sp spOrganizationId, lookupSpMay m_sp spUserId, lookupSpBool m_sp spSelf) of

    (Just org_id, _, _)          -> getTeamPacks_ByOrganizationIdM m_sp user_id org_id
    (_, Just lookup_user_id, _)  -> getTeamPacks_ByUserIdM m_sp user_id lookup_user_id
    _                            -> leftA $ Error_InvalidArguments "org_id, user_id, self"



getTeamPackM :: UserId -> TeamId -> HandlerErrorEff TeamPackResponse
getTeamPackM user_id team_id = do

  e_team <- getTeamM user_id team_id
  rehtie e_team leftA $ \team -> getTeamPack_ByTeamM user_id team



getTeamPackMH :: Maybe StandardParams -> UserId -> Text -> HandlerErrorEff TeamPackResponse
getTeamPackMH m_sp user_id team_name = do

  case (lookupSpMay m_sp spOrganizationId) of

    Just org_id -> do
      e_team <- getTeamMH user_id team_name org_id
      rehtie e_team leftA $ getTeamPack_ByTeamM user_id

    _           -> leftA $ Error_InvalidArguments "org_id"



getTeamPacks_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff TeamPackResponses
getTeamPacks_ByUserIdM m_sp user_id lookup_user_id = do

  e_teams <- getTeams_ByUserIdM m_sp user_id lookup_user_id
  rehtie e_teams leftA $ \teams -> do
    teams_packs <- rights <$> mapM (\team -> getTeamPack_ByTeamM user_id team) teams
    rightA $ TeamPackResponses {
      teamPackResponses = teams_packs
    }



getTeamPacks_ByOrganizationIdM :: Maybe StandardParams -> UserId -> OrganizationId -> HandlerErrorEff TeamPackResponses
getTeamPacks_ByOrganizationIdM m_sp user_id org_id = do
  e_teams <- getTeams_ByOrganizationIdM m_sp user_id org_id
  rehtie e_teams leftA $ \teams -> do
    teams_packs <- rights <$> mapM (\team -> getTeamPack_ByTeamM user_id team) teams
    rightA $ TeamPackResponses {
      teamPackResponses = teams_packs
    }




getTeamPack_ByTeamM :: UserId -> Entity Team -> HandlerErrorEff TeamPackResponse
getTeamPack_ByTeamM user_id team@(Entity team_id Team{..}) = do

  lr <- runEitherT $ do
    team_user    <- mustT $ getUserM user_id teamUserId
    team_stats   <- mustT $ getTeamStatM user_id (entityKey team)
    pure (team_user, team_stats)

  rehtie lr leftA $ \(team_user, team_stats) -> do
    rightA $ TeamPackResponse {
      teamPackResponseUser          = userToSanitizedResponse team_user,
      teamPackResponseUserId        = entityKeyToInt64 team_user,
      teamPackResponseTeam          = teamToResponse team,
      teamPackResponseTeamId        = keyToInt64 team_id,
      teamPackResponseStat          = team_stats,
      teamPackResponsePermissions   = emptyPermissions
    }
