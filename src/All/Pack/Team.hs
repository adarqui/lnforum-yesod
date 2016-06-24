{-# LANGUAGE RecordWildCards #-}

module All.Pack.Team (
  -- Handler
  getTeamPacksR,
  getTeamPackR,
  getTeamPackH,

  -- Model
) where



import           All.Team
import           All.Prelude
import           All.User



--
-- Handler
--

getTeamPacksR :: Handler Value
getTeamPacksR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getTeamPacksM user_id



getTeamPackR :: TeamId -> Handler Value
getTeamPackR team_id = run $ do
  user_id <- _requireAuthId
  toJSON <$> getTeamPackM user_id team_id



getTeamPackH :: Text -> Handler Value
getTeamPackH team_name = run $ do
  user_id <- _requireAuthId
  toJSON <$> getTeamPackMH user_id team_name







-- Model

getTeamPacksM :: UserId -> HandlerEff TeamPackResponses
getTeamPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spOrganizationId, spUserId, spSelf) of

    (Just organization_id, _, _) -> getTeamPacks_ByOrganizationIdM user_id organization_id sp
    (_, Just lookup_user_id, _)  -> getTeamPacks_ByUserIdM user_id lookup_user_id sp
    (_, _, _)                    -> notFound



getTeamPackM :: UserId -> TeamId -> HandlerEff TeamPackResponse
getTeamPackM user_id team_id = do

  team         <- getTeamM user_id team_id
  getTeamPack_ByTeamM user_id team



getTeamPackMH :: UserId -> Text -> HandlerEff TeamPackResponse
getTeamPackMH user_id team_name = do

  sp@StandardParams{..} <- lookupStandardParams

  case spOrganizationId of
    Just organization_id -> do
      team <- getTeamMH user_id team_name organization_id
      getTeamPack_ByTeamM user_id team
    Nothing              -> notFound



getTeamPacks_ByUserIdM :: UserId -> UserId -> StandardParams -> HandlerEff TeamPackResponses
getTeamPacks_ByUserIdM user_id lookup_user_id sp = do

  teams       <- getTeams_ByUserIdM user_id lookup_user_id sp
  teams_packs <- mapM (\team -> getTeamPack_ByTeamM user_id team) teams
  return $ TeamPackResponses {
    teamPackResponses = teams_packs
  }



getTeamPacks_ByOrganizationIdM :: UserId -> OrganizationId -> StandardParams -> HandlerEff TeamPackResponses
getTeamPacks_ByOrganizationIdM user_id organization_id sp = do
  teams       <- getTeams_ByOrganizationIdM user_id organization_id sp
  teams_packs <- mapM (\team -> getTeamPack_ByTeamM user_id team) teams
  return $ TeamPackResponses {
    teamPackResponses = teams_packs
  }




getTeamPack_ByTeamM :: UserId -> Entity Team -> HandlerEff TeamPackResponse
getTeamPack_ByTeamM user_id team@(Entity team_id Team{..}) = do

  -- let sp = defaultStandardParams {
  --     spSortOrder = Just SortOrderBy_Dsc,
  --     spOrder     = Just OrderBy_ActivityAt,
  --     spLimit     = Just 1
  --   }

  team_user    <- getUserM user_id teamUserId
  team_stats   <- getTeamStatM user_id (entityKey team)

  return $ TeamPackResponse {
    teamPackResponseUser          = userToSanitizedResponse team_user,
    teamPackResponseUserId        = entityKeyToInt64 team_user,
    teamPackResponseTeam          = teamToResponse team,
    teamPackResponseTeamId        = keyToInt64 team_id,
    teamPackResponseStat          = team_stats,
    teamPackResponsePermissions   = emptyPermissions
  }
