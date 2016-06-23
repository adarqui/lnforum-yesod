{-# LANGUAGE RecordWildCards #-}

module All.Pack.TeamMember (
  -- Handler
  getTeamMemberPacksR,
  getTeamMemberPackR,

  -- Model
) where



import           All.Prelude
import           All.TeamMember
import           All.User



--
-- Handler
--

getTeamMemberPacksR :: Handler Value
getTeamMemberPacksR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getTeamMemberPacksM user_id



getTeamMemberPackR :: TeamMemberId -> Handler Value
getTeamMemberPackR team_member_id = run $ do
  user_id <- _requireAuthId
  toJSON <$> getTeamMemberPackM user_id team_member_id




--
-- Model
--

getTeamMemberPacksM :: UserId -> HandlerEff TeamMemberPackResponses
getTeamMemberPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spTeamId of

    Just team_id -> getTeamMemberPacks_ByTeamIdM user_id team_id sp
    _            -> notFound



getTeamMemberPackM :: UserId -> TeamMemberId -> HandlerEff TeamMemberPackResponse
getTeamMemberPackM user_id team_member_id = do

  teamMember         <- getTeamMemberM user_id team_member_id
  getTeamMemberPack_ByTeamMemberM user_id teamMember




getTeamMemberPacks_ByTeamIdM :: UserId -> TeamId -> StandardParams -> HandlerEff TeamMemberPackResponses
getTeamMemberPacks_ByTeamIdM user_id team_id sp = do

  team_members      <- getTeamMembers_ByTeamIdM user_id team_id sp
  team_member_packs <- mapM (\team_member -> getTeamMemberPack_ByTeamMemberM user_id team_member) team_members
  return $ TeamMemberPackResponses {
    teamMemberPackResponses = team_member_packs
  }



getTeamMemberPack_ByTeamMemberM :: UserId -> Entity TeamMember -> HandlerEff TeamMemberPackResponse
getTeamMemberPack_ByTeamMemberM user_id team_member@(Entity team_member_id TeamMember{..}) = do

  -- let sp = defaultStandardParams {
  --     spSortOrder = Just SortOrderBy_Dsc,
  --     spOrder     = Just OrderBy_ActivityAt,
  --     spLimit     = Just 1
  --   }

  team_member_user    <- getUserM user_id teamMemberUserId

  return $ TeamMemberPackResponse {
    teamMemberPackResponseUser         = userToSanitizedResponse team_member_user,
    teamMemberPackResponseUserId       = entityKeyToInt64 team_member_user,
    teamMemberPackResponseTeamMember   = teamMemberToResponse team_member,
    teamMemberPackResponseTeamMemberId = keyToInt64 team_member_id,
    teamMemberPackResponsePermissions  = emptyPermissions
  }
