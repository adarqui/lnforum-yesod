{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.TeamMember (
  -- Handler
  getTeamMemberPacksR,
  getTeamMemberPackR,

  -- Model
) where



import           LN.All.Prelude
import           LN.All.TeamMember
import           LN.All.User



--
-- Handler
--

getTeamMemberPacksR :: Handler Value
getTeamMemberPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getTeamMemberPacksM (pure sp) user_id



getTeamMemberPackR :: TeamMemberId -> Handler Value
getTeamMemberPackR team_member_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getTeamMemberPackM user_id team_member_id




--
-- Model
--

getTeamMemberPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff TeamMemberPackResponses
getTeamMemberPacksM m_sp user_id = do

  case (lookupSpMay m_sp spTeamId) of

    Just team_id -> getTeamMemberPacks_ByTeamIdM m_sp user_id team_id
    _            -> left $ Error_InvalidArguments "team_id"



getTeamMemberPackM :: UserId -> TeamMemberId -> HandlerErrorEff TeamMemberPackResponse
getTeamMemberPackM user_id team_member_id = do

  e_team_member <- getTeamMemberM user_id team_member_id
  rehtie e_team_member left $ \team_member -> getTeamMemberPack_ByTeamMemberM user_id team_member




getTeamMemberPacks_ByTeamIdM :: Maybe StandardParams -> UserId -> TeamId -> HandlerErrorEff TeamMemberPackResponses
getTeamMemberPacks_ByTeamIdM m_sp user_id team_id = do

  e_team_members <- getTeamMembers_ByTeamIdM m_sp user_id team_id
  rehtie e_team_members left $ \team_members -> do
    team_member_packs <- rights <$> mapM (\team_member -> getTeamMemberPack_ByTeamMemberM user_id team_member) team_members
    right $ TeamMemberPackResponses {
      teamMemberPackResponses = team_member_packs
    }



getTeamMemberPack_ByTeamMemberM :: UserId -> Entity TeamMember -> HandlerErrorEff TeamMemberPackResponse
getTeamMemberPack_ByTeamMemberM user_id team_member@(Entity team_member_id TeamMember{..}) = do

  e_team_member_user <- getUserM user_id teamMemberUserId
  rehtie e_team_member_user left $ \team_member_user -> do

    right $ TeamMemberPackResponse {
      teamMemberPackResponseUser         = userToSanitizedResponse team_member_user,
      teamMemberPackResponseUserId       = entityKeyToInt64 team_member_user,
      teamMemberPackResponseTeamMember   = teamMemberToResponse team_member,
      teamMemberPackResponseTeamMemberId = keyToInt64 team_member_id,
      teamMemberPackResponsePermissions  = emptyPermissions
    }
