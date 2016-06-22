{-# LANGUAGE RecordWildCards #-}

module All.Pack.TeamMember (
  -- Handler
  getTeamMemberPacksR,
  getTeamMemberPackR,
  getTeamMemberPackH,

  -- Model

) where



import           All.Prelude
import           All.TeamMember
import           All.User



--
-- Handler
--

getTeamMemberPacksR :: Handler Value
getTeamMemberPacksR = do
  user_id <- requireAuthId
  toJSON <$> getTeamMemberPacksM user_id



getTeamMemberPackR :: TeamMemberId -> Handler Value
getTeamMemberPackR team_member_id = do
  user_id <- requireAuthId
  toJSON <$> getTeamMemberPackM user_id team_member_id



getTeamMemberPackH :: Text -> Handler Value
getTeamMemberPackH team_member_name = do
  user_id <- requireAuthId
  toJSON <$> getTeamMemberPackMH user_id team_member_name






--
-- Model
--

getTeamMemberPacksM :: UserId -> Handler TeamMemberPackResponses
getTeamMemberPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spUserId of

    Just lookup_user_id -> getTeamMemberPacks_ByUserIdM user_id lookup_user_id sp
    _                   -> getTeamMemberPacks_ByEverythingM user_id sp



getTeamMemberPackM :: UserId -> TeamMemberId -> Handler TeamMemberPackResponse
getTeamMemberPackM user_id team_member_id = do

  teamMember         <- getTeamMemberM user_id team_member_id
  getTeamMemberPack_ByTeamMemberM user_id teamMember



getTeamMemberPackMH :: UserId -> Text -> Handler TeamMemberPackResponse
getTeamMemberPackMH user_id team_member_name = do

  teamMember         <- getTeamMemberMH user_id team_member_name
  getTeamMemberPack_ByTeamMemberM user_id teamMember



getTeamMemberPacks_ByEverythingM :: UserId -> StandardParams -> Handler TeamMemberPackResponses
getTeamMemberPacks_ByEverythingM user_id sp = do
  teamMembers       <- getTeamMembers_ByEverythingM user_id sp
  teamMembers_packs <- mapM (\teamMember -> getTeamMemberPack_ByTeamMemberM user_id teamMember) teamMembers
  return $ TeamMemberPackResponses {
    teamMemberPackResponses = teamMembers_packs
  }



getTeamMemberPacks_ByUserIdM :: UserId -> UserId -> StandardParams -> Handler TeamMemberPackResponses
getTeamMemberPacks_ByUserIdM user_id lookup_user_id sp = do

  teamMembers       <- getTeamMembers_ByUserIdM user_id lookup_user_id sp
  teamMembers_packs <- mapM (\teamMember -> getTeamMemberPack_ByTeamMemberM user_id teamMember) teamMembers
  return $ TeamMemberPackResponses {
    teamMemberPackResponses = teamMembers_packs
  }



getTeamMemberPack_ByTeamMemberM :: UserId -> Entity TeamMember -> Handler TeamMemberPackResponse
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
