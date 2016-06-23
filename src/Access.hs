{-# LANGUAGE RecordWildCards #-}

module Access (
  isOwnerOf_OrganizationIdM,
  isMemberOf_OrganizationIdM,
  isMemberOf_OrganizationId_TeamM,
  userTeamsOf_OrganizationIdM,
  organizationPermissions_BySystemTeamM,
  organizationPermissions_BySystemTeamsM,
  organizationPermissions_ByTeamsM,
  userPermissions_ByOrganizationIdM,
) where



import           Api.Params
import           Control
import           Data.List       (nub)
import           Import
import           LN.T.Membership
import           LN.T.Permission
import           LN.T.Team
import           LN.T.Visibility
import           Model.Misc



isOwnerOf_OrganizationIdM :: UserId -> OrganizationId -> HandlerEff Bool
isOwnerOf_OrganizationIdM user_id org_id = do
  isMemberOf_OrganizationId_TeamM user_id org_id Team_Owners



isMemberOf_OrganizationIdM :: UserId -> OrganizationId -> HandlerEff Bool
isMemberOf_OrganizationIdM user_id org_id =
  isMemberOf_OrganizationId_TeamM user_id org_id Team_Members



isMemberOf_OrganizationId_TeamM :: UserId -> OrganizationId -> SystemTeam -> HandlerEff Bool
isMemberOf_OrganizationId_TeamM user_id org_id system_team = do

  m_team <- selectFirstDb [ TeamOrgId ==. org_id, TeamSystem ==. system_team, TeamActive ==. True ] []
  case m_team of
    Nothing                        -> pure False
    Just (Entity team_id Team{..}) -> do
      maybe False (const True) <$> selectFirstDb [ TeamMemberTeamId ==. team_id, TeamMemberUserId ==. user_id, TeamMemberActive ==. True] []



userTeamsOf_OrganizationIdM :: UserId -> OrganizationId -> HandlerEff [Entity Team]
userTeamsOf_OrganizationIdM user_id organization_id = do

  teams <- selectListDb'' [ TeamOrgId ==. organization_id, TeamActive ==. True ] [] TeamId
  catMaybes <$> mapM (\team@(Entity team_id _) -> do
    maybe Nothing (const $ Just team) <$> selectFirstDb [ TeamMemberTeamId ==. team_id, TeamMemberUserId ==. user_id ] [])
    teams



organizationPermissions_BySystemTeamM :: SystemTeam -> Permissions
organizationPermissions_BySystemTeamM team =
  case team of
    Team_Owners -> allPermissions
    Team_Members -> [Perm_Read]



organizationPermissions_BySystemTeamsM :: [SystemTeam] -> Permissions
organizationPermissions_BySystemTeamsM = nub . concatMap organizationPermissions_BySystemTeamM



organizationPermissions_ByTeamsM :: [Entity Team] -> Permissions
organizationPermissions_ByTeamsM = organizationPermissions_BySystemTeamsM . map (teamSystem . entityVal)




-- | Calculates permissions based on a user's membership of an Organization.
-- If a user is not a member, calculates permissions based on the Organization's Visibility
--
userPermissions_ByOrganizationIdM :: UserId -> OrganizationId -> HandlerEff Permissions
userPermissions_ByOrganizationIdM user_id organization_id = do
  org <- selectFirstDb [ OrganizationId ==. organization_id ] []
  case org of
    Nothing -> pure []
    Just (Entity organization_id Organization{..}) -> do
      user_teams <- userTeamsOf_OrganizationIdM user_id organization_id
      case user_teams of
        [] -> pure $ if organizationVisibility == Public then [Perm_Read] else []
        xs -> pure $ organizationPermissions_ByTeamsM xs
