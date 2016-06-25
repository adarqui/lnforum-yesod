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
  userPermissions_ByForumIdM,
  userPermissions_ByBoardIdM,
  userPermissions_ByThreadIdM,
  userPermissions_ByThreadPostIdM
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
userTeamsOf_OrganizationIdM user_id org_id = do

  teams <- selectListDb'' [ TeamOrgId ==. org_id, TeamActive ==. True ] [] TeamId
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



--
-- TODO FIXME: HACKING STUFF UP, JUST TO GET STUFF WORKING
--



-- | Calculates permissions based on a user's membership of an Organization.
-- If a user is not a member, calculates permissions based on the Organization's Visibility
--
userPermissions_ByOrganizationIdM :: UserId -> OrganizationId -> HandlerEff Permissions
userPermissions_ByOrganizationIdM user_id org_id = do
  org <- selectFirstDb [ OrganizationId ==. org_id ] []
  case org of
    Nothing -> pure []
    Just (Entity org_id Organization{..}) -> do
      user_teams <- userTeamsOf_OrganizationIdM user_id org_id
      case user_teams of
        [] -> pure $ if organizationVisibility == Public then [Perm_Read] else []
        xs -> pure $ organizationPermissions_ByTeamsM xs



userPermissions_ByForumIdM :: UserId -> ForumId -> HandlerEff Permissions
userPermissions_ByForumIdM user_id forum_id = do
  m_forum <- selectFirstDb [ ForumId ==. forum_id ] []
  case m_forum of
    Nothing -> pure []
    Just (Entity _ Forum{..}) -> userPermissions_ByOrganizationIdM user_id forumOrgId



userPermissions_ByBoardIdM :: UserId -> BoardId -> HandlerEff Permissions
userPermissions_ByBoardIdM user_id board_id = do
  m_board <- selectFirstDb [ BoardId ==. board_id ] []
  case m_board of
    Nothing -> pure []
    Just (Entity _ Board{..}) -> userPermissions_ByOrganizationIdM user_id boardOrgId



userPermissions_ByThreadIdM :: UserId -> ThreadId -> HandlerEff Permissions
userPermissions_ByThreadIdM user_id thread_id = do
  m_thread <- selectFirstDb [ ThreadId ==. thread_id ] []
  case m_thread of
    Nothing -> pure []
    Just (Entity _ Thread{..}) -> userPermissions_ByOrganizationIdM user_id threadOrgId



userPermissions_ByThreadPostIdM :: UserId -> ThreadPostId -> HandlerEff Permissions
userPermissions_ByThreadPostIdM user_id thread_post_id = do
  m_thread_post <- selectFirstDb [ ThreadPostId ==. thread_post_id ] []
  case m_thread_post of
    Nothing -> pure []
    Just (Entity _ ThreadPost{..}) -> userPermissions_ByOrganizationIdM user_id threadPostOrgId
