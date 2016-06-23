{-# LANGUAGE RecordWildCards #-}

module Access (
  isOwnerOf_OrganizationIdM,
  isMemberOf_OrganizationIdM,
  isMemberOf_OrganizationId_TeamM,
  userTeamsOf_OrganizationIdM,
) where



import           Api.Params
import           Control
import           Import
import           LN.T.Membership
import           LN.T.Visibility
import           LN.T.Team
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
