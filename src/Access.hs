module Access (
  isOwnerOf_OrganizationIdM,
  isMemberOf_OrganizationIdM,
  isMemberOf_OrganizationId_TeamNameM
) where



import           Import
import           Api.Params
import           Model.Misc
import           LN.T.Membership
import           LN.T.Visibility



isOwnerOf_OrganizationIdM :: UserId -> OrganizationId -> Handler Bool
isOwnerOf_OrganizationIdM user_id org_id = do
  isMemberOf_OrganizationId_TeamNameM user_id org_id "owners"



isMemberOf_OrganizationIdM :: UserId -> OrganizationId -> Handler Bool
isMemberOf_OrganizationIdM user_id org_id =
  isMemberOf_OrganizationId_TeamNameM user_id org_id "members"



isMemberOf_OrganizationId_TeamNameM :: UserId -> OrganizationId -> Text -> Handler Bool
isMemberOf_OrganizationId_TeamNameM user_id org_id team_name = do

  m_team <- selectFirstDb [ TeamOrgId ==. org_id, TeamName ==. team_name, TeamActive ==. True ] []
  case m_team of
    Nothing                        -> pure False
    Just (Entity team_id Team{..}) -> do
      maybe False (const True) <$>  selectFirstDb [ TeamMemberTeamId ==. team_id, TeamMemberUserId ==. user_id, TeamMemberActive ==. True] []
