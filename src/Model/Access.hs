module Model.Access (
  isOwnerOf_OrganizationIdM
) where



import           Import
import           Api.Params
import           Model.Misc
import           LN.T.Membership
import           LN.T.Visibility



isOwnerOf_OrganizationIdM :: UserId -> OrganizationId -> Handler Bool
isOwnerOf_OrganizationIdM user_id org_id = do

  m_team <- selectFirstDb [ TeamOrgId ==. org_id, TeamName ==. "owners", TeamActive ==. True ] []
  case m_team of
    Nothing                        -> pure False
    Just (Entity team_id Team{..}) -> do
      maybe False (const True) <$>  selectFirstDb [ TeamMemberTeamId ==. team_id, TeamMemberUserId ==. user_id, TeamMemberActive ==. True] []
