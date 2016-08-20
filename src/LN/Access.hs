{-# LANGUAGE RecordWildCards #-}

module LN.Access (
  isSuperM,
  mustBe_SuperM,
  mustBe_SameUserM,
  mustBe_OwnerOf_OrganizationIdM,
  mustBe_OwnerOf_ForumIdM,
  mustBe_OwnerOf_BoardIdM,
  mustBe_OwnerOf_ThreadIdM,
  mustBe_OwnerOf_ThreadPostIdM,
  mustBe_MemberOf_OrganizationIdM,
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



import           Control.Monad.Trans.Either (runEitherT)
import           Data.Ebyam                 (ebyam)
import           Data.List                  (nub)

import           LN.All.Internal
import           LN.Control
import           LN.Generate.Permission     (allPermissions)
import           LN.Import
import           LN.T.Permission
import           LN.T.Team
import           LN.T.Visibility



-- | Queries appSuperUsers (from Foundation)
-- If our user_id is found within this list, consider us a super user.
--
isSuperM :: UserId -> HandlerEff Bool
isSuperM user_id = do
  super_users <- getsYesod appSuperUsers
  pure $ any (\(Entity _ Super{..}) -> user_id == superUserId) super_users



mustBe_SuperM :: UserId -> HandlerErrorEff ()
mustBe_SuperM user_id = do
  super_users <- getsYesod appSuperUsers
  if (not $ any (\(Entity _ Super{..}) -> user_id == superUserId) super_users)
     then leftA Error_PermissionDenied
     else rightA ()



mustBe_SameUserM :: UserId -> UserId -> HandlerErrorEff ()
mustBe_SameUserM user_id lookup_user_id = do
  if user_id == lookup_user_id
    then rightA ()
    else leftA Error_PermissionDenied



mustBe_OwnerOf_OrganizationIdM :: UserId -> OrganizationId -> HandlerErrorEff ()
mustBe_OwnerOf_OrganizationIdM user_id org_id = do
  is_owner <- isOwnerOf_OrganizationIdM user_id org_id
  if is_owner
    then rightA ()
    else leftA Error_PermissionDenied



mustBe_MemberOf_OrganizationIdM :: UserId -> OrganizationId -> HandlerErrorEff ()
mustBe_MemberOf_OrganizationIdM user_id org_id = do
  is_owner <- isMemberOf_OrganizationIdM user_id org_id
  if is_owner
    then rightA ()
    else leftA Error_PermissionDenied



mustBe_OwnerOf_ForumIdM :: UserId -> ForumId -> HandlerErrorEff ()
mustBe_OwnerOf_ForumIdM user_id forum_id = do
  runEitherT $ do
    (Entity _ Forum{..}) <- mustT $ getForumM user_id forum_id
    mustT $ mustBe_OwnerOf_OrganizationIdM user_id forumOrgId



mustBe_OwnerOf_BoardIdM :: UserId -> BoardId -> HandlerErrorEff ()
mustBe_OwnerOf_BoardIdM user_id board_id = do
  runEitherT $ do
    (Entity _ Board{..}) <- mustT $ getBoardM user_id board_id
    mustT $ mustBe_OwnerOf_OrganizationIdM user_id boardOrgId



mustBe_OwnerOf_ThreadIdM :: UserId -> ThreadId -> HandlerErrorEff ()
mustBe_OwnerOf_ThreadIdM user_id thread_id = do
  -- TODO FIXME: Possibly broken? untested
  runEitherT $ do
    (Entity _ Thread{..}) <- mustT $ getThreadM user_id thread_id
    void $ mustT $ do
      choiceEitherM Error_PermissionDenied [mustBe_SameUserM user_id user_id, mustBe_OwnerOf_OrganizationIdM user_id threadOrgId]



mustBe_OwnerOf_ThreadPostIdM :: UserId -> ThreadPostId -> HandlerErrorEff ()
mustBe_OwnerOf_ThreadPostIdM user_id thread_post_id = do
  -- TODO FIXME: Possibly broken? untested
  runEitherT $ do
    (Entity _ ThreadPost{..}) <- mustT $ getThreadPostM user_id thread_post_id
    void $ mustT $
      choiceEitherM Error_PermissionDenied [mustBe_SameUserM user_id user_id, mustBe_OwnerOf_OrganizationIdM user_id threadPostOrgId]



isOwnerOf_OrganizationIdM :: UserId -> OrganizationId -> HandlerEff Bool
isOwnerOf_OrganizationIdM user_id org_id = do
  isMemberOf_OrganizationId_TeamM user_id org_id Team_Owners



isMemberOf_OrganizationIdM :: UserId -> OrganizationId -> HandlerEff Bool
isMemberOf_OrganizationIdM user_id org_id =
  isMemberOf_OrganizationId_TeamM user_id org_id Team_Members



isMemberOf_OrganizationId_TeamM :: UserId -> OrganizationId -> SystemTeam -> HandlerEff Bool
isMemberOf_OrganizationId_TeamM user_id org_id system_team = do

  m_teams <- getTeams_ByOrg_MaybeM user_id org_id
  ebyam m_teams (pure False) $ \teams -> do

    case (find (\(Entity _ Team{..}) -> teamSystem == system_team) teams) of
      Nothing                 -> pure False
      Just (Entity team_id _) -> maybe False (const True) <$> getTeamMember_ByTeam_MaybeM team_id user_id

    -- m_team <- selectFirstDb [TeamOrgId ==. org_id, TeamSystem ==. system_team, TeamActive ==. True] []
    -- ebyam m_team (pure False) $ \(Entity team_id Team{..}) -> do
    --   maybe False (const True) <$> selectFirstDb [TeamMemberTeamId ==. team_id, TeamMemberUserId ==. user_id, TeamMemberActive ==. True] []



userTeamsOf_OrganizationIdM :: UserId -> OrganizationId -> HandlerEff [Entity Team]
userTeamsOf_OrganizationIdM user_id org_id = do

  m_teams <- getTeams_ByOrg_MaybeM user_id org_id
  ebyam m_teams (pure []) $ \teams -> do

    catMaybes <$>
      (forM teams $ \team@(Entity team_id _) -> do
        maybe Nothing (const $ Just team) <$> getTeamMember_ByTeam_MaybeM team_id user_id)

  -- teams <- selectListDb Nothing [TeamOrgId ==. org_id, TeamActive ==. True] [] TeamId
  -- catMaybes <$> mapM (\team@(Entity team_id _) -> do
  --   maybe Nothing (const $ Just team) <$> selectFirstDb [TeamMemberTeamId ==. team_id, TeamMemberUserId ==. user_id, TeamMemberActive ==. True] [])
  --   teams



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
  m_org <- getOrganizationMaybeM user_id org_id
  ebyam m_org (pure []) $ \(Entity _ Organization{..}) -> do
    user_teams <- userTeamsOf_OrganizationIdM user_id org_id
    case user_teams of
      [] -> pure $ if organizationVisibility == Public then [Perm_Read] else []
      xs -> pure $ organizationPermissions_ByTeamsM xs



userPermissions_ByForumIdM :: UserId -> ForumId -> HandlerEff Permissions
userPermissions_ByForumIdM user_id forum_id = do
  m_forum <- getForumMaybeM user_id forum_id
  ebyam m_forum (pure []) $ \(Entity _ Forum{..}) -> do
   userPermissions_ByOrganizationIdM user_id forumOrgId



userPermissions_ByBoardIdM :: UserId -> BoardId -> HandlerEff Permissions
userPermissions_ByBoardIdM user_id board_id = do
  m_board <- getBoardMaybeM user_id board_id
  ebyam m_board (pure []) $ \(Entity _ Board{..}) -> do
   userPermissions_ByOrganizationIdM user_id boardOrgId



userPermissions_ByThreadIdM :: UserId -> ThreadId -> HandlerEff Permissions
userPermissions_ByThreadIdM user_id thread_id = do
  m_thread <- getThreadMaybeM user_id thread_id
  ebyam m_thread (pure []) $ \(Entity _ Thread{..}) -> do
    userPermissions_ByOrganizationIdM user_id threadOrgId



userPermissions_ByThreadPostIdM :: UserId -> ThreadPostId -> HandlerEff Permissions
userPermissions_ByThreadPostIdM user_id thread_post_id = do
  m_thread_post <- getThreadPostMaybeM user_id thread_post_id
  ebyam m_thread_post (pure []) $ \(Entity _ ThreadPost{..}) -> do
    userPermissions_ByOrganizationIdM user_id threadPostOrgId
