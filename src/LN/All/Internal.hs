{-# LANGUAGE RecordWildCards #-}

module LN.All.Internal (
  getOrganizationM,
  getOrganizationMaybeM,
  getTeamM,
  getTeamMaybeM,
  getUserM,
  getUserMaybeM,
  getForumM,
  getForumMaybeM,
  getBoardM,
  getBoardMaybeM,
  getThreadM,
  getThreadMaybeM,
  getThreadPostM,
  getThreadPostMaybeM,
  getTeams_ByOrgM,
  getTeams_ByOrg_MaybeM,
  getTeamMember_ByTeamM,
  getTeamMember_ByTeam_MaybeM
) where



import           Data.Ebyam                 as A (ebyam)

import           LN.Cache                   as A
import           LN.Cache.Internal
import           LN.Control                 as A
import           LN.Db                      as A
import           LN.Import                  as A




getOrganizationM :: UserId -> OrganizationId -> HandlerErrorEff (Entity Organization)
getOrganizationM user_id organization_id = do
  maybe (leftA Error_NotFound) rightA =<< getOrganizationMaybeM user_id organization_id



getOrganizationMaybeM :: UserId -> OrganizationId -> HandlerEff (Maybe (Entity Organization))
getOrganizationMaybeM _ organization_id = do
  m_c_organization <- getOrganizationC organization_id
  cacheRunMaybe' m_c_organization $ do
    m_organization <- selectFirstDb [OrganizationId ==. organization_id, OrganizationActive ==. True] []
    ebyam
      m_organization
      (putOrganizationC organization_id CacheMissing *> pure Nothing)
      (\organization -> putOrganizationC organization_id (CacheEntry organization) *> pure (Just organization))



getTeamM :: UserId -> TeamId -> HandlerErrorEff (Entity Team)
getTeamM user_id team_id = do
  maybe (leftA Error_NotFound) rightA =<< getTeamMaybeM user_id team_id



getTeamMaybeM :: UserId -> TeamId -> HandlerEff (Maybe (Entity Team))
getTeamMaybeM _ team_id = do
  m_c_team <- getTeamC team_id
  cacheRunMaybe' m_c_team $ do
    m_team <- selectFirstDb [TeamId ==. team_id, TeamActive ==. True] []
    ebyam
      m_team
      (putTeamC team_id CacheMissing *> pure Nothing)
      (\team -> putTeamC team_id (CacheEntry team) *> pure (Just team))



getUserM :: UserId -> UserId -> HandlerErrorEff (Entity User)
getUserM user_id lookup_user_id = do
  maybe (leftA Error_NotFound) rightA =<< getUserMaybeM user_id lookup_user_id



getUserMaybeM :: UserId -> UserId -> HandlerEff (Maybe (Entity User))
getUserMaybeM _ lookup_user_id = do
  m_c_user <- getUserC lookup_user_id
  cacheRunMaybe' m_c_user $ do
    m_user <- selectFirstDb [UserId ==. lookup_user_id, UserActive ==. True] []
    ebyam
      m_user
      (putUserC lookup_user_id CacheMissing *> pure Nothing)
      (\user -> putUserC lookup_user_id (CacheEntry user) *> pure (Just user))



getForumM :: UserId -> ForumId -> HandlerErrorEff (Entity Forum)
getForumM user_id forum_id = do
  maybe (leftA Error_NotFound) rightA =<< getForumMaybeM user_id forum_id



getForumMaybeM :: UserId -> ForumId -> HandlerEff (Maybe (Entity Forum))
getForumMaybeM _ forum_id = do
  m_c_forum <- getForumC forum_id
  cacheRunMaybe' m_c_forum $ do
    m_forum <- selectFirstDb [ForumId ==. forum_id, ForumActive ==. True] []
    ebyam
      m_forum
      (putForumC forum_id CacheMissing *> pure Nothing)
      (\forum -> putForumC forum_id (CacheEntry forum) *> pure (Just forum))



getBoardM :: UserId -> BoardId -> HandlerErrorEff (Entity Board)
getBoardM user_id board_id = do
  maybe (leftA Error_NotFound) rightA =<< getBoardMaybeM user_id board_id



getBoardMaybeM :: UserId -> BoardId -> HandlerEff (Maybe (Entity Board))
getBoardMaybeM _ board_id = do
  m_c_board <- getBoardC board_id
  cacheRunMaybe' m_c_board $ do
    m_board <- selectFirstDb [BoardId ==. board_id, BoardActive ==. True] []
    ebyam
      m_board
      (putBoardC board_id CacheMissing *> pure Nothing)
      (\board -> putBoardC board_id (CacheEntry board) *> pure (Just board))



getThreadM :: UserId -> ThreadId -> HandlerErrorEff (Entity Thread)
getThreadM user_id thread_id = do
  maybe (leftA Error_NotFound) rightA =<< getThreadMaybeM user_id thread_id



getThreadMaybeM :: UserId -> ThreadId -> HandlerEff (Maybe (Entity Thread))
getThreadMaybeM _ thread_id = do
  m_c_thread <- getThreadC thread_id
  cacheRunMaybe' m_c_thread $ do
    m_thread <- selectFirstDb [ThreadId ==. thread_id, ThreadActive ==. True] []
    ebyam
      m_thread
      (putThreadC thread_id CacheMissing *> pure Nothing)
      (\thread -> putThreadC thread_id (CacheEntry thread) *> pure (Just thread))



getThreadPostM :: UserId -> ThreadPostId -> HandlerErrorEff (Entity ThreadPost)
getThreadPostM user_id post_id = do
  (maybe (leftA Error_NotFound) rightA) =<< getThreadPostMaybeM user_id post_id

  -- m_c_forum <- getThreadPostC forum_id
  -- cacheRun' m_c_forum $ do
  --   lr <- selectFirstDbE [ThreadPostId ==. forum_id, ThreadPostActive ==. True] []
  --   rehtie
  --     lr
  --     (\err   -> putThreadPostC forum_id CacheMissing *> leftA err)
  --     (\forum -> putThreadPostC forum_id (CacheEntry forum) *> rightA forum)



getThreadPostMaybeM :: UserId -> ThreadPostId -> HandlerEff (Maybe (Entity ThreadPost))
getThreadPostMaybeM _ post_id = do
  m_c_post <- getThreadPostC post_id
  cacheRunMaybe' m_c_post $ do
    m_post <- selectFirstDb [ThreadPostId ==. post_id, ThreadPostActive ==. True] []
    ebyam
      m_post
      (putThreadPostC post_id CacheMissing *> pure Nothing)
      (\post -> putThreadPostC post_id (CacheEntry post) *> pure (Just post))




getTeams_ByOrgM :: UserId -> OrganizationId -> HandlerErrorEff [Entity Team]
getTeams_ByOrgM user_id org_id = do
  (maybe (leftA Error_NotFound) rightA) =<< getTeams_ByOrg_MaybeM user_id org_id



getTeams_ByOrg_MaybeM :: UserId -> OrganizationId -> HandlerEff (Maybe [Entity Team])
getTeams_ByOrg_MaybeM _ org_id = do
  m_c_teams <- getTeams_ByOrgC org_id
  cacheRunMaybe' m_c_teams $ do
    teams <- selectListDb Nothing [TeamOrgId ==. org_id, TeamActive ==. True] [] TeamId
    putTeams_ByOrgC org_id (CacheEntry teams) *> pure (Just teams)



getTeamMember_ByTeamM :: TeamId -> UserId -> HandlerErrorEff (Entity TeamMember)
getTeamMember_ByTeamM team_id lookup_user_id = do
  (maybe (leftA Error_NotFound) rightA) =<< getTeamMember_ByTeam_MaybeM team_id lookup_user_id



getTeamMember_ByTeam_MaybeM :: TeamId -> UserId -> HandlerEff (Maybe (Entity TeamMember))
getTeamMember_ByTeam_MaybeM team_id lookup_user_id = do
  m_c_team_members <- getTeamMember_ByTeamC team_id lookup_user_id
  cacheRunMaybe' m_c_team_members $ do
    m_team_member <- selectFirstDb [TeamMemberTeamId ==. team_id, TeamMemberUserId ==. lookup_user_id, TeamMemberActive ==. True] []
    ebyam
      m_team_member
      (putTeamMember_ByTeamC team_id lookup_user_id CacheMissing *> pure Nothing)
      (\team_member -> putTeamMember_ByTeamC team_id lookup_user_id (CacheEntry team_member) *> pure (Just team_member))
