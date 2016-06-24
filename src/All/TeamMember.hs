module All.TeamMember (
  -- Handler
  getTeamMembersR,
  postTeamMemberR0,
  getTeamMemberR,
  putTeamMemberR,
  deleteTeamMemberR,
  getTeamMembersCountR,

  -- Model/Function
  teamMemberRequestToTeamMember,
  teamMemberToResponse,
  teamMembersToResponses,

  -- Model/Internal
  getTeamMembersM,
  getTeamMembers_ByTeamIdM,
  getTeamMemberM,
  insertTeamMemberM,
  insertTeamMember_InternalM,
  updateTeamMemberM,
  deleteTeamMemberM,
) where



import           All.Prelude
import           LN.T.Membership



--
-- Handler
--

getTeamMembersR :: Handler Value
getTeamMembersR = run $ do
  user_id <- _requireAuthId
  (toJSON . teamMembersToResponses) <$> getTeamMembersM user_id



postTeamMemberR0 :: Handler Value
postTeamMemberR0 = run $ do
  user_id <- _requireAuthId
  team_member_request <- requireJsonBody :: HandlerEff TeamMemberRequest
  (toJSON . teamMemberToResponse) <$> insertTeamMemberM user_id team_member_request



getTeamMemberR :: TeamMemberId -> Handler Value
getTeamMemberR team_member_id = run $ do
  user_id <- _requireAuthId
  (toJSON . teamMemberToResponse) <$> getTeamMemberM user_id team_member_id



putTeamMemberR :: TeamMemberId -> Handler Value
putTeamMemberR team_member_id = run $ do
  user_id <- _requireAuthId
  team_member_request <- requireJsonBody
  (toJSON . teamMemberToResponse) <$> updateTeamMemberM user_id team_member_id team_member_request



deleteTeamMemberR :: TeamMemberId -> Handler Value
deleteTeamMemberR team_member_id = run $ do
  user_id <- _requireAuthId
  void $ deleteTeamMemberM user_id team_member_id
  pure $ toJSON ()



getTeamMembersCountR :: Handler Value
getTeamMembersCountR = run $ do
  user_id <- _requireAuthId
  toJSON <$> countTeamMembersM user_id







--
-- Model/Function
--

teamMemberRequestToTeamMember :: UserId -> TeamId -> TeamMemberRequest -> TeamMember
teamMemberRequestToTeamMember user_id team_id TeamMemberRequest{..} = TeamMember {
  teamMemberUserId      = user_id,
  teamMemberTeamId      = team_id,
  teamMemberIsAccepted  = False,
  teamMemberAcceptedAt  = Nothing,
  teamMemberIsBlocked   = False,
  teamMemberBlockedAt   = Nothing,
  teamMemberActive      = True,
  teamMemberGuard       = teamMemberRequestGuard,
  teamMemberCreatedAt   = Nothing,
  teamMemberModifiedBy  = Nothing,
  teamMemberModifiedAt  = Nothing,
  teamMemberActivityAt  = Nothing
}



teamMemberToResponse :: Entity TeamMember -> TeamMemberResponse
teamMemberToResponse (Entity team_memberid TeamMember{..}) = TeamMemberResponse {
  teamMemberResponseId          = keyToInt64 team_memberid,
  teamMemberResponseUserId      = keyToInt64 teamMemberUserId,
  teamMemberResponseTeamId      = keyToInt64 teamMemberTeamId,
  teamMemberResponseIsAccepted  = teamMemberIsAccepted,
  teamMemberResponseAcceptedAt  = teamMemberAcceptedAt,
  teamMemberResponseIsBlocked   = teamMemberIsBlocked,
  teamMemberResponseBlockedAt   = teamMemberBlockedAt,
  teamMemberResponseActive      = teamMemberActive,
  teamMemberResponseGuard       = teamMemberGuard,
  teamMemberResponseCreatedAt   = teamMemberCreatedAt,
  teamMemberResponseModifiedBy  = fmap keyToInt64 teamMemberModifiedBy,
  teamMemberResponseModifiedAt  = teamMemberModifiedAt,
  teamMemberResponseActivityAt  = teamMemberActivityAt
}



teamMembersToResponses :: [Entity TeamMember] -> TeamMemberResponses
teamMembersToResponses teamMembers = TeamMemberResponses {
  teamMemberResponses = map teamMemberToResponse teamMembers
}







--
-- Model/Internal
--

getTeamMembersM :: UserId -> HandlerEff [Entity TeamMember]
getTeamMembersM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spTeamId of
    Just team_id -> getTeamMembers_ByTeamIdM user_id team_id sp
    _            -> notFound



getTeamMembers_ByTeamIdM :: UserId -> TeamId -> StandardParams -> HandlerEff [Entity TeamMember]
getTeamMembers_ByTeamIdM _ team_id sp = do
  -- TODO ACCESS:
  selectListDb sp [TeamMemberTeamId ==. team_id] [] TeamMemberId



getTeamMemberM :: UserId -> TeamMemberId -> HandlerEff (Entity TeamMember)
getTeamMemberM _ team_member_id = do
  notFoundMaybe =<< selectFirstDb [ TeamMemberId ==. team_member_id ] []




insertTeamMemberM :: UserId -> TeamMemberRequest -> HandlerEff (Entity TeamMember)
insertTeamMemberM user_id team_member_request = do

  --  TODO FIXME
  sp@StandardParams{..} <- lookupStandardParams

  case (spOrganizationId, spTeamId) of
    (Just organization_id, _)  -> insertTeamMember_JoinM user_id organization_id team_member_request
    (_, Just team_id)          -> insertTeamMember_InternalM user_id team_id team_member_request
    (_, _)                     -> notFound



-- | Simple JOIN
-- Find Team_Members and insert this user into that team
--
insertTeamMember_JoinM :: UserId -> OrganizationId -> TeamMemberRequest -> HandlerEff (Entity TeamMember)
insertTeamMember_JoinM user_id organization_id team_member_request = do

  ts <- timestampH'

  (Entity team_id Team{..}) <- notFoundMaybe =<< selectFirstDb [ TeamOrgId ==. organization_id, TeamSystem ==. Team_Members, TeamActive ==. True ] []

  let
    teamMember = (teamMemberRequestToTeamMember user_id team_id team_member_request) { teamMemberCreatedAt = Just ts }

  (Entity _ Team{..}) <- notFoundMaybe =<< selectFirstDb [ TeamId ==. team_id ] []
  case teamMembership of
    Membership_Join -> insertEntityDb teamMember
    _               -> lift $ permissionDenied "Unable to join team"



-- | TODO ACCESS RESTRICTIONS
-- 1. Can only add to owners, by an owner
-- 2. Restrictions based on Membership
--
insertTeamMember_InternalM :: UserId -> TeamId -> TeamMemberRequest -> HandlerEff (Entity TeamMember)
insertTeamMember_InternalM user_id team_id team_member_request = do

  ts <- timestampH'

  let
    teamMember = (teamMemberRequestToTeamMember user_id team_id team_member_request) { teamMemberCreatedAt = Just ts }

  (Entity _ Team{..}) <- notFoundMaybe =<< selectFirstDb [ TeamId ==. team_id ] []
  case teamMembership of
    Membership_Join -> insertEntityDb teamMember
    _               -> lift $ permissionDenied "Unable to join team"



updateTeamMemberM :: UserId -> TeamMemberId -> TeamMemberRequest -> HandlerEff (Entity TeamMember)
updateTeamMemberM user_id team_memberid team_member_request = do

  ts <- timestampH'

  let
    TeamMember{..} = (teamMemberRequestToTeamMember user_id dummyId team_member_request) { teamMemberModifiedAt = Just ts }

  updateWhereDb
    [ TeamMemberUserId ==. user_id, TeamMemberId ==. team_memberid ]
    [ TeamMemberModifiedAt  =. teamMemberModifiedAt
    , TeamMemberGuard      +=. teamMemberGuard
    ]

  notFoundMaybe =<< selectFirstDb [ TeamMemberUserId ==. user_id, TeamMemberId ==. team_memberid ] []



deleteTeamMemberM :: UserId -> TeamMemberId -> HandlerEff ()
deleteTeamMemberM user_id team_memberid = do
  deleteWhereDb [ TeamMemberUserId ==. user_id, TeamMemberId ==. team_memberid ]



countTeamMembersM :: UserId -> HandlerEff CountResponses
countTeamMembersM _ = do

  StandardParams{..} <- lookupStandardParams

  case spUserId of

    Nothing             -> do
      notFound
--      n <- countDb [ TeamMemberId /=. 0 ]
--      return $ CountResponses [CountResponse (fromIntegral 0) (fromIntegral n)]

    Just lookup_user_id -> do
      n <- countDb [ TeamMemberUserId ==. lookup_user_id ]
      return $ CountResponses [CountResponse (keyToInt64 lookup_user_id) (fromIntegral n)]
