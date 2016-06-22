module All.TeamMember (
  -- Handler
  getTeamMembersR,
  postTeamMemberR0,
  getTeamMemberR,
  getTeamMemberH,
  putTeamMemberR,
  deleteTeamMemberR,
  getCountTeamMembersR,

  -- Model/Function
  teamMemberRequestToTeamMember,
  teamMemberToResponse,
  teamMembersToResponses,

  -- Model/Internal
  getTeamMembersM,
  getTeamMembers_ByUserIdM,
  getTeamMembers_ByEverythingM,
  getTeamMemberM,
  getTeamMemberMH,
  getTeamMemberMH',
  insertTeamMemberM,
  insertTeamMember_BypassM,
  updateTeamMemberM,
  deleteTeamMemberM,
) where



import           All.Prelude



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



getTeamMemberH :: Text -> Handler Value
getTeamMemberH team_name = run $ do
  user_id <- _requireAuthId
  (toJSON . teamMemberToResponse) <$> getTeamMemberMH user_id team_name



putTeamMemberR :: TeamMemberId -> Handler Value
putTeamMemberR team_member_id = run $ do
  user_id <- _requireAuthId
  team_member_request <- requireJsonBody
  (toJSON . teamMemberToResponse) <$> updateTeamMemberM user_id team_member_id team_member_request



deleteTeamMemberR :: TeamMemberId -> Handler Value
deleteTeamMemberR team_member_id = run $ do
  user_id <- _requireAuthId
  void $ deleteTeamMemberM user_id team_member_id
  sendResponseStatus status200 ("DELETED" :: Text)



getCountTeamMembersR :: Handler Value
getCountTeamMembersR = run $ do
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

  case spUserId of
    Just lookup_user_id -> getTeamMembers_ByUserIdM user_id lookup_user_id sp
    _                   -> notFound



getTeamMembers_ByUserIdM :: UserId -> UserId -> StandardParams -> HandlerEff [Entity TeamMember]
getTeamMembers_ByUserIdM _ lookup_user_id sp = do
  selectListDb sp [TeamMemberUserId ==. lookup_user_id] [] TeamMemberId



getTeamMembers_ByEverythingM :: UserId -> StandardParams -> HandlerEff [Entity TeamMember]
getTeamMembers_ByEverythingM _ sp = do
  selectListDb sp [] [] TeamMemberId



getTeamMemberM :: UserId -> TeamMemberId -> HandlerEff (Entity TeamMember)
getTeamMemberM _ team_memberid = do
  notFoundMaybe =<< selectFirstDb [ TeamMemberId ==. team_memberid ] []



getTeamMemberMH :: UserId -> Text -> HandlerEff (Entity TeamMember)
getTeamMemberMH user_id team_member_name = do

  sp@StandardParams{..} <- lookupStandardParams

  case spUserId of

    Just lookup_user_id -> getTeamMember_ByUserIdMH user_id team_member_name user_id sp
    _                   -> getTeamMemberMH' user_id team_member_name sp



getTeamMember_ByUserIdMH :: UserId -> Text -> UserId -> StandardParams -> HandlerEff (Entity TeamMember)
getTeamMember_ByUserIdMH user_id team_member_name lookup_user_id _ = notFound



getTeamMemberMH' :: UserId -> Text -> StandardParams -> HandlerEff (Entity TeamMember)
getTeamMemberMH' user_id team_member_name _ = notFound



insertTeamMemberM :: UserId -> TeamMemberRequest -> HandlerEff (Entity TeamMember)
insertTeamMemberM user_id team_member_request = do

  --  TODO FIXME
  sp@StandardParams{..} <- lookupStandardParams

  case spTeamId of
    Nothing      -> notFound
    Just team_id -> insertTeamMember_BypassM user_id team_id team_member_request



insertTeamMember_BypassM :: UserId -> TeamId -> TeamMemberRequest -> HandlerEff (Entity TeamMember)
insertTeamMember_BypassM user_id team_id team_member_request = do

  ts <- timestampH'

  let
    teamMember = (teamMemberRequestToTeamMember user_id team_id team_member_request) { teamMemberCreatedAt = Just ts }

  insertEntityDb teamMember



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
