module All.TeamMember (
  -- handler
  getTeamMembersR,
  postTeamMemberR0,
  getTeamMemberR,
  getTeamMemberH,
  putTeamMemberR,
  deleteTeamMemberR,

  getCountTeamMembersR,

  getTeamMemberStatsR,
  getTeamMemberStatR,

  getTeamMemberPacksR,
  getTeamMemberPackR,
  getTeamMemberPackH,



  -- model/functions
  teamMemberRequestToTeamMember,
  teamMemberToResponse,
  teamMembersToResponses,



  -- model/internal
  getTeamMembersM,
  getTeamMembers_ByUserIdM,
  getTeamMembers_ByEverythingM,

  getTeamMemberM,
  insertTeamMemberM,
  insertTeamMember_BypassM,
  updateTeamMemberM,
  deleteTeamMemberM,


  -- model/pack

) where



import           Handler.Prelude
import           Model.Prelude



getTeamMembersR :: Handler Value
getTeamMembersR = do
  user_id <- requireAuthId
  (toJSON . teamMembersToResponses) <$> getTeamMembersM user_id



postTeamMemberR0 :: Handler Value
postTeamMemberR0 = do

  user_id <- requireAuthId

  team_member_request <- requireJsonBody :: Handler TeamMemberRequest
  (toJSON . teamMemberToResponse) <$> insertTeamMemberM user_id team_member_request



getTeamMemberR :: TeamMemberId -> Handler Value
getTeamMemberR team_member_id = do
  user_id <- requireAuthId
  (toJSON . teamMemberToResponse) <$> getTeamMemberM user_id team_member_id



getTeamMemberH :: Text -> Handler Value
getTeamMemberH team_name = do
  user_id <- requireAuthId
  (toJSON . teamMemberToResponse) <$> getTeamMemberMH user_id team_name



putTeamMemberR :: TeamMemberId -> Handler Value
putTeamMemberR team_member_id = do
  user_id <- requireAuthId
  team_member_request <- requireJsonBody
  (toJSON . teamMemberToResponse) <$> updateTeamMemberM user_id team_member_id team_member_request



deleteTeamMemberR :: TeamMemberId -> Handler Value
deleteTeamMemberR team_member_id = do
  user_id <- requireAuthId
  void $ deleteTeamMemberM user_id team_member_id
  sendResponseStatus status200 ("DELETED" :: Text)



getCountTeamMembersR :: Handler Value
getCountTeamMembersR = do
  user_id <- requireAuthId
  toJSON <$> countTeamMembersM user_id



getTeamMemberStatsR :: Handler Value
getTeamMemberStatsR = do
  user_id <- requireAuthId
  toJSON <$> getTeamMemberStatsM user_id



getTeamMemberStatR :: TeamMemberId -> Handler Value
getTeamMemberStatR team_member_id = do
  user_id <- requireAuthId
  toJSON <$> getTeamMemberStatM user_id team_member_id



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



-- model/function



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



-- model/internal

getTeamMembersM :: UserId -> Handler [Entity TeamMember]
getTeamMembersM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spUserId of
    Just lookup_user_id -> getTeamMembers_ByUserIdM user_id lookup_user_id sp
    _                   -> notFound



getTeamMembers_ByUserIdM :: UserId -> UserId -> StandardParams -> Handler [Entity TeamMember]
getTeamMembers_ByUserIdM _ lookup_user_id sp = do
  selectListDb sp [TeamMemberUserId ==. lookup_user_id] [] TeamMemberId



getTeamMembers_ByEverythingM :: UserId -> StandardParams -> Handler [Entity TeamMember]
getTeamMembers_ByEverythingM _ sp = do
  selectListDb sp [] [] TeamMemberId



getTeamMemberM :: UserId -> TeamMemberId -> Handler (Entity TeamMember)
getTeamMemberM _ team_memberid = do
  notFoundMaybe =<< selectFirstDb [ TeamMemberId ==. team_memberid ] []



getTeamMemberMH :: UserId -> Text -> Handler (Entity TeamMember)
getTeamMemberMH user_id team_member_name = do

  sp@StandardParams{..} <- lookupStandardParams

  case spUserId of

    Just lookup_user_id -> getTeamMember_ByUserIdMH user_id team_member_name user_id sp
    _                   -> getTeamMemberMH' user_id team_member_name sp



getTeamMember_ByUserIdMH :: UserId -> Text -> UserId -> StandardParams -> Handler (Entity TeamMember)
getTeamMember_ByUserIdMH user_id team_member_name lookup_user_id _ = notFound



getTeamMemberMH' :: UserId -> Text -> StandardParams -> Handler (Entity TeamMember)
getTeamMemberMH' user_id team_member_name _ = notFound



insertTeamMemberM :: UserId -> TeamMemberRequest -> Handler (Entity TeamMember)
insertTeamMemberM user_id team_member_request = do

  --  TODO FIXME
  sp@StandardParams{..} <- lookupStandardParams

  case spTeamId of
    Nothing      -> notFound
    Just team_id -> insertTeamMember_BypassM user_id team_id team_member_request



insertTeamMember_BypassM :: UserId -> TeamId -> TeamMemberRequest -> Handler (Entity TeamMember)
insertTeamMember_BypassM user_id team_id team_member_request = do

  ts <- timestampH'

  let
    teamMember = (teamMemberRequestToTeamMember user_id team_id team_member_request) { teamMemberCreatedAt = Just ts }

  insertEntityDb teamMember



updateTeamMemberM :: UserId -> TeamMemberId -> TeamMemberRequest -> Handler (Entity TeamMember)
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



deleteTeamMemberM :: UserId -> TeamMemberId -> Handler ()
deleteTeamMemberM user_id team_memberid = do
  deleteWhereDb [ TeamMemberUserId ==. user_id, TeamMemberId ==. team_memberid ]



countTeamMembersM :: UserId -> Handler CountResponses
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



getTeamMemberStatsM :: UserId -> Handler TeamMemberStatResponses
getTeamMemberStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spBoardId of

    Just _  -> notFound
    Nothing -> notFound



getTeamMemberStatM :: UserId -> TeamMemberId -> Handler TeamMemberStatResponse
getTeamMemberStatM _ team_member_id = do

  return $ TeamMemberStatResponse {
  }



-- model/pack


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
getTeamMemberPack_ByTeamMemberM user_id teamMember = do

  -- let sp = defaultStandardParams {
  --     spSortOrder = Just SortOrderBy_Dsc,
  --     spOrder     = Just OrderBy_ActivityAt,
  --     spLimit     = Just 1
  --   }

  team_member_stats   <- getTeamMemberStatM user_id (entityKey teamMember)

  return $ TeamMemberPackResponse {
    teamMemberPackResponseTeamMember   = teamMemberToResponse teamMember,
    teamMemberPackResponseTeamMemberId = team_member_id
--    teamMemberPackResponseStat    = team_member_stats
  }
  where
  team_member_id = entityKeyToInt64 teamMember
