{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}

module Model.Organization.Internal (
  getOrganizationsM,
  getOrganizations_ByUserIdM,
  getOrganizations_ByEverythingM,

  getOrganizationM,
  getOrganizationMH,
  getOrganization_ByOrganizationNameM,
  insertOrganizationM,
  updateOrganizationM,
  deleteOrganizationM,
  deleteOrganizationTeamsM,

  countOrganizationsM,

  getOrganizationStatsM,
  getOrganizationStatM
) where



import           All.TeamMember
import           Model.Prelude
import           Model.Organization.Function
import           Model.Team.Internal
import           LN.T.Membership
import           LN.T.Visibility



getOrganizationsM :: UserId -> Handler [Entity Organization]
getOrganizationsM user_id = do
  sp@StandardParams{..} <- lookupStandardParams
  case spUserId of
    Just lookup_user_id -> getOrganizations_ByUserIdM user_id lookup_user_id sp
    Nothing             -> getOrganizations_ByEverythingM user_id sp



getOrganizations_ByUserIdM :: UserId -> UserId -> StandardParams -> Handler [Entity Organization]
getOrganizations_ByUserIdM _ lookup_user_id sp = do
  selectListDb sp [OrganizationUserId ==. lookup_user_id] [] OrganizationId



getOrganizations_ByEverythingM :: UserId -> StandardParams -> Handler [Entity Organization]
getOrganizations_ByEverythingM _ sp = do
  selectListDb sp [] [] OrganizationId



getOrganizationM :: UserId -> OrganizationId -> Handler (Entity Organization)
getOrganizationM user_id org_id = getOrganizationM' user_id (OrganizationId ==. org_id)



getOrganizationMH :: UserId -> Text -> Handler (Entity Organization)
getOrganizationMH user_id org_name = getOrganizationM' user_id (OrganizationName ==. org_name)



getOrganization_ByOrganizationNameM :: UserId -> Text -> Handler (Entity Organization)
getOrganization_ByOrganizationNameM _ org_name = do
  notFoundMaybe =<< selectFirstDb [OrganizationName ==. org_name] []



getOrganizationM' :: forall t site val.
                     (PersistEntity val, YesodPersist site,
                      PersistQuery (YesodPersistBackend site),
                      YesodPersistBackend site ~ PersistEntityBackend val) =>
                     t -> Filter val -> HandlerT site IO (Entity val)
getOrganizationM' _ q = do
  notFoundMaybe =<< selectFirstDb [q] []



insertOrganizationM :: UserId -> OrganizationRequest -> Handler (Entity Organization)
insertOrganizationM user_id organization_request = do

  void $ permissionDeniedEither $ validateOrganizationRequest organization_request

  ts <- timestampH'

  let
    email_md5 = md5Text (organizationRequestEmail organization_request)
    organization = (organizationRequestToOrganization user_id organization_request) {
        organizationEmailMD5 = email_md5
      , organizationCreatedAt = Just ts
    }
  org@(Entity organization_id _) <- insertEntityDb organization

  -- bg job: Insert owners team
  (Entity team_id team) <- insertTeam_BypassM user_id organization_id (TeamRequest "owners" (Just "owners") Membership_InviteOnly Nothing [] Public 0)
  void $ insertTeamMember_BypassM user_id team_id (TeamMemberRequest 0)
  return org



updateOrganizationM :: UserId -> OrganizationId -> OrganizationRequest -> Handler (Entity Organization)
updateOrganizationM user_id organization_id organization_request = do

  void $ permissionDeniedEither $ validateOrganizationRequest organization_request

  ts <- timestampH'

  let
    email_md5 = md5Text (organizationRequestEmail organization_request)
    Organization{..} = (organizationRequestToOrganization user_id organization_request) { organizationModifiedAt = Just ts }

  updateWhereDb
    [ OrganizationUserId ==. user_id, OrganizationId ==. organization_id ]
    [ OrganizationModifiedAt  =. organizationModifiedAt
    , OrganizationActivityAt  =. Just ts
    , OrganizationName        =. organizationName
    , OrganizationDisplayName =. organizationDisplayName
    , OrganizationDescription =. organizationDescription
    , OrganizationCompany     =. organizationCompany
    , OrganizationLocation    =. organizationLocation
    , OrganizationEmail       =. organizationEmail
    , OrganizationEmailMD5    =. email_md5
    , OrganizationMembership  =. organizationMembership
    , OrganizationIcon        =. organizationIcon
    , OrganizationTags        =. organizationTags
    , OrganizationVisibility  =. organizationVisibility
    , OrganizationGuard      +=. 1
    ]
  notFoundMaybe =<< selectFirstDb [ OrganizationUserId ==. user_id, OrganizationId ==. organization_id ] []



deleteOrganizationM :: UserId -> OrganizationId -> Handler ()
deleteOrganizationM user_id organization_id = do

  deleteCascadeWhereDb [ OrganizationUserId ==. user_id, OrganizationId ==. organization_id ]

{-
  -- bg job: Delete owners team
  deleteOrganizationTeamsM user_id organization_id

  -- bg job: Delete Org
  void $ runDB $ deleteWhere [ OrganizationUserId ==. user_id, OrganizationId ==. organization_id ]
  -}




deleteOrganizationTeamsM :: UserId -> OrganizationId -> Handler ()
deleteOrganizationTeamsM _ organization_id = do
  -- TODO: FIXME: security
  deleteWhereDb [ TeamOrgId ==. organization_id ]



countOrganizationsM :: UserId -> Handler CountResponses
countOrganizationsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spUserId of

    Just _  -> notFound
    Nothing -> do
      n <- countDb [ OrganizationActive ==. True ]
      return $ CountResponses [CountResponse 0 (fromIntegral n)]



getOrganizationStatsM :: UserId -> Handler OrganizationStatResponses
getOrganizationStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spBoardId of

    Just _  -> notFound
    Nothing -> notFound




getOrganizationStatM :: UserId -> OrganizationId -> Handler OrganizationStatResponse
getOrganizationStatM _ organization_id = do

  return $ OrganizationStatResponse {
    organizationStatResponseOrganizationId = keyToInt64 organization_id,
    organizationStatResponseTeams          = 0,
    organizationStatResponseMembers        = 0,
    organizationStatResponseForums         = 0,
    organizationStatResponseBoards         = 0,
    organizationStatResponseThreads        = 0,
    organizationStatResponseThreadPosts    = 0,
    organizationStatResponseViews          = 0
  }
