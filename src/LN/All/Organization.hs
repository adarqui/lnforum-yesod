{-# LANGUAGE RecordWildCards #-}

module LN.All.Organization (
  -- Handler
  getOrganizationsR,
  postOrganizationR0,
  getOrganizationR,
  getOrganizationH,
  putOrganizationR,
  deleteOrganizationR,
  getOrganizationsCountR,
  getOrganizationStatsR,
  getOrganizationStatR,

  -- Model/Function
  organizationRequestToOrganization,
  organizationToResponse,
  organizationsToResponses,
  validateOrganizationRequest,

  -- Model/Internal
  getOrganizationsM,
  getOrganizations_ByUserIdM,
  getOrganizations_ByEverythingM,
  getOrganizationM,
  getOrganizationMH,
  getOrganization_ByOrganizationNameM,
  getWithOrganizationM,
  insertOrganizationM,
  updateOrganizationM,
  deleteOrganizationM,
  deleteOrganizationTeamsM,
  countOrganizationsM,
  getOrganizationStatsM,
  getOrganizationStatM
) where



import           LN.All.Prelude
import           LN.All.Team




getOrganizationsR :: Handler Value
getOrganizationsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON organizationsToResponses $ getOrganizationsM (Just sp) user_id



postOrganizationR0 :: Handler Value
postOrganizationR0 = run $ do
  user_id              <- _requireAuthId
  organization_request <- requireJsonBody :: HandlerEff OrganizationRequest
  errorOrJSON organizationToResponse $ insertOrganizationM user_id organization_request



getOrganizationR :: OrganizationId -> Handler Value
getOrganizationR org_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON organizationToResponse $ getOrganizationM user_id org_id



getOrganizationH :: Text -> Handler Value
getOrganizationH org_name = run $ do
  user_id <- _requireAuthId
  errorOrJSON organizationToResponse $ getOrganizationMH user_id org_name



putOrganizationR :: OrganizationId -> Handler Value
putOrganizationR org_id = run $ do
  user_id              <- _requireAuthId
  organization_request <- requireJsonBody
  errorOrJSON organizationToResponse $ updateOrganizationM user_id org_id organization_request



deleteOrganizationR :: OrganizationId -> Handler Value
deleteOrganizationR org_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteOrganizationM user_id org_id



getOrganizationsCountR :: Handler Value
getOrganizationsCountR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ countOrganizationsM (Just sp) user_id



getOrganizationStatsR :: Handler Value
getOrganizationStatsR = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getOrganizationStatsM user_id



getOrganizationStatR :: OrganizationId -> Handler Value
getOrganizationStatR org_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getOrganizationStatM user_id org_id








--
-- Model/Function
--

organizationRequestToOrganization :: UserId -> OrganizationRequest -> Organization
organizationRequestToOrganization user_id OrganizationRequest{..} = Organization {
  organizationUserId      = user_id,
  organizationName        = toSafeName organizationRequestDisplayName,
  organizationDisplayName = organizationRequestDisplayName,
  organizationDescription = organizationRequestDescription,
  organizationCompany     = organizationRequestCompany,
  organizationLocation    = organizationRequestLocation,
  organizationEmail       = organizationRequestEmail,
  organizationEmailMD5    = "md5",
  organizationMembership  = organizationRequestMembership,
  organizationIcon        = organizationRequestIcon,
  organizationTags        = organizationRequestTags,
  organizationVisibility  = organizationRequestVisibility,
  organizationActive      = True,
  organizationGuard       = organizationRequestGuard,
  organizationCreatedAt   = Nothing,
  organizationModifiedBy  = Nothing,
  organizationModifiedAt  = Nothing,
  organizationActivityAt  = Nothing
}


organizationToResponse :: Entity Organization -> OrganizationResponse
organizationToResponse (Entity org_id Organization{..}) = OrganizationResponse {
  organizationResponseId          = keyToInt64 org_id,
  organizationResponseUserId      = keyToInt64 organizationUserId,
  organizationResponseName        = organizationName,
  organizationResponseDisplayName = organizationDisplayName,
  organizationResponseDescription = organizationDescription,
  organizationResponseCompany     = organizationCompany,
  organizationResponseLocation    = organizationLocation,
  organizationResponseEmail       = organizationEmail,
  organizationResponseEmailMD5    = organizationEmailMD5,
  organizationResponseMembership  = organizationMembership,
  organizationResponseIcon        = organizationIcon,
  organizationResponseTags        = organizationTags,
  organizationResponseVisibility  = organizationVisibility,
  organizationResponseActive      = organizationActive,
  organizationResponseGuard       = organizationGuard,
  organizationResponseCreatedAt   = organizationCreatedAt,
  organizationResponseModifiedBy  = fmap keyToInt64 organizationModifiedBy,
  organizationResponseModifiedAt  = organizationModifiedAt,
  organizationResponseActivityAt  = organizationActivityAt
}



organizationsToResponses :: [Entity Organization] -> OrganizationResponses
organizationsToResponses orgs = OrganizationResponses {
  organizationResponses = map organizationToResponse orgs
}









--
-- Model/Internal
--

getOrganizationsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Organization]
getOrganizationsM m_sp user_id = do
  case (lookupSpMay m_sp spUserId) of
    Just lookup_user_id -> getOrganizations_ByUserIdM m_sp user_id lookup_user_id
    Nothing             -> getOrganizations_ByEverythingM m_sp user_id



getOrganizations_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity Organization]
getOrganizations_ByUserIdM m_sp _ lookup_user_id = do
  selectListDbE m_sp [OrganizationUserId ==. lookup_user_id, OrganizationActive ==. True] [] OrganizationId



getOrganizations_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Organization]
getOrganizations_ByEverythingM m_sp _ = do
  selectListDbE m_sp [OrganizationActive ==. True] [] OrganizationId



getOrganizationM :: UserId -> OrganizationId -> HandlerErrorEff (Entity Organization)
getOrganizationM _ org_id = do
  selectFirstDbE [OrganizationId ==. org_id, OrganizationActive ==. True] []



getWithOrganizationM :: Bool -> UserId -> OrganizationId -> HandlerErrorEff (Maybe (Entity Organization))
getWithOrganizationM False _ _           = right Nothing
getWithOrganizationM True user_id org_id = fmap Just <$> getOrganizationM user_id org_id



getOrganizationMH :: UserId -> Text -> HandlerErrorEff (Entity Organization)
getOrganizationMH user_id org_name = getOrganization_ByOrganizationNameM user_id org_name



getOrganization_ByOrganizationNameM :: UserId -> Text -> HandlerErrorEff (Entity Organization)
getOrganization_ByOrganizationNameM _ org_name = do
  selectFirstDbE [OrganizationName ==. org_name, OrganizationActive ==. True] []



insertOrganizationM :: UserId -> OrganizationRequest -> HandlerErrorEff (Entity Organization)
insertOrganizationM user_id organization_request = do

  runEitherT $ do
    sanitized_organization_request <- isT $ isValidAppM $ validateOrganizationRequest organization_request

    ts <- lift timestampH'

    let
      email_md5 = md5Text (organizationRequestEmail sanitized_organization_request)
      organization = (organizationRequestToOrganization user_id sanitized_organization_request) {
          organizationEmailMD5 = email_md5
        , organizationCreatedAt = Just ts
      }
    org@(Entity org_id _) <- isT $ insertEntityByDbE organization

    void $ lift $ insert_SystemTeamsM user_id org_id
    pure org



updateOrganizationM :: UserId -> OrganizationId -> OrganizationRequest -> HandlerErrorEff (Entity Organization)
updateOrganizationM user_id org_id organization_request = do

  runEitherT $ do

    sanitized_organization_request <- isT $ isValidAppM $ validateOrganizationRequest organization_request

    ts <- lift timestampH'

    let
      email_md5 = md5Text (organizationRequestEmail sanitized_organization_request)
      Organization{..} = (organizationRequestToOrganization user_id sanitized_organization_request) { organizationModifiedAt = Just ts }

    isT $ updateWhereDbE
      [ OrganizationUserId ==. user_id, OrganizationId ==. org_id ]
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

    isT $ selectFirstDbE [OrganizationUserId ==. user_id, OrganizationId ==. org_id, OrganizationActive ==. True] []



deleteOrganizationM :: UserId -> OrganizationId -> HandlerErrorEff ()
deleteOrganizationM user_id org_id = do
  deleteCascadeWhereDbE [OrganizationUserId ==. user_id, OrganizationId ==. org_id, OrganizationActive ==. True]

{-
  -- bg job: Delete owners team
  deleteOrganizationTeamsM user_id org_id

  -- bg job: Delete Org
  void $ _runDB $ deleteWhere [ OrganizationUserId ==. user_id, OrganizationId ==. org_id ]
  -}




deleteOrganizationTeamsM :: UserId -> OrganizationId -> HandlerErrorEff ()
deleteOrganizationTeamsM _ org_id = do
  -- TODO: FIXME: security
  deleteWhereDbE [TeamOrgId ==. org_id, TeamActive ==. True]



countOrganizationsM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countOrganizationsM m_sp _ = do
  case (lookupSpMay m_sp spUserId) of
    Just _  -> left Error_NotImplemented
    Nothing -> do
      n <- countDb [OrganizationActive ==. True]
      right $ CountResponses [CountResponse 0 (fromIntegral n)]



getOrganizationStatsM :: UserId -> HandlerErrorEff OrganizationStatResponses
getOrganizationStatsM _ = left Error_NotImplemented



getOrganizationStatM :: UserId -> OrganizationId -> HandlerErrorEff OrganizationStatResponse
getOrganizationStatM _ org_id = do

  num_org_forums  <- countDb [ForumOrgId ==. org_id, ForumActive ==. True]
  num_org_boards  <- countDb [BoardOrgId ==. org_id, BoardActive ==. True]
  num_org_threads <- countDb [ThreadOrgId ==. org_id, ThreadActive ==. True]
  num_org_posts   <- countDb [ThreadPostOrgId ==. org_id, ThreadPostActive ==. True]

  right $ OrganizationStatResponse {
    organizationStatResponseOrganizationId = keyToInt64 org_id,
    organizationStatResponseTeams          = 0,
    organizationStatResponseMembers        = 0,
    organizationStatResponseForums         = fromIntegral num_org_forums,
    organizationStatResponseBoards         = fromIntegral num_org_boards,
    organizationStatResponseThreads        = fromIntegral num_org_threads,
    organizationStatResponseThreadPosts    = fromIntegral num_org_posts,
    organizationStatResponseViews          = 0
  }
