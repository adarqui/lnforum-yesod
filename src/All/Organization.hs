{-# LANGUAGE RecordWildCards #-}

module All.Organization (
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



import           All.Prelude
import           All.TeamMember
import           All.Team
import           LN.T.Membership
import           LN.T.Visibility




getOrganizationsR :: Handler Value
getOrganizationsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  (toJSON . organizationsToResponses) <$> getOrganizationsM (Just sp) user_id



postOrganizationR0 :: Handler Value
postOrganizationR0 = run $ do
  user_id <- _requireAuthId
  organization_request <- requireJsonBody :: HandlerEff OrganizationRequest
  (toJSON . organizationToResponse) <$> insertOrganizationM user_id organization_request



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
  user_id <- _requireAuthId
  organization_request <- requireJsonBody
  (toJSON . organizationToResponse) <$> updateOrganizationM user_id org_id organization_request



deleteOrganizationR :: OrganizationId -> Handler Value
deleteOrganizationR org_id = run $ do
  user_id <- _requireAuthId
  void $ deleteOrganizationM user_id org_id
  pure $ toJSON ()



getOrganizationsCountR :: Handler Value
getOrganizationsCountR = run $ do
  user_id <- _requireAuthId
  toJSON <$> countOrganizationsM user_id



getOrganizationStatsR :: Handler Value
getOrganizationStatsR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getOrganizationStatsM user_id



getOrganizationStatR :: OrganizationId -> Handler Value
getOrganizationStatR org_id = run $ do
  user_id <- _requireAuthId
  toJSON <$> getOrganizationStatM user_id org_id








--
-- Model/Function
--

organizationRequestToOrganization :: UserId -> OrganizationRequest -> Organization
organizationRequestToOrganization user_id OrganizationRequest{..} = Organization {
  organizationUserId      = user_id,
  organizationName        = toPrettyName organizationRequestDisplayName,
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



validateOrganizationRequest :: OrganizationRequest -> Either Text OrganizationRequest
validateOrganizationRequest z@OrganizationRequest{..} = do
--  _ <- isValidNick organizationRequestName
  _ <- isValidName organizationRequestDisplayName
  _ <- isValidEmail organizationRequestEmail
  _ <- isValidNonEmptyString organizationRequestCompany
  _ <- isValidNonEmptyString organizationRequestLocation
  Right z






--
-- Model/Internal
--

getOrganizationsM :: Maybe Sp -> UserId -> HandlerEff [Entity Organization]
getOrganizationsM m_sp user_id = do
  case (lookupSpMay m_sp spUserId) of
    Just lookup_user_id -> getOrganizations_ByUserIdM m_sp user_id lookup_user_id
    Nothing             -> getOrganizations_ByEverythingM m_sp user_id



getOrganizations_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerEff [Entity Organization]
getOrganizations_ByUserIdM m_sp _ lookup_user_id = do
  selectListDbMay m_sp [OrganizationUserId ==. lookup_user_id] [] OrganizationId



getOrganizations_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerEff [Entity Organization]
getOrganizations_ByEverythingM m_sp _ = do
  selectListDbMay m_sp [] [] OrganizationId



getOrganizationM :: UserId -> OrganizationId -> HandlerEff (Maybe (Entity Organization))
getOrganizationM user_id org_id = do
  selectFirstDb [OrganizationId ==. org_id] []



getWithOrganizationM :: Bool -> UserId -> OrganizationId -> HandlerEff (Maybe (Entity Organization))
getWithOrganizationM False _ _           = pure Nothing
getWithOrganizationM True user_id org_id = getOrganizationM user_id org_id



getOrganizationMH :: UserId -> Text -> HandlerEff (Maybe (Entity Organization))
getOrganizationMH user_id org_name = getOrganization_ByOrganizationNameM user_id org_name



getOrganization_ByOrganizationNameM :: UserId -> Text -> HandlerEff (Maybe (Entity Organization))
getOrganization_ByOrganizationNameM _ org_name = do
  selectFirstDb [OrganizationName ==. org_name] []



insertOrganizationM :: UserId -> OrganizationRequest -> HandlerEff (Entity Organization)
insertOrganizationM user_id organization_request = do

  void $ permissionDeniedEither $ validateOrganizationRequest organization_request

  ts <- timestampH'

  let
    email_md5 = md5Text (organizationRequestEmail organization_request)
    organization = (organizationRequestToOrganization user_id organization_request) {
        organizationEmailMD5 = email_md5
      , organizationCreatedAt = Just ts
    }
  org@(Entity org_id _) <- insertEntityDb organization

  void $ insert_SystemTeamsM user_id org_id

  return org



updateOrganizationM :: UserId -> OrganizationId -> OrganizationRequest -> HandlerEff (Entity Organization)
updateOrganizationM user_id org_id organization_request = do

  void $ permissionDeniedEither $ validateOrganizationRequest organization_request

  ts <- timestampH'

  let
    email_md5 = md5Text (organizationRequestEmail organization_request)
    Organization{..} = (organizationRequestToOrganization user_id organization_request) { organizationModifiedAt = Just ts }

  updateWhereDb
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
  notFoundMaybe =<< selectFirstDb [ OrganizationUserId ==. user_id, OrganizationId ==. org_id ] []



deleteOrganizationM :: UserId -> OrganizationId -> HandlerEff ()
deleteOrganizationM user_id org_id = do

  deleteCascadeWhereDb [ OrganizationUserId ==. user_id, OrganizationId ==. org_id ]

{-
  -- bg job: Delete owners team
  deleteOrganizationTeamsM user_id org_id

  -- bg job: Delete Org
  void $ _runDB $ deleteWhere [ OrganizationUserId ==. user_id, OrganizationId ==. org_id ]
  -}




deleteOrganizationTeamsM :: UserId -> OrganizationId -> HandlerEff ()
deleteOrganizationTeamsM _ org_id = do
  -- TODO: FIXME: security
  deleteWhereDb [ TeamOrgId ==. org_id ]



countOrganizationsM :: UserId -> HandlerEff CountResponses
countOrganizationsM _ = do
  StandardParams{..} <- lookupStandardParams
  case spUserId of
    Just _  -> notFound
    Nothing -> do
      n <- countDb [ OrganizationActive ==. True ]
      return $ CountResponses [CountResponse 0 (fromIntegral n)]



getOrganizationStatsM :: UserId -> HandlerEff OrganizationStatResponses
getOrganizationStatsM _ = do
  StandardParams{..} <- lookupStandardParams
  case spBoardId of
    Just _  -> notFound
    Nothing -> notFound




getOrganizationStatM :: UserId -> OrganizationId -> HandlerEff OrganizationStatResponse
getOrganizationStatM _ org_id = do

  num_org_forums  <- countDb [ForumOrgId ==. org_id, ForumActive ==. True]
  num_org_boards  <- countDb [BoardOrgId ==. org_id, BoardActive ==. True]
  num_org_threads <- countDb [ThreadOrgId ==. org_id, ThreadActive ==. True]
  num_org_posts   <- countDb [ThreadPostOrgId ==. org_id, ThreadPostActive ==. True]

  return $ OrganizationStatResponse {
    organizationStatResponseOrganizationId = keyToInt64 org_id,
    organizationStatResponseTeams          = 0,
    organizationStatResponseMembers        = 0,
    organizationStatResponseForums         = fromIntegral num_org_forums,
    organizationStatResponseBoards         = fromIntegral num_org_boards,
    organizationStatResponseThreads        = fromIntegral num_org_threads,
    organizationStatResponseThreadPosts    = fromIntegral num_org_posts,
    organizationStatResponseViews          = 0
  }
