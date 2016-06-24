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
  (toJSON . organizationsToResponses) <$> getOrganizationsM user_id



postOrganizationR0 :: Handler Value
postOrganizationR0 = run $ do

  user_id <- _requireAuthId

  organization_request <- requireJsonBody :: HandlerEff OrganizationRequest
  (toJSON . organizationToResponse) <$> insertOrganizationM user_id organization_request



getOrganizationR :: OrganizationId -> Handler Value
getOrganizationR org_id = getOrganizationR' getOrganizationM org_id
--  user_id <- _requireAuthId
--  (toJSON . organizationToResponse) <$> getOrganizationM user_id org_id



getOrganizationH :: Text -> Handler Value
getOrganizationH org_name = getOrganizationR' getOrganizationMH org_name
--  user_id <- _requireAuthId
--  (toJSON . organizationToResponse) <$> getOrganizationMH user_id org_name



getOrganizationR' f a = run $ do
  user_id <- _requireAuthId
  (toJSON . organizationToResponse <$> f user_id a)



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

getOrganizationsM :: UserId -> HandlerEff [Entity Organization]
getOrganizationsM user_id = do
  sp@StandardParams{..} <- lookupStandardParams
  case spUserId of
    Just lookup_user_id -> getOrganizations_ByUserIdM user_id lookup_user_id sp
    Nothing             -> getOrganizations_ByEverythingM user_id sp



getOrganizations_ByUserIdM :: UserId -> UserId -> StandardParams -> HandlerEff [Entity Organization]
getOrganizations_ByUserIdM _ lookup_user_id sp = do
  selectListDb sp [OrganizationUserId ==. lookup_user_id] [] OrganizationId



getOrganizations_ByEverythingM :: UserId -> StandardParams -> HandlerEff [Entity Organization]
getOrganizations_ByEverythingM _ sp = do
  selectListDb sp [] [] OrganizationId



getOrganizationM :: UserId -> OrganizationId -> HandlerEff (Entity Organization)
getOrganizationM user_id org_id = getOrganizationM' user_id (OrganizationId ==. org_id)



getOrganizationMH :: UserId -> Text -> HandlerEff (Entity Organization)
getOrganizationMH user_id org_name = getOrganizationM' user_id (OrganizationName ==. org_name)



getOrganization_ByOrganizationNameM :: UserId -> Text -> HandlerEff (Entity Organization)
getOrganization_ByOrganizationNameM _ org_name = do
  notFoundMaybe =<< selectFirstDb [OrganizationName ==. org_name] []



getOrganizationM' :: forall t site val.
                     (PersistEntity val, YesodPersist site,
                      PersistQuery (YesodPersistBackend site),
                      YesodPersistBackend site ~ PersistEntityBackend val) =>
                     t -> Filter val -> ControlMA (HandlerT site IO) (Entity val)
getOrganizationM' _ q = do
  notFoundMaybe =<< selectFirstDb [q] []



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

  return $ OrganizationStatResponse {
    organizationStatResponseOrganizationId = keyToInt64 org_id,
    organizationStatResponseTeams          = 0,
    organizationStatResponseMembers        = 0,
    organizationStatResponseForums         = 0,
    organizationStatResponseBoards         = 0,
    organizationStatResponseThreads        = 0,
    organizationStatResponseThreadPosts    = 0,
    organizationStatResponseViews          = 0
  }
