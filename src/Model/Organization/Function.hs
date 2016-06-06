{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Organization.Function (
  organizationRequestToOrganization,
  organizationToResponse,
  organizationsToResponses,
  validateOrganizationRequest
) where



import           Model.Prelude



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
organizationToResponse (Entity organization_id Organization{..}) = OrganizationResponse {
  organizationResponseId          = keyToInt64 organization_id,
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
