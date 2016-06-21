{-# LANGUAGE RecordWildCards #-}

module All.Pack.Organization (
  -- Model
  getOrganizationPacksM,
  getOrganizationPackM,
  getOrganizationPackMH,
) where



import           All.Organization
import           All.Prelude
import           Model.User.Function
import           Model.User.Internal2



getOrganizationPacksM :: UserId -> Handler OrganizationPackResponses
getOrganizationPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  getOrganizationPacks_ByEverythingM user_id sp



getOrganizationPackM :: UserId -> OrganizationId -> Handler OrganizationPackResponse
getOrganizationPackM user_id organization_id = do

  organization         <- getOrganizationM user_id organization_id
  getOrganizationPack_ByOrganizationM user_id organization



getOrganizationPackMH :: UserId -> Text -> Handler OrganizationPackResponse
getOrganizationPackMH = getOrganizationPack_ByOrganizationName



getOrganizationPacks_ByEverythingM :: UserId -> StandardParams -> Handler OrganizationPackResponses
getOrganizationPacks_ByEverythingM user_id sp = do

  organizations       <- getOrganizations_ByEverythingM user_id sp
  organizations_packs <- mapM (\organization -> getOrganizationPack_ByOrganizationM user_id organization) organizations
  return $ OrganizationPackResponses {
    organizationPackResponses = organizations_packs
  }



getOrganizationPack_ByOrganizationName :: UserId -> Text -> Handler OrganizationPackResponse
getOrganizationPack_ByOrganizationName user_id organization_name = do

  organization         <- getOrganization_ByOrganizationNameM user_id organization_name
  getOrganizationPack_ByOrganizationM user_id organization



getOrganizationPack_ByOrganizationM :: UserId -> Entity Organization -> Handler OrganizationPackResponse
getOrganizationPack_ByOrganizationM user_id organization@(Entity org_id Organization{..}) = do

  organization_user    <- getUserM user_id organizationUserId
  organization_stats   <- getOrganizationStatM user_id (entityKey organization)
--  is_org_owner         <- isOwnerOf_OrganizationIdM user_id org_id
--  is_org_member        <- isMemberOf_OrganizationIdM user_id org_id

  return $ OrganizationPackResponse {
    organizationPackResponseOrganization   = organizationToResponse organization,
    organizationPackResponseOrganizationId = organization_id,
    organizationPackResponseUser           = userToSanitizedResponse organization_user,
    organizationPackResponseUserId         = entityKeyToInt64 organization_user,
    organizationPackResponseStat           = organization_stats,
    organizationPackResponseLike           = Nothing,
    organizationPackResponseStar           = Nothing,
    organizationPackResponsePermissions    = emptyPermissions
--    organizationPackResponseIsOwner        = is_org_owner,
--    organizationPackResponseIsMember       = is_org_member
  }
  where
  organization_id = entityKeyToInt64 organization
