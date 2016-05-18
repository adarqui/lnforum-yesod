module Model.Pack.Organization (
  getOrganizationPacksM,
  getOrganizationPackM,
  getOrganizationPackMH,
) where



import           Model.Prelude
import           Model.User.Function
import           Model.User.Internal2
import           Model.Organization.Function
import           Model.Organization.Internal



getOrganizationPacksM :: UserId -> Handler OrganizationPackResponses
getOrganizationPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  getOrganizationPacksBy_EverythingM user_id sp



getOrganizationPackM :: UserId -> OrganizationId -> Handler OrganizationPackResponse
getOrganizationPackM user_id organization_id = do

  organization         <- getOrganizationM user_id organization_id
  getOrganizationPack_ByOrganizationM user_id organization



getOrganizationPackMH :: UserId -> Text -> Handler OrganizationPackResponse
getOrganizationPackMH = getOrganizationPackBy_OrganizationName



getOrganizationPacksBy_EverythingM :: UserId -> StandardParams -> Handler OrganizationPackResponses
getOrganizationPacksBy_EverythingM user_id sp = do

  organizations       <- getOrganizationsBy_EverythingM user_id sp
  organizations_packs <- mapM (\organization -> getOrganizationPack_ByOrganizationM user_id organization) organizations
  return $ OrganizationPackResponses {
    organizationPackResponses = organizations_packs
  }



getOrganizationPackBy_OrganizationName :: UserId -> Text -> Handler OrganizationPackResponse
getOrganizationPackBy_OrganizationName user_id organization_name = do

  organization         <- getOrganizationBy_OrganizationNameM user_id organization_name
  getOrganizationPack_ByOrganizationM user_id organization



getOrganizationPack_ByOrganizationM :: UserId -> Entity Organization -> Handler OrganizationPackResponse
getOrganizationPack_ByOrganizationM user_id organization@(Entity _ Organization{..}) = do

  organization_user    <- getUserM user_id organizationUserId
  organization_stats   <- getOrganizationStatM user_id (entityKey organization)

  return $ OrganizationPackResponse {
    organizationPackResponseOrganization   = organizationToResponse organization,
    organizationPackResponseOrganizationId = organization_id,
    organizationPackResponseUser           = userToSanitizedResponse organization_user,
    organizationPackResponseUserId         = entityKeyToInt64 organization_user,
    organizationPackResponseStat           = organization_stats,
    organizationPackResponseLike           = Nothing,
    organizationPackResponseStar           = Nothing
  }
  where
  organization_id = entityKeyToInt64 organization
