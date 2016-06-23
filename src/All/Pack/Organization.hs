{-# LANGUAGE RecordWildCards #-}

module All.Pack.Organization (
  -- Handler
  getOrganizationPacksR,
  getOrganizationPackR,
  getOrganizationPackH,

  -- Model
  getOrganizationPacksM,
  getOrganizationPackM,
  getOrganizationPackMH,
) where



import           Access
import           All.Organization
import           All.Prelude
import           All.User
import           Data.List        (nub)



--
-- Handler
--

getOrganizationPacksR :: Handler Value
getOrganizationPacksR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getOrganizationPacksM user_id



getOrganizationPackR :: OrganizationId -> Handler Value
getOrganizationPackR organization_id = run $ do
  user_id <- _requireAuthId
  toJSON <$> getOrganizationPackM user_id organization_id



getOrganizationPackH :: Text -> Handler Value
getOrganizationPackH org_name = run $ do
  user_id <- _requireAuthId
  toJSON <$> getOrganizationPackMH user_id org_name






--
-- Model
--

getOrganizationPacksM :: UserId -> HandlerEff OrganizationPackResponses
getOrganizationPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  getOrganizationPacks_ByEverythingM user_id sp



getOrganizationPackM :: UserId -> OrganizationId -> HandlerEff OrganizationPackResponse
getOrganizationPackM user_id organization_id = do

  organization         <- getOrganizationM user_id organization_id
  getOrganizationPack_ByOrganizationM user_id organization



getOrganizationPackMH :: UserId -> Text -> HandlerEff OrganizationPackResponse
getOrganizationPackMH = getOrganizationPack_ByOrganizationName



getOrganizationPacks_ByEverythingM :: UserId -> StandardParams -> HandlerEff OrganizationPackResponses
getOrganizationPacks_ByEverythingM user_id sp = do

  organizations       <- getOrganizations_ByEverythingM user_id sp
  organizations_packs <- mapM (\organization -> getOrganizationPack_ByOrganizationM user_id organization) organizations
  return $ OrganizationPackResponses {
    organizationPackResponses = organizations_packs
  }



getOrganizationPack_ByOrganizationName :: UserId -> Text -> HandlerEff OrganizationPackResponse
getOrganizationPack_ByOrganizationName user_id organization_name = do

  organization         <- getOrganization_ByOrganizationNameM user_id organization_name
  getOrganizationPack_ByOrganizationM user_id organization



getOrganizationPack_ByOrganizationM :: UserId -> Entity Organization -> HandlerEff OrganizationPackResponse
getOrganizationPack_ByOrganizationM user_id organization@(Entity org_id Organization{..}) = do

  organization_user    <- getUserM user_id organizationUserId
  organization_stats   <- getOrganizationStatM user_id (entityKey organization)
  user_perms_by_org    <- userPermissions_ByOrganizationIdM user_id org_id
  user_teams           <- userTeamsOf_OrganizationIdM user_id org_id

  return $ OrganizationPackResponse {
    organizationPackResponseOrganization   = organizationToResponse organization,
    organizationPackResponseOrganizationId = organization_id,
    organizationPackResponseUser           = userToSanitizedResponse organization_user,
    organizationPackResponseUserId         = entityKeyToInt64 organization_user,
    organizationPackResponseStat           = organization_stats,
    organizationPackResponseLike           = Nothing,
    organizationPackResponseStar           = Nothing,
    organizationPackResponsePermissions    = user_perms_by_org,
    organizationPackResponseTeams          = map (teamSystem.entityVal) user_teams
  }
  where
  organization_id = entityKeyToInt64 organization
