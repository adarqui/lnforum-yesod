{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.Organization (
  -- LN.Handler
  getOrganizationPacksR,
  getOrganizationPackR,
  getOrganizationPackH,

  -- LN.Model
  getOrganizationPacksM,
  getOrganizationPackM,
  getOrganizationPackMH,
) where



import           LN.Access
import           LN.All.Internal
import           LN.All.Organization
import           LN.All.Prelude
import           LN.All.User



--
-- LN.Handler
--

getOrganizationPacksR :: Handler Value
getOrganizationPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getOrganizationPacksM (Just sp) user_id



getOrganizationPackR :: OrganizationId -> Handler Value
getOrganizationPackR org_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getOrganizationPackM user_id org_id



getOrganizationPackH :: Text -> Handler Value
getOrganizationPackH org_name = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getOrganizationPackMH user_id org_name






--
-- LN.Model
--

getOrganizationPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff OrganizationPackResponses
getOrganizationPacksM m_sp user_id = do
  getOrganizationPacks_ByEverythingM m_sp user_id



getOrganizationPackM :: UserId -> OrganizationId -> HandlerErrorEff OrganizationPackResponse
getOrganizationPackM user_id org_id = do

  e_organization <- getOrganizationM user_id org_id
  rehtie e_organization leftA $ \organization -> do
    getOrganizationPack_ByOrganizationM user_id organization



getOrganizationPackMH :: UserId -> Text -> HandlerErrorEff OrganizationPackResponse
getOrganizationPackMH = getOrganizationPack_ByOrganizationName



getOrganizationPacks_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff OrganizationPackResponses
getOrganizationPacks_ByEverythingM m_sp user_id = do

  e_organizations        <- getOrganizations_ByEverythingM m_sp user_id

  rehtie e_organizations leftA $ \organizations -> do

    organization_packs <- fmap rights (mapM (\organization -> getOrganizationPack_ByOrganizationM user_id organization) organizations)
    rightA $ OrganizationPackResponses {
      organizationPackResponses = organization_packs
    }



getOrganizationPack_ByOrganizationName :: UserId -> Text -> HandlerEff (ErrorEff OrganizationPackResponse)
getOrganizationPack_ByOrganizationName user_id organization_name = do

  e_organization <- getOrganization_ByOrganizationNameM user_id organization_name
  case e_organization of
    Left err           -> leftA err
    Right organization -> getOrganizationPack_ByOrganizationM user_id organization



getOrganizationPack_ByOrganizationM :: UserId -> Entity Organization -> HandlerEff (ErrorEff OrganizationPackResponse)
getOrganizationPack_ByOrganizationM user_id organization@(Entity org_id Organization{..}) = do

  lr <- runEitherT $ do

    organization_user  <- mustT $ getUserM user_id organizationUserId
    organization_stats <- mustT $ getOrganizationStatM user_id (entityKey organization)
    user_perms_by_org  <- lift $ userPermissions_ByOrganizationIdM user_id org_id
    user_teams         <- lift $ userTeamsOf_OrganizationIdM user_id org_id

    pure (organization_user
         ,organization_stats
         ,user_perms_by_org
         ,user_teams)

  rehtie lr leftA $ \(organization_user, organization_stats, user_perms_by_org, user_teams) -> do

      rightA $ OrganizationPackResponse {
        organizationPackResponseOrganization   = organizationToResponse organization,
        organizationPackResponseOrganizationId = keyToInt64 org_id,
        organizationPackResponseUser           = userToSanitizedResponse organization_user,
        organizationPackResponseUserId         = entityKeyToInt64 organization_user,
        organizationPackResponseStat           = organization_stats,
        organizationPackResponseLike           = Nothing,
        organizationPackResponseStar           = Nothing,
        organizationPackResponsePermissions    = user_perms_by_org,
        organizationPackResponseTeams          = map (teamSystem.entityVal) user_teams
      }
