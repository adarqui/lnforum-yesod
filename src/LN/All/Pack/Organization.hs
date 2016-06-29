{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.Organization (
  -- LN.Handler
  getOrganizationPacksR,
  getOrganizationPackR,
  getOrganizationPackH,

  -- Model
  getOrganizationPacksM,
  getOrganizationPackM,
  getOrganizationPackMH,
) where



import           Access
import           LN.All.Organization
import           LN.All.Prelude
import           LN.All.User



--
-- LN.Handler
--

getOrganizationPacksR :: LN.Handler Value
getOrganizationPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getOrganizationPacksM (Just sp) user_id



getOrganizationPackR :: OrganizationId -> LN.Handler Value
getOrganizationPackR org_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getOrganizationPackM user_id org_id



getOrganizationPackH :: Text -> LN.Handler Value
getOrganizationPackH org_name = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getOrganizationPackMH user_id org_name






--
-- Model
--

getOrganizationPacksM :: Maybe StandardParams -> UserId -> LN.HandlerErrorEff OrganizationPackResponses
getOrganizationPacksM m_sp user_id = do
  getOrganizationPacks_ByEverythingM m_sp user_id



getOrganizationPackM :: UserId -> OrganizationId -> LN.HandlerErrorEff OrganizationPackResponse
getOrganizationPackM user_id org_id = do

  e_organization <- getOrganizationM user_id org_id
  rehtie e_organization left $ \organization -> do
    getOrganizationPack_ByOrganizationM user_id organization



getOrganizationPackMH :: UserId -> Text -> LN.HandlerErrorEff OrganizationPackResponse
getOrganizationPackMH = getOrganizationPack_ByOrganizationName



getOrganizationPacks_ByEverythingM :: Maybe StandardParams -> UserId -> LN.HandlerErrorEff OrganizationPackResponses
getOrganizationPacks_ByEverythingM m_sp user_id = do

  e_organizations        <- getOrganizations_ByEverythingM m_sp user_id

  rehtie e_organizations left $ \organizations -> do

    organization_packs <- fmap rights (mapM (\organization -> getOrganizationPack_ByOrganizationM user_id organization) organizations)
    right $ OrganizationPackResponses {
      organizationPackResponses = organization_packs
    }



getOrganizationPack_ByOrganizationName :: UserId -> Text -> LN.HandlerEff (ErrorEff OrganizationPackResponse)
getOrganizationPack_ByOrganizationName user_id organization_name = do

  e_organization <- getOrganization_ByOrganizationNameM user_id organization_name
  case e_organization of
    Left err           -> left err
    Right organization -> getOrganizationPack_ByOrganizationM user_id organization



getOrganizationPack_ByOrganizationM :: UserId -> Entity Organization -> LN.HandlerEff (ErrorEff OrganizationPackResponse)
getOrganizationPack_ByOrganizationM user_id organization@(Entity org_id Organization{..}) = do

  lr <- runEitherT $ do

    organization_user  <- isT $ getUserM user_id organizationUserId
    organization_stats <- isT $ getOrganizationStatM user_id (entityKey organization)
    user_perms_by_org  <- lift $ userPermissions_ByOrganizationIdM user_id org_id
    user_teams         <- lift $ userTeamsOf_OrganizationIdM user_id org_id

    pure (organization_user
         ,organization_stats
         ,user_perms_by_org
         ,user_teams)

  rehtie lr left $ \(organization_user, organization_stats, user_perms_by_org, user_teams) -> do

      right $ OrganizationPackResponse {
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
