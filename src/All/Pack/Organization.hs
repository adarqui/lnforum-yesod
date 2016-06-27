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
  sp      <- lookupStandardParams
  toJSON <$> getOrganizationPacksM (Just sp) user_id



getOrganizationPackR :: OrganizationId -> Handler Value
getOrganizationPackR org_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getOrganizationPackM user_id org_id



getOrganizationPackH :: Text -> Handler Value
getOrganizationPackH org_name = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getOrganizationPackMH user_id org_name






--
-- Model
--

getOrganizationPacksM :: Maybe StandardParams -> UserId -> HandlerEff OrganizationPackResponses
getOrganizationPacksM m_sp user_id = do
  getOrganizationPacks_ByEverythingM m_sp user_id



getOrganizationPackM :: UserId -> OrganizationId -> HandlerEff (Maybe OrganizationPackResponse)
getOrganizationPackM user_id org_id = do

  m_organization <- getOrganizationM user_id org_id
  case m_organization of
    Nothing           -> pure Nothing
    Just organization -> getOrganizationPack_ByOrganizationM user_id organization



getOrganizationPackMH :: UserId -> Text -> HandlerEff (Maybe OrganizationPackResponse)
getOrganizationPackMH = getOrganizationPack_ByOrganizationName



getOrganizationPacks_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerEff OrganizationPackResponses
getOrganizationPacks_ByEverythingM m_sp user_id = do

  organizations       <- getOrganizations_ByEverythingM m_sp user_id
  organizations_packs <- catMaybes <$> mapM (\organization -> getOrganizationPack_ByOrganizationM user_id organization) organizations
  return $ OrganizationPackResponses {
    organizationPackResponses = organizations_packs
  }



getOrganizationPack_ByOrganizationName :: UserId -> Text -> HandlerEff (Maybe OrganizationPackResponse)
getOrganizationPack_ByOrganizationName user_id organization_name = do

  m_organization <- getOrganization_ByOrganizationNameM user_id organization_name
  case m_organization of
    Nothing           -> pure Nothing
    Just organization -> getOrganizationPack_ByOrganizationM user_id organization



getOrganizationPack_ByOrganizationM :: UserId -> Entity Organization -> HandlerEff (Maybe OrganizationPackResponse)
getOrganizationPack_ByOrganizationM user_id organization@(Entity org_id Organization{..}) = do

  organization_user    <- getUserM user_id organizationUserId
  m_organization_stats <- getOrganizationStatM user_id (entityKey organization)
  user_perms_by_org    <- userPermissions_ByOrganizationIdM user_id org_id
  user_teams           <- userTeamsOf_OrganizationIdM user_id org_id

  case m_organization_stats of
    Just organization_stats -> do

      pure $ Just $ OrganizationPackResponse {
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

    _ -> pure Nothing
