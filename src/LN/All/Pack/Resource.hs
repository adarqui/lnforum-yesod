{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.Resource (
  -- Handler
  getResourcePacksR,
  getResourcePackR,

  -- Model
  getResourcePacksM,
  getResourcePackM
) where



import           LN.All.Prelude
import           LN.All.Resource
import           LN.All.User



--
-- Handler
--

getResourcePacksR :: Handler Value
getResourcePacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getResourcePacksM (pure sp) user_id



getResourcePackR :: ResourceId -> Handler Value
getResourcePackR thread_post_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getResourcePackM user_id thread_post_id






--
-- Model
--

getResourcePacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff ResourcePackResponses
getResourcePacksM m_sp user_id = do

  e_resources <- getResourcesM m_sp user_id
  rehtie e_resources left $ \resources -> do

    resource_packs <- rights <$> mapM (\resource -> getResourcePack_ByResourceM user_id resource) resources

    right $ ResourcePackResponses {
      resourcePackResponses = resource_packs
    }



getResourcePackM :: UserId -> ResourceId -> HandlerErrorEff ResourcePackResponse
getResourcePackM user_id resource_id = do

  e_resource <- getResourceM user_id resource_id
  rehtie e_resource left $ getResourcePack_ByResourceM user_id



getResourcePack_ByResourceM :: UserId -> Entity Resource -> HandlerErrorEff ResourcePackResponse
getResourcePack_ByResourceM user_id resource@(Entity resource_id Resource{..}) = do

  lr <- runEitherT $ do

    resource_user <- isT $ getUserM user_id resourceUserId
    resource_stat <- isT $ getResourceStatM user_id resource_id
--  resource_like <- isT $ getResourceLike_ByResourceM user_id resource
--  resource_star <- isT $ getResourceStar_ByResourceM user_id resource
    pure (resource_user, resource_stat)

  rehtie lr left $ \(resource_user, resource_stat) -> do
    right $ ResourcePackResponse {
      resourcePackResponseResource    = resourceToResponse resource,
      resourcePackResponseResourceId  = keyToInt64 resource_id,
      resourcePackResponseUser        = userToSanitizedResponse resource_user,
      resourcePackResponseUserId      = entityKeyToInt64 resource_user,
      resourcePackResponseStat        = resource_stat,
      resourcePackResponseLike        = Nothing,
      resourcePackResponseStar        = Nothing,
      resourcePackResponsePermissions = emptyPermissions
  --    resourcePackResponseLike     = fmap resourceLikeToResponse resource_like,
  --    resourcePackResponseStar     = fmap resourceStarToResponse resource_star
    }
