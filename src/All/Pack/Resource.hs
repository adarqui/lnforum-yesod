{-# LANGUAGE RecordWildCards #-}

module All.Pack.Resource (
  -- Handler
  getResourcePacksR,
  getResourcePackR,

  -- Model
  getResourcePacksM,
  getResourcePackM
) where



import           All.Prelude
import           All.Resource
import           All.User



--
-- Handler
--

getResourcePacksR :: HandlerEff Value
getResourcePacksR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getResourcePacksM user_id



getResourcePackR :: ResourceId -> HandlerEff Value
getResourcePackR thread_post_id = do
  user_id <- _requireAuthId
  toJSON <$> getResourcePackM user_id thread_post_id






--
-- Model
--

getResourcePacksM :: UserId -> HandlerEff ResourcePackResponses
getResourcePacksM user_id = do

  sp <- lookupStandardParams

  resources <- getResourcesM user_id

  resource_packs <- mapM (\resource -> getResourcePack_ByResourceM user_id resource sp) resources


  return $ ResourcePackResponses {
    resourcePackResponses = resource_packs
  }



getResourcePackM :: UserId -> ResourceId -> HandlerEff ResourcePackResponse
getResourcePackM user_id resource_id = do

  resource <- getResourceM user_id resource_id

  getResourcePack_ByResourceM user_id resource defaultStandardParams



getResourcePack_ByResourceM :: UserId -> Entity Resource -> StandardParams -> HandlerEff ResourcePackResponse
getResourcePack_ByResourceM user_id resource@(Entity resource_id Resource{..}) _ = do

  resource_user <- getUserM user_id resourceUserId
  resource_stat <- getResourceStatM user_id resource_id
--  resource_like <- getResourceLike_ByResourceM user_id resource
--  resource_star <- getResourceStar_ByResourceM user_id resource

  return $ ResourcePackResponse {
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
