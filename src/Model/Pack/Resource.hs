{-# LANGUAGE RecordWildCards #-}

module Model.Pack.Resource (
  getResourcePacksM,
  getResourcePackM
) where



import           Model.Prelude
import           Model.Resource.Function
import           Model.Resource.Internal
import           Model.User.Function
import           Model.User.Internal2



getResourcePacksM :: UserId -> Handler ResourcePackResponses
getResourcePacksM user_id = do

  sp <- lookupStandardParams

  resources <- getResourcesM user_id

  resource_packs <- mapM (\resource -> getResourcePack_ByResourceM user_id resource sp) resources


  return $ ResourcePackResponses {
    resourcePackResponses = resource_packs
  }



getResourcePackM :: UserId -> ResourceId -> Handler ResourcePackResponse
getResourcePackM user_id resource_id = do

  resource <- getResourceM user_id resource_id

  getResourcePack_ByResourceM user_id resource defaultStandardParams



getResourcePack_ByResourceM :: UserId -> Entity Resource -> StandardParams -> Handler ResourcePackResponse
getResourcePack_ByResourceM user_id resource@(Entity resource_id Resource{..}) _ = do

  resource_user <- getUserM user_id resourceUserId
  resource_stat <- getResourceStatM user_id resource_id
--  resource_like <- getResourceLike_ByResourceM user_id resource
--  resource_star <- getResourceStar_ByResourceM user_id resource

  return $ ResourcePackResponse {
    resourcePackResponseResource   = resourceToResponse resource,
    resourcePackResponseResourceId = keyToInt64 resource_id,
    resourcePackResponseUser       = userToSanitizedResponse resource_user,
    resourcePackResponseUserId     = entityKeyToInt64 resource_user,
    resourcePackResponseStat       = resource_stat,
    resourcePackResponseLike       = Nothing,
    resourcePackResponseStar       = Nothing
--    resourcePackResponseLike     = fmap resourceLikeToResponse resource_like,
--    resourcePackResponseStar     = fmap resourceStarToResponse resource_star
  }
