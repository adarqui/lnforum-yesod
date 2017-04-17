{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.Resource (
  -- LN.Handler
  getResourcePacksR,
  getResourcePackR,

  -- LN.Model
  getResourcePacksM,
  getResourcePackM
) where



import           LN.All.Internal
import           LN.All.Prelude
import           LN.All.Resource
import           LN.All.BucketResource
import           LN.All.User



--
-- LN.Handler
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
-- LN.Model
--

getResourcePacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff ResourcePackResponses
getResourcePacksM m_sp user_id = do

  liftIO $ print m_sp

  e_resources <-
    case lookupSpMay m_sp spBucketId of
      Just bucket_id -> getBucketResourcesM m_sp user_id bucket_id
      _              -> getResourcesM m_sp user_id

  rehtie e_resources leftA $ \resources -> do

    resource_packs <- rights <$> mapM (\resource -> getResourcePack_ByResourceM user_id resource) resources

    rightA $ ResourcePackResponses {
      resourcePackResponses = resource_packs
    }



getResourcePackM :: UserId -> ResourceId -> HandlerErrorEff ResourcePackResponse
getResourcePackM user_id resource_id = do

  e_resource <- getResourceM user_id resource_id
  rehtie e_resource leftA $ getResourcePack_ByResourceM user_id



getResourcePack_ByResourceM :: UserId -> Entity Resource -> HandlerErrorEff ResourcePackResponse
getResourcePack_ByResourceM user_id resource@(Entity resource_id Resource{..}) = do

  lr <- runEitherT $ do

    resource_user <- mustT $ getUserM user_id resourceUserId
    resource_stat <- mustT $ getResourceStatM user_id resource_id
--  resource_like <- mustT $ getResourceLike_ByResourceM user_id resource
--  resource_star <- mustT $ getResourceStar_ByResourceM user_id resource
    pure (resource_user, resource_stat)

  rehtie lr leftA $ \(resource_user, resource_stat) -> do
    rightA $ ResourcePackResponse {
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
