{-# LANGUAGE RecordWildCards #-}

module LN.All.BucketResource (
  -- Handler
  postBucketResourceR,
  deleteBucketResourceR,

  -- Model/Internal
  getBucketResourcesM,
  getBucketResources_ParameterizedM,
  getBucketResourceM,
  insertBucketResourceM,
  deleteBucketResourceM,
  countBucketResourcesM,

 -- Other
  getBucketResourceIdsR,
  getBucketResourceIdsM,


) where



import           Control.Monad.Trans.State
import qualified Control.Monad.Trans.State as StateT

import           LN.All.Prelude
import           LN.All.Resource



--
-- Handler
--

postBucketResourceR :: BucketId -> ResourceId -> Handler Value
postBucketResourceR bucket_id resource_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON (pure ()) $ insertBucketResourceM user_id bucket_id resource_id



deleteBucketResourceR :: BucketId -> ResourceId -> Handler Value
deleteBucketResourceR bucket_id resource_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteBucketResourceM user_id bucket_id resource_id





--
-- Model/Internal
--

getBucketResourcesM :: Maybe StandardParams -> UserId -> BucketId -> HandlerErrorEff [Entity Resource]
getBucketResourcesM = getBucketResources_ParameterizedM



-- | Handles all possible query parameters
--
getBucketResources_ParameterizedM :: Maybe StandardParams -> UserId -> BucketId -> HandlerErrorEff [Entity Resource]
getBucketResources_ParameterizedM m_sp user_id bucket_id = do
  evalStateT go []
  where
  go = do

    -- user_id=
    ebyam (lookupSpMay m_sp spUserId) (pure ()) $ \lookup_user_id ->
      modify (\st->st <> [BucketResourceUserId ==. lookup_user_id])

    params <- ([BucketResourceBucketId ==. bucket_id, BucketResourceActive ==. True] <>) <$> StateT.get

    e_bucket_resources <- lift $ selectListDbE m_sp params [] BucketResourceId

    rehtie e_bucket_resources leftA $ \bucket_resources -> do
      v <- rights <$> mapM (\(Entity _ BucketResource{..}) -> lift $ getResourceM user_id bucketResourceResourceId) bucket_resources
      rightA v



getBucketResourceM :: UserId -> BucketResourceId -> HandlerErrorEff (Entity BucketResource)
getBucketResourceM _ bucket_resource_id = do
  selectFirstDbE [BucketResourceId ==. bucket_resource_id, BucketResourceActive ==. True] []



insertBucketResourceM :: UserId -> BucketId -> ResourceId -> HandlerErrorEff (Entity BucketResource)
insertBucketResourceM user_id bucket_id resource_id = do

  ts <- timestampH'

  let
    bucketResource = BucketResource {
      bucketResourceUserId     = user_id,
      bucketResourceBucketId   = bucket_id,
      bucketResourceResourceId = resource_id,
      bucketResourceActive     = True,
      bucketResourceCreatedAt  = Just ts
    }

  insertEntityDbE bucketResource



deleteBucketResourceM :: UserId -> BucketId -> ResourceId -> HandlerErrorEff ()
deleteBucketResourceM user_id bucket_id resource_id = do
  deleteWhereDbE [BucketResourceUserId ==. user_id, BucketResourceBucketId ==. bucket_id, BucketResourceResourceId ==. resource_id, BucketResourceActive ==. True]



countBucketResourcesM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countBucketResourcesM m_sp _ = do

  case (lookupSpMay m_sp spUserId, lookupSpMay m_sp spUserIds) of

    -- TODO FIXME: not handling argument properly
    _ -> do
      n <- countDb [BucketResourceActive ==. True]
      rightA $ CountResponses [CountResponse 0 (fromIntegral n)]






--
-- Other
--

getBucketResourceIdsR :: BucketId -> Handler Value
getBucketResourceIdsR bucket_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getBucketResourceIdsM user_id bucket_id



getBucketResourceIdsM :: UserId -> BucketId -> HandlerErrorEff SimpleIntsResponse
getBucketResourceIdsM _ bucket_id = do
  e_ents <- selectListDbE Nothing [BucketResourceBucketId ==. bucket_id, BucketResourceActive ==. True] [] BucketResourceId
  rehtie e_ents leftA $ \ents -> do
    rightA $ SimpleIntsResponse $ map (\(Entity _ BucketResource{..}) -> keyToInt64 bucketResourceResourceId) ents
