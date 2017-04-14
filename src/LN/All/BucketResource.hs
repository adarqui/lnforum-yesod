{-# LANGUAGE RecordWildCards #-}

module LN.All.BucketResource (
  -- Handler
  postBucketResourcesR,
  deleteBucketResourceR,

  -- Model/Function
  bucketResourceRequestToBucketResource,
  bucketResourceToResponse,
  -- bucketResourcesToResponses,

  -- Model/Internal
  getBucketResourcesM,
  getBucketResources_ByEverythingM,
  getBucketResources_ByUserIdM,
  getBucketResourceM,
  insertBucketResourceM,
  deleteBucketResourceM,
  countBucketResourcesM
) where



import           LN.All.Prelude
import LN.T.Id



--
-- Handler
--

postBucketResourcesR :: BucketId -> ResourceId -> Handler Value
postBucketResourcesR bucket_id resource_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON bucketResourceToResponse $ insertBucketResourceM user_id bucket_id resource_id



deleteBucketResourceR :: BucketId -> ResourceId -> Handler Value
deleteBucketResourceR bucket_id resource_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteBucketResourceM user_id bucket_id resource_id





--
-- Model/Function
--

bucketResourceRequestToBucketResource :: UserId -> IdRequest -> BucketResource
bucketResourceRequestToBucketResource user_id IdRequest{..} = BucketResource {
  bucketResourceUserId         = user_id,
  bucketResourceBucketId       = int64ToKey' 0,
  bucketResourceResourceId     = int64ToKey' idRequestTargetId,
  bucketResourceCreatedAt      = Nothing,
  bucketResourceActive         = True
}



bucketResourceToResponse :: Entity BucketResource -> IdResponse
bucketResourceToResponse (Entity bucket_resource_id BucketResource{..}) = IdResponse {
  idResponseId                  = keyToInt64 bucket_resource_id,
  idResponseUserId              = keyToInt64 bucketResourceUserId,
  idResponseTargetId            = keyToInt64 bucketResourceResourceId,
  idResponseGuard               = 0,
  idResponseCreatedAt           = bucketResourceCreatedAt,
  idResponseModifiedAt          = Nothing,
  idResponseActivityAt          = Nothing
}



{-
bucketResourcesToResponses :: [Entity BucketResource] -> IdResponses
bucketResourcesToResponses bucket_resources = idResponses {
  idResponses = map bucketResourceToResponse bucket_resources
}
-}








--
-- Model/Internal
--

getBucketResourcesM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity BucketResource]
getBucketResourcesM m_sp user_id = do

  case (lookupSpMay m_sp spUserId) of

    Just lookup_user_id -> getBucketResources_ByUserIdM m_sp user_id lookup_user_id
    _                   -> getBucketResources_ByEverythingM m_sp user_id



getBucketResources_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity BucketResource]
getBucketResources_ByEverythingM m_sp _ = do
  selectListDbE m_sp [BucketResourceActive ==. True] [] BucketResourceId



getBucketResources_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity BucketResource]
getBucketResources_ByUserIdM m_sp _ lookup_user_id = do
  selectListDbE m_sp [BucketResourceUserId ==. lookup_user_id, BucketResourceActive ==. True] [] BucketResourceId



getBucketResourceM :: UserId -> BucketResourceId -> HandlerErrorEff (Entity BucketResource)
getBucketResourceM _ bucket_resource_id = do
  selectFirstDbE [BucketResourceId ==. bucket_resource_id, BucketResourceActive ==. True] []



insertBucketResourceM :: UserId -> IdRequest -> HandlerErrorEff (Entity BucketResource)
insertBucketResourceM user_id id_resource_request = do

  ts <- timestampH'

  let
    bucketResource = (bucketResourceRequestToBucketResource user_id id_resource_request) { bucketResourceCreatedAt = Just ts }

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
