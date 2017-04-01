{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.Bucket (
  -- LN.Handler
  getBucketPacksR,
  getBucketPackR,

  -- LN.Model
  getBucketPacksM,
  getBucketPackM
) where



import           LN.All.Internal
import           LN.All.Prelude
import           LN.All.Bucket
import           LN.All.User



--
-- LN.Handler
--

getBucketPacksR :: Handler Value
getBucketPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getBucketPacksM (pure sp) user_id



getBucketPackR :: BucketId -> Handler Value
getBucketPackR thread_post_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getBucketPackM user_id thread_post_id






--
-- LN.Model
--

getBucketPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff BucketPackResponses
getBucketPacksM m_sp user_id = do

  e_buckets <- getBucketsM m_sp user_id
  rehtie e_buckets leftA $ \buckets -> do

    bucket_packs <- rights <$> mapM (\bucket -> getBucketPack_ByBucketM user_id bucket) buckets

    rightA $ BucketPackResponses {
      bucketPackResponses = bucket_packs
    }



getBucketPackM :: UserId -> BucketId -> HandlerErrorEff BucketPackResponse
getBucketPackM user_id bucket_id = do

  e_bucket <- getBucketM user_id bucket_id
  rehtie e_bucket leftA $ getBucketPack_ByBucketM user_id



getBucketPack_ByBucketM :: UserId -> Entity Bucket -> HandlerErrorEff BucketPackResponse
getBucketPack_ByBucketM user_id bucket@(Entity bucket_id Bucket{..}) = do

  lr <- runEitherT $ do

    bucket_user <- mustT $ getUserM user_id bucketUserId
    pure bucket_user

  rehtie lr leftA $ \bucket_user -> do
    rightA $ BucketPackResponse {
      bucketPackResponseBucket    = bucketToResponse bucket,
      bucketPackResponseBucketId  = keyToInt64 bucket_id,
      bucketPackResponseUser      = userToSanitizedResponse bucket_user,
      bucketPackResponseUserId    = entityKeyToInt64 bucket_user
    }
