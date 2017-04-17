{-# LANGUAGE RecordWildCards #-}

module LN.All.BucketLeuron (
  -- Handler
  postBucketLeuronR,
  deleteBucketLeuronR,

  -- Model/Internal
  getBucketLeuronsM,
  getBucketLeurons_ParameterizedM,
  getBucketLeuronM,
  insertBucketLeuronM,
  deleteBucketLeuronM,
  countBucketLeuronsM,

  -- Other
  getBucketLeuronIdsR,
  getBucketLeuronIdsM,

) where



import           Control.Monad.Trans.State
import qualified Control.Monad.Trans.State as StateT

import           LN.All.Prelude
import           LN.All.Leuron



--
-- Handler
--

postBucketLeuronR :: BucketId -> LeuronId -> Handler Value
postBucketLeuronR bucket_id leuron_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON (pure ()) $ insertBucketLeuronM user_id bucket_id leuron_id



deleteBucketLeuronR :: BucketId -> LeuronId -> Handler Value
deleteBucketLeuronR bucket_id leuron_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteBucketLeuronM user_id bucket_id leuron_id





--
-- Model/Internal
--

getBucketLeuronsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity BucketLeuron]
getBucketLeuronsM m_sp user_id = do

  case (lookupSpMay m_sp spUserId) of

    Just lookup_user_id -> getBucketLeurons_ByUserIdM m_sp user_id lookup_user_id
    _                   -> getBucketLeurons_ByEverythingM m_sp user_id



getBucketLeurons_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity BucketLeuron]
getBucketLeurons_ByEverythingM m_sp _ = do
  selectListDbE m_sp [BucketLeuronActive ==. True] [] BucketLeuronId



getBucketLeurons_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity BucketLeuron]
getBucketLeurons_ByUserIdM m_sp _ lookup_user_id = do
  selectListDbE m_sp [BucketLeuronUserId ==. lookup_user_id, BucketLeuronActive ==. True] [] BucketLeuronId



getBucketLeuronM :: UserId -> BucketLeuronId -> HandlerErrorEff (Entity BucketLeuron)
getBucketLeuronM _ bucket_leuron_id = do
  selectFirstDbE [BucketLeuronId ==. bucket_leuron_id, BucketLeuronActive ==. True] []



insertBucketLeuronM :: UserId -> BucketId -> LeuronId -> HandlerErrorEff (Entity BucketLeuron)
insertBucketLeuronM user_id bucket_id leuron_id = do

  ts <- timestampH'

  let
    bucketLeuron = BucketLeuron {
      bucketLeuronUserId     = user_id,
      bucketLeuronBucketId   = bucket_id,
      bucketLeuronLeuronId = leuron_id,
      bucketLeuronActive     = True,
      bucketLeuronCreatedAt  = Just ts
    }

  insertEntityDbE bucketLeuron



deleteBucketLeuronM :: UserId -> BucketId -> LeuronId -> HandlerErrorEff ()
deleteBucketLeuronM user_id bucket_id leuron_id = do
  deleteWhereDbE [BucketLeuronUserId ==. user_id, BucketLeuronBucketId ==. bucket_id, BucketLeuronLeuronId ==. leuron_id, BucketLeuronActive ==. True]



countBucketLeuronsM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countBucketLeuronsM m_sp _ = do

  case (lookupSpMay m_sp spUserId, lookupSpMay m_sp spUserIds) of

    -- TODO FIXME: not handling argument properly
    _ -> do
      n <- countDb [BucketLeuronActive ==. True]
      rightA $ CountResponses [CountResponse 0 (fromIntegral n)]





--
-- Other
--

getBucketLeuronIdsR :: BucketId -> Handler Value
getBucketLeuronIdsR bucket_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getBucketLeuronIdsM user_id bucket_id



getBucketLeuronIdsM :: UserId -> BucketId -> HandlerErrorEff SimpleIntsResponse
getBucketLeuronIdsM _ bucket_id = do
  e_ents <- selectListDbE Nothing [BucketLeuronBucketId ==. bucket_id, BucketLeuronActive ==. True] [] BucketLeuronId
  rehtie e_ents leftA $ \ents -> do
    rightA $ SimpleIntsResponse $ map (\(Entity _ BucketLeuron{..}) -> keyToInt64 bucketLeuronLeuronId) ents
