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
  sp      <- lookupStandardParams
  errorOrJSON (pure ()) $ insertBucketLeuronM (pure sp) user_id bucket_id leuron_id



deleteBucketLeuronR :: BucketId -> LeuronId -> Handler Value
deleteBucketLeuronR bucket_id leuron_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteBucketLeuronM user_id bucket_id leuron_id





--
-- Model/Internal
--

getBucketLeuronsM :: Maybe StandardParams -> UserId -> BucketId -> HandlerErrorEff [Entity Leuron]
getBucketLeuronsM = getBucketLeurons_ParameterizedM



-- | Handles all possible query parameters
--
getBucketLeurons_ParameterizedM :: Maybe StandardParams -> UserId -> BucketId -> HandlerErrorEff [Entity Leuron]
getBucketLeurons_ParameterizedM m_sp user_id bucket_id = do
  evalStateT go []
  where
  go = do

    -- user_id=
    ebyam (lookupSpMay m_sp spUserId) (pure ()) $ \lookup_user_id ->
      modify (\st->st <> [BucketLeuronUserId ==. lookup_user_id])

    params <- ([BucketLeuronBucketId ==. bucket_id, BucketLeuronActive ==. True] <>) <$> StateT.get

    e_bucket_leurons <- lift $ selectListDbE m_sp params [] BucketLeuronId

    rehtie e_bucket_leurons leftA $ \bucket_leurons -> do
      v <- rights <$> mapM (\(Entity _ BucketLeuron{..}) -> lift $ getLeuronM user_id bucketLeuronLeuronId) bucket_leurons
      rightA v



getBucketLeuronM :: UserId -> BucketLeuronId -> HandlerErrorEff (Entity BucketLeuron)
getBucketLeuronM _ bucket_leuron_id = do
  selectFirstDbE [BucketLeuronId ==. bucket_leuron_id, BucketLeuronActive ==. True] []



insertBucketLeuronM :: Maybe StandardParams -> UserId -> BucketId -> LeuronId -> HandlerErrorEff (Entity BucketLeuron)
insertBucketLeuronM m_sp user_id bucket_id leuron_id = do

  case lookupSpMay m_sp spResourceId of
    Just resource_id -> do
      ts <- timestampH'

      let
        bucketLeuron = BucketLeuron {
          bucketLeuronUserId     = user_id,
          bucketLeuronBucketId   = bucket_id,
          bucketLeuronResourceId = resource_id,
          bucketLeuronLeuronId   = leuron_id,
          bucketLeuronActive     = True,
          bucketLeuronCreatedAt  = Just ts
        }

      insertEntityDbE bucketLeuron

    _ -> leftA $ Error_InvalidArguments "resource_id"



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
