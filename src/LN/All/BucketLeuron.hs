{-# LANGUAGE RecordWildCards #-}

module LN.All.BucketLeuron (
  -- Handler
  postBucketLeuronsR,
  deleteBucketLeuronR,

  -- Model/Function
  bucketLeuronRequestToBucketLeuron,
  bucketLeuronToResponse,
  -- bucketLeuronsToResponses,

  -- Model/Internal
  getBucketLeuronsM,
  getBucketLeurons_ByEverythingM,
  getBucketLeurons_ByUserIdM,
  getBucketLeuronM,
  insertBucketLeuronM,
  deleteBucketLeuronM,
  countBucketLeuronsM
) where



import           LN.All.Prelude



--
-- Handler
--

postBucketLeuronsR :: Handler Value
postBucketLeuronsR = run $ do
  user_id          <- _requireAuthId
  id_leuron_request <- requireJsonBody
  errorOrJSON bucketLeuronToResponse $ insertBucketLeuronM user_id id_leuron_request



deleteBucketLeuronR :: BucketLeuronId -> Handler Value
deleteBucketLeuronR bucketLeuron_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteBucketLeuronM user_id bucketLeuron_id





--
-- Model/Function
--

bucketLeuronRequestToBucketLeuron :: UserId -> IdRequest -> BucketLeuron
bucketLeuronRequestToBucketLeuron user_id IdRequest{..} = BucketLeuron {
  bucketLeuronUserId         = user_id,
  bucketLeuronBucketId       = int64ToKey' 0,
  bucketLeuronLeuronId     = int64ToKey' idRequestTargetId,
  bucketLeuronCreatedAt      = Nothing,
  bucketLeuronActive         = True
}



bucketLeuronToResponse :: Entity BucketLeuron -> IdResponse
bucketLeuronToResponse (Entity bucket_leuron_id BucketLeuron{..}) = IdResponse {
  idResponseId                  = keyToInt64 bucket_leuron_id,
  idResponseUserId              = keyToInt64 bucketLeuronUserId,
  idResponseTargetId            = keyToInt64 bucketLeuronLeuronId,
  idResponseGuard               = 0,
  idResponseCreatedAt           = bucketLeuronCreatedAt,
  idResponseModifiedAt          = Nothing,
  idResponseActivityAt          = Nothing
}



{-
bucketLeuronsToResponses :: [Entity BucketLeuron] -> IdResponses
bucketLeuronsToResponses bucket_leurons = idResponses {
  idResponses = map bucketLeuronToResponse bucket_leurons
}
-}








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
getBucketLeuronM _ bucketLeuron_id = do
  selectFirstDbE [BucketLeuronId ==. bucketLeuron_id, BucketLeuronActive ==. True] []



insertBucketLeuronM :: UserId -> IdRequest -> HandlerErrorEff (Entity BucketLeuron)
insertBucketLeuronM user_id id_leuron_request = do

  ts <- timestampH'

  let
    bucketLeuron = (bucketLeuronRequestToBucketLeuron user_id id_leuron_request) { bucketLeuronCreatedAt = Just ts }

  insertEntityDbE bucketLeuron



deleteBucketLeuronM :: UserId -> BucketLeuronId -> HandlerErrorEff ()
deleteBucketLeuronM user_id bucketLeuron_id = do
  deleteWhereDbE [BucketLeuronUserId ==. user_id, BucketLeuronId ==. bucketLeuron_id, BucketLeuronActive ==. True]



countBucketLeuronsM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countBucketLeuronsM m_sp _ = do

  case (lookupSpMay m_sp spUserId, lookupSpMay m_sp spUserIds) of

    -- TODO FIXME: not handling argument properly
    _ -> do
      n <- countDb [BucketLeuronActive ==. True]
      rightA $ CountResponses [CountResponse 0 (fromIntegral n)]
