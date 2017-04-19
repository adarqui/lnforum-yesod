{-# LANGUAGE RecordWildCards #-}

module LN.All.Bucket (
  -- Handler
  getBucketsR,
  postBucketsR,
  getBucketR,
  putBucketR,
  deleteBucketR,
  getBucketsCountR,

  -- Model/Function
  bucketRequestToBucket,
  bucketToResponse,
  bucketsToResponses,

  -- Model/Internal
  getBucketsM,
  getBuckets_ByEverythingM,
  getBuckets_ByUserIdM,
  getBucketM,
  insertBucketM,
  updateBucketM,
  deleteBucketM,
  countBucketsM
) where



import           LN.All.Prelude



--
-- Handler
--

getBucketsR :: Handler Value
getBucketsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON bucketsToResponses $ getBucketsM (pure sp) user_id



postBucketsR :: Handler Value
postBucketsR = run $ do
  user_id          <- _requireAuthId
  bucket_request <- requireJsonBody
  errorOrJSON bucketToResponse $ insertBucketM user_id bucket_request



getBucketR :: BucketId -> Handler Value
getBucketR bucket_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON bucketToResponse $ getBucketM user_id bucket_id



putBucketR :: BucketId -> Handler Value
putBucketR bucket_id = run $ do
  user_id          <- _requireAuthId
  bucket_request <- requireJsonBody
  errorOrJSON bucketToResponse $ updateBucketM user_id bucket_id bucket_request



deleteBucketR :: BucketId -> Handler Value
deleteBucketR bucket_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteBucketM user_id bucket_id



getBucketsCountR :: Handler Value
getBucketsCountR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ countBucketsM (pure sp) user_id





--
-- Model/Function
--

bucketRequestToBucket :: UserId -> BucketRequest -> Bucket
bucketRequestToBucket user_id BucketRequest{..} = Bucket {
  bucketUserId          = user_id,
  bucketName            = toSafeUrl bucketRequestDisplayName,
  bucketDisplayName     = bucketRequestDisplayName,
  bucketDescription     = bucketRequestDescription,
  bucketCategories      = [],
  bucketRounds          = 0,
  bucketScoreLo         = bucketRequestScoreLo,
  bucketScoreHi         = bucketRequestScoreHi,
  bucketLeurons         = [], -- bucketRequestLeurons,
  bucketResources       = [], -- bucketRequestResources,
  bucketFilters         = [],

  bucketMaxRounds       = 0,
  bucketTrainingStyles  = [],
  bucketThreshold       = 0,
  bucketTimeLimit       = 0,
  bucketNumKnow         = 0,
  bucketNumDontKnow     = 0,
  bucketNumDontCare     = 0,
  bucketNumProtest      = 0,
  bucketHonorKnow       = 0,
  bucketHonorDontKnow   = 0,
  bucketHonorDontCare   = 0,
  bucketHonorProtest    = 0,
  bucketBooleanKnow     = 0,
  bucketBooleanDontKnow = 0,
  bucketBooleanDontCare = 0,
  bucketBooleanProtest  = 0,
  bucketMatchKnow       = 0,
  bucketMatchDontKnow   = 0,
  bucketMatchDontCare   = 0,
  bucketMatchProtest    = 0,
  bucketSubsKnow        = 0,
  bucketSubsDontKnow    = 0,
  bucketSubsDontCare    = 0,
  bucketSubsProtest     = 0,
  bucketSplitsKnow      = 0,
  bucketSplitsDontKnow  = 0,
  bucketSplitsDontCare  = 0,
  bucketSplitsProtest   = 0,

  bucketActive          = True,
  bucketGuard           = bucketRequestGuard,
  bucketCreatedAt       = Nothing,
  bucketModifiedAt      = Nothing,
  bucketActivityAt      = Nothing
}



bucketToResponse :: Entity Bucket -> BucketResponse
bucketToResponse (Entity bucket_id Bucket{..}) = BucketResponse {
  bucketResponseId            = keyToInt64 bucket_id,
  bucketResponseUserId        = keyToInt64 bucketUserId,
  bucketResponseName          = bucketName,
  bucketResponseDisplayName   = bucketDisplayName,
  bucketResponseDescription   = bucketDescription,
  bucketResponseCategories    = [], -- map (\cat -> maybe [] id (decodeText cat)) bucketCategories,
  bucketResponseScoreLo       = bucketScoreLo,
  bucketResponseScoreHi       = bucketScoreHi,
  bucketResponseResources     = [], -- bucketResources,
  bucketResponseLeurons       = [], -- bucketLeurons,
  bucketResponseFilters       = [], -- bucketFilters,
  bucketResponseTrainingNode  = defaultTrainingNode,
  bucketResponseActive        = bucketActive,
  bucketResponseGuard         = bucketGuard,
  bucketResponseCreatedAt     = bucketCreatedAt,
  bucketResponseModifiedAt    = bucketModifiedAt,
  bucketResponseActivityAt    = bucketActivityAt
}



bucketsToResponses :: [Entity Bucket] -> BucketResponses
bucketsToResponses buckets = BucketResponses {
  bucketResponses = map bucketToResponse buckets
}








--
-- Model/Internal
--

getBucketsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Bucket]
getBucketsM m_sp user_id = do

  case (lookupSpMay m_sp spUserId) of

    Just lookup_user_id -> getBuckets_ByUserIdM m_sp user_id lookup_user_id
    _                   -> getBuckets_ByEverythingM m_sp user_id



getBuckets_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Bucket]
getBuckets_ByEverythingM m_sp _ = do
  selectListDbE m_sp [BucketActive ==. True] [] BucketId



getBuckets_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity Bucket]
getBuckets_ByUserIdM m_sp _ lookup_user_id = do
  selectListDbE m_sp [BucketUserId ==. lookup_user_id, BucketActive ==. True] [] BucketId



getBucketM :: UserId -> BucketId -> HandlerErrorEff (Entity Bucket)
getBucketM _ bucket_id = do
  selectFirstDbE [BucketId ==. bucket_id, BucketActive ==. True] []



insertBucketM :: UserId -> BucketRequest -> HandlerErrorEff (Entity Bucket)
insertBucketM user_id bucket_request = do

  ts <- timestampH'

  let
    bucket = (bucketRequestToBucket user_id bucket_request) { bucketCreatedAt = Just ts }

  insertEntityDbE bucket



updateBucketM :: UserId -> BucketId -> BucketRequest -> HandlerErrorEff (Entity Bucket)
updateBucketM user_id bucket_id bucket_request = do

  ts <- timestampH'

  let
    Bucket{..} = (bucketRequestToBucket user_id bucket_request) { bucketModifiedAt = Just ts }

  updateWhereDb
    [ BucketUserId ==. user_id, BucketId ==. bucket_id, BucketActive ==. True ]
    [ BucketModifiedAt    =. bucketModifiedAt
    , BucketName          =. bucketName
    , BucketDisplayName   =. bucketDisplayName
    , BucketDescription   =. bucketDescription
    , BucketCategories    =. bucketCategories
    , BucketScoreLo       =. bucketScoreLo
    , BucketScoreHi       =. bucketScoreHi
    , BucketLeurons       =. bucketLeurons
    , BucketResources     =. bucketResources
    , BucketGuard        +=. 1
    ]

  selectFirstDbE [BucketUserId ==. user_id, BucketId ==. bucket_id, BucketActive ==. True] []



deleteBucketM :: UserId -> BucketId -> HandlerErrorEff ()
deleteBucketM user_id bucket_id = do
  deleteWhereDbE [BucketUserId ==. user_id, BucketId ==. bucket_id, BucketActive ==. True]



countBucketsM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countBucketsM m_sp _ = do

  case (lookupSpMay m_sp spUserId, lookupSpMay m_sp spUserIds) of

    -- TODO FIXME: not handling argument properly
    _ -> do
      n <- countDb [BucketActive ==. True]
      rightA $ CountResponses [CountResponse 0 (fromIntegral n)]
