{-# LANGUAGE RecordWildCards #-}

module LN.All.BucketRound (
  -- Handler
  getBucketRoundsR,
  postBucketRoundsR,
  getBucketRoundR,
  putBucketRoundR,
  deleteBucketRoundR,
  getBucketRoundsCountR,

  -- Model/Function
  bucketRoundRequestToBucketRound,
  bucketRoundToResponse,
  bucketRoundsToResponses,

  -- Model/Internal
  getBucketRoundsM,
  getBucketRounds_ByEverythingM,
  getBucketRounds_ByUserIdM,
  getBucketRoundM,
  insertBucketRoundM,
  updateBucketRoundM,
  deleteBucketRoundM,
  countBucketRoundsM,

  getBucketRoundLeuronsM,
  getBucketRoundLeuronM
) where



import           Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import qualified Database.Redis     as R

import           LN.All.Leuron
import           LN.All.Prelude
import           LN.All.BucketResource
import           LN.All.Bucket
import           LN.All.Resource
import           LN.Misc



--
-- Handler
--

getBucketRoundsR :: Handler Value
getBucketRoundsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON bucketRoundsToResponses $ getBucketRoundsM (pure sp) user_id



postBucketRoundsR :: Handler Value
postBucketRoundsR = run $ do
  user_id              <- _requireAuthId
  sp                   <- lookupStandardParams
  bucket_round_request <- requireJsonBody
  errorOrJSON bucketRoundToResponse $ insertBucketRoundM (pure sp) user_id bucket_round_request



getBucketRoundR :: BucketRoundId -> Handler Value
getBucketRoundR bucket_round_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON bucketRoundToResponse $ getBucketRoundM user_id bucket_round_id



putBucketRoundR :: BucketRoundId -> Handler Value
putBucketRoundR bucket_round_id = run $ do
  user_id          <- _requireAuthId
  bucket_round_request <- requireJsonBody
  errorOrJSON bucketRoundToResponse $ updateBucketRoundM user_id bucket_round_id bucket_round_request



deleteBucketRoundR :: BucketRoundId -> Handler Value
deleteBucketRoundR bucket_round_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteBucketRoundM user_id bucket_round_id



getBucketRoundsCountR :: Handler Value
getBucketRoundsCountR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ countBucketRoundsM (pure sp) user_id





--
-- Model/Function
--

bucketRoundRequestToBucketRound :: UserId -> BucketId -> BucketRoundRequest -> BucketRound
bucketRoundRequestToBucketRound user_id bucket_id BucketRoundRequest{..} = BucketRound {
  bucketRoundUserId          = user_id,
  bucketRoundBucketId        = bucket_id,

  bucketRoundTrainingStyles    = bucketRoundRequestTrainingStyles,
  bucketRoundThreshold         = 0,
  bucketRoundTimeLimit         = 0,
  bucketRoundNumTotal          = 0,
  bucketRoundNumKnow           = 0,
  bucketRoundNumDontKnow       = 0,
  bucketRoundNumDontCare       = 0,
  bucketRoundNumProtest        = 0,
  bucketRoundHonorKnow         = 0,
  bucketRoundHonorKnowAt       = Nothing,
  bucketRoundHonorDontKnow     = 0,
  bucketRoundHonorDontKnowAt   = Nothing,
  bucketRoundHonorDontCare     = 0,
  bucketRoundHonorDontCareAt   = Nothing,
  bucketRoundHonorProtest      = 0,
  bucketRoundHonorProtestAt    = Nothing,
  bucketRoundBooleanKnow       = 0,
  bucketRoundBooleanKnowAt     = Nothing,
  bucketRoundBooleanDontKnow   = 0,
  bucketRoundBooleanDontKnowAt = Nothing,
  bucketRoundBooleanDontCare   = 0,
  bucketRoundBooleanDontCareAt = Nothing,
  bucketRoundBooleanProtest    = 0,
  bucketRoundBooleanProtestAt  = Nothing,
  bucketRoundMatchKnow         = 0,
  bucketRoundMatchKnowAt       = Nothing,
  bucketRoundMatchDontKnow     = 0,
  bucketRoundMatchDontKnowAt   = Nothing,
  bucketRoundMatchDontCare     = 0,
  bucketRoundMatchDontCareAt   = Nothing,
  bucketRoundMatchProtest      = 0,
  bucketRoundMatchProtestAt    = Nothing,
  bucketRoundSubsKnow          = 0,
  bucketRoundSubsKnowAt        = Nothing,
  bucketRoundSubsDontKnow      = 0,
  bucketRoundSubsDontKnowAt    = Nothing,
  bucketRoundSubsDontCare      = 0,
  bucketRoundSubsDontCareAt    = Nothing,
  bucketRoundSubsProtest       = 0,
  bucketRoundSubsProtestAt     = Nothing,
  bucketRoundSplitsKnow        = 0,
  bucketRoundSplitsKnowAt      = Nothing,
  bucketRoundSplitsDontKnow    = 0,
  bucketRoundSplitsDontKnowAt  = Nothing,
  bucketRoundSplitsDontCare    = 0,
  bucketRoundSplitsDontCareAt  = Nothing,
  bucketRoundSplitsProtest     = 0,
  bucketRoundSplitsProtestAt   = Nothing,

  bucketRoundStartedAt         = Nothing,
  bucketRoundFinished          = False,
  bucketRoundFinishedAt        = Nothing,

  bucketRoundActive          = True,
  bucketRoundGuard           = bucketRoundRequestGuard,
  bucketRoundCreatedAt       = Nothing,
  bucketRoundModifiedAt      = Nothing
}



bucketRoundToResponse :: Entity BucketRound -> BucketRoundResponse
bucketRoundToResponse (Entity bucket_round_id BucketRound{..}) = BucketRoundResponse {
  bucketRoundResponseId             = keyToInt64 bucket_round_id,
  bucketRoundResponseUserId         = keyToInt64 bucketRoundUserId,
  bucketRoundResponseBucketId       = keyToInt64 bucketRoundBucketId,

  bucketRoundResponseTrainingStyles = bucketRoundTrainingStyles,
  bucketRoundResponseTrainingNode   = defaultTrainingNode,
  bucketRoundResponseThreshold      = bucketRoundThreshold,
  bucketRoundResponseTimeLimit      = bucketRoundTimeLimit,

  bucketRoundResponseActive         = bucketRoundActive,
  bucketRoundResponseGuard          = bucketRoundGuard,
  bucketRoundResponseCreatedAt      = bucketRoundCreatedAt,
  bucketRoundResponseModifiedAt     = bucketRoundModifiedAt,
  bucketRoundResponseActivityAt     = Nothing
}



bucketRoundsToResponses :: [Entity BucketRound] -> BucketRoundResponses
bucketRoundsToResponses bucketRounds = BucketRoundResponses {
  bucketRoundResponses = map bucketRoundToResponse bucketRounds
}








--
-- Model/Internal
--

getBucketRoundsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity BucketRound]
getBucketRoundsM m_sp user_id = do

  case (lookupSpMay m_sp spUserId) of

    Just lookup_user_id -> getBucketRounds_ByUserIdM m_sp user_id lookup_user_id
    _                   -> getBucketRounds_ByEverythingM m_sp user_id



getBucketRounds_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity BucketRound]
getBucketRounds_ByEverythingM m_sp _ = do
  selectListDbE m_sp [BucketRoundActive ==. True] [] BucketRoundId



getBucketRounds_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity BucketRound]
getBucketRounds_ByUserIdM m_sp _ lookup_user_id = do
  selectListDbE m_sp [BucketRoundUserId ==. lookup_user_id, BucketRoundActive ==. True] [] BucketRoundId



getBucketRoundM :: UserId -> BucketRoundId -> HandlerErrorEff (Entity BucketRound)
getBucketRoundM _ bucket_round_id = do
  selectFirstDbE [BucketRoundId ==. bucket_round_id, BucketRoundActive ==. True] []



insertBucketRoundM :: Maybe StandardParams -> UserId -> BucketRoundRequest -> HandlerErrorEff (Entity BucketRound)
insertBucketRoundM m_sp user_id bucket_round_request = do

  rehtie (validateBucketRoundRequest bucket_round_request) (leftA . Error_Validation) $ const $ do

    case lookupSpMay m_sp spBucketId of
      Just bucket_id -> do

        ts <- timestampH'

        let
          bucketRound = (bucketRoundRequestToBucketRound user_id bucket_id bucket_round_request) { bucketRoundCreatedAt = Just ts }

        lr <- insertEntityDbE bucketRound
        rehtie lr leftA $ \entity@(Entity round_id BucketRound{..}) -> do

          --
          -- Grab resources & leurons, add them to redis bucket
          --

          lr_ <- runEitherT $ do

            resources <- mustT $ getBucketResourcesM Nothing user_id bucket_id
            -- bucket_leurons   <- mustT $ getBucketLeurons Nothing user_id Nothing user_id bucket_id
            -- pure (ucket_resources, bucket_leurons)
            --
            pure resources

          rehtie lr_ leftA $ \resources -> do

            leuron_ids <- forM resources $ \(Entity resource_id Resource{..}) -> do

              leuron_ids <- _runDB
                $ E.select $ E.from $ \leuron -> do
                    E.where_ ((leuron ^. LeuronResourceId) E.==. E.val resource_id)
                    E.where_ $ E.notExists $ E.from $ \leuron_node -> do

                      -- TODO FIXME
                      let

                        nop = (E.val False) E.==. (E.val False)

                        where_honor_know = if TS_Simple `elem` bucketRoundTrainingStyles
                                              then ((leuron_node ^. LeuronNodeHonorKnow) E.>. E.val 3)
                                              else nop

                        where_boolean_know = if TS_Boolean `elem` bucketRoundTrainingStyles
                                                then ((leuron_node ^. LeuronNodeBooleanKnow) E.>. E.val 3)
                                                else nop

                        where_matching_know = if TS_Match `elem` bucketRoundTrainingStyles
                                                then ((leuron_node ^. LeuronNodeMatchKnow) E.>. E.val 3)
                                                else nop

                        where_subs_know = if TS_Subs `elem` bucketRoundTrainingStyles
                                                then ((leuron_node ^. LeuronNodeSubsKnow) E.>. E.val 3)
                                                else nop

                        where_splits_know = if TS_Splits `elem` bucketRoundTrainingStyles
                                                then ((leuron_node ^. LeuronNodeSplitsKnow) E.>. E.val 3)
                                                else nop
                      E.where_ (
                        ((leuron ^. LeuronId) E.==. (leuron_node ^. LeuronNodeLeuronId))
                        E.&&. ((leuron_node ^. LeuronNodeUserId) E.==. E.val user_id)
                        -- calculation section
                        E.&&. where_honor_know
                        E.&&. ((leuron_node ^. LeuronNodeHonorDontCare) E.!=. E.val 0)
                        E.&&. where_boolean_know
                        E.&&. ((leuron_node ^. LeuronNodeBooleanDontCare) E.!=. E.val 0)
                        E.&&. where_matching_know
                        E.&&. ((leuron_node ^. LeuronNodeMatchDontCare) E.!=. E.val 0)
                        E.&&. where_subs_know
                        E.&&. ((leuron_node ^. LeuronNodeSubsDontCare) E.!=. E.val 0)
                        E.&&. where_splits_know
                        E.&&. ((leuron_node ^. LeuronNodeSplitsDontCare) E.!=. E.val 0))
                    pure (leuron ^. LeuronId)

-- add bucketroundrequest to sanitize
-- add a bunch of go's to the end of this, to account for all of the different training types
-- add leurons, not just resources


              liftIO $ print leuron_ids

              let
                l_ids = map (keyToInt64 . E.unValue) leuron_ids

              red <- getsYesod appRed
              liftIO $ R.runRedis red $ do
                R.sadd (cs $ "round:" <> show (keyToInt64 round_id)) $ map (cs . show) l_ids

              pure ()

            rightA ()

          rightA entity

      _ -> leftA $ Error_InvalidArguments "bucket_id"



updateBucketRoundM :: UserId -> BucketRoundId -> BucketRoundRequest -> HandlerErrorEff (Entity BucketRound)
updateBucketRoundM user_id bucket_round_id bucket_round_request = do

  ts <- timestampH'

  let
    BucketRound{..} = (bucketRoundRequestToBucketRound user_id (int64ToKey' 0) bucket_round_request) { bucketRoundModifiedAt = Just ts }

  updateWhereDb
    [ BucketRoundUserId ==. user_id, BucketRoundId ==. bucket_round_id, BucketRoundActive ==. True ]
    [ BucketRoundModifiedAt    =. bucketRoundModifiedAt
    , BucketRoundGuard        +=. 1
    ]

  selectFirstDbE [BucketRoundUserId ==. user_id, BucketRoundId ==. bucket_round_id, BucketRoundActive ==. True] []



deleteBucketRoundM :: UserId -> BucketRoundId -> HandlerErrorEff ()
deleteBucketRoundM user_id bucket_round_id = do
  deleteWhereDbE [BucketRoundUserId ==. user_id, BucketRoundId ==. bucket_round_id, BucketRoundActive ==. True]



countBucketRoundsM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countBucketRoundsM m_sp _ = do

  case (lookupSpMay m_sp spUserId, lookupSpMay m_sp spUserIds) of

    -- TODO FIXME: not handling argument properly
    _ -> do
      n <- countDb [BucketRoundActive ==. True]
      rightA $ CountResponses [CountResponse 0 (fromIntegral n)]



getBucketRoundLeuronsM :: Maybe StandardParams -> UserId -> BucketRoundId -> HandlerErrorEff [Entity Leuron]
getBucketRoundLeuronsM m_sp user_id bucket_round_id = do
  lr <- getBucketRoundLeuronM m_sp user_id bucket_round_id
  rehtie lr leftA $ \l -> rightA [l]



getBucketRoundLeuronM :: Maybe StandardParams -> UserId -> BucketRoundId -> HandlerErrorEff (Entity Leuron)
getBucketRoundLeuronM m_sp user_id bucket_round_id = do

  red <- getsYesod appRed
  lr <- liftIO $ R.runRedis red $ do
         R.spop (cs $ "round:" <> show (keyToInt64 bucket_round_id))
  rehtie lr (const $ leftA Error_Unknown) $ \m_v -> do
    case m_v of
      Nothing        -> leftA Error_Unknown
      Just leuron_id_bs -> do
        getLeuronM user_id (bscToKey' leuron_id_bs)

