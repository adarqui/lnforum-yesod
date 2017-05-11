{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.Leuron (
  -- LN.Handler
  getLeuronPacksR,
  getLeuronPackR,

  -- LN.Model
  getLeuronPacksM,
  getLeuronPackM
) where



import           LN.All.Internal
import           LN.All.BucketLeuron
import           LN.All.BucketRound
import           LN.All.Leuron
import           LN.All.LeuronTraining
import           LN.All.Prelude
import           LN.All.User



--
-- LN.Handler
--

getLeuronPacksR :: Handler Value
getLeuronPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getLeuronPacksM (pure sp) user_id



getLeuronPackR :: LeuronId -> Handler Value
getLeuronPackR leuron_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getLeuronPackM user_id leuron_id







--
-- LN.Model
--

getLeuronPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff LeuronPackResponses
getLeuronPacksM m_sp user_id = do

  e_leurons <-
    case (lookupSpMay m_sp spBucketId, lookupSpMay m_sp spBucketRoundId) of
      (Just bucket_id, Nothing)       -> getBucketLeuronsM m_sp user_id bucket_id
      (Nothing, Just bucket_round_id) -> getBucketRoundLeuronsM m_sp user_id bucket_round_id
      (_, _)                          -> getLeuronsM m_sp user_id

  rehtie e_leurons leftA $ \leurons -> do

    leuron_packs <- fmap rights $ mapM (\leuron -> getLeuronPack_ByLeuronM user_id leuron) leurons

    rightA $ LeuronPackResponses {
      leuronPackResponses = leuron_packs
    }



getLeuronPackM :: UserId -> LeuronId -> HandlerErrorEff LeuronPackResponse
getLeuronPackM user_id leuron_id = do

  e_leuron <- getLeuronM user_id leuron_id
  rehtie e_leuron leftA $ \leuron -> getLeuronPack_ByLeuronM user_id leuron



getLeuronPack_ByLeuronM :: UserId -> Entity Leuron -> HandlerErrorEff LeuronPackResponse
getLeuronPack_ByLeuronM user_id leuron@(Entity leuron_id Leuron{..}) = do

  lr <- runEitherT $ do
    leuron_user     <- mustT $ getUserM user_id leuronUserId
    leuron_stat     <- mustT $ getLeuronStatM user_id leuron_id
    leuron_training <- mustT $ insertLeuronTrainingM user_id leuron_id $ LeuronTrainingRequest LTS_View 0

    pure (leuron_user
         ,leuron_stat
         ,leuron_training)

  rehtie lr leftA $ \(leuron_user, leuron_stat, leuron_training) -> do

    rightA $ LeuronPackResponse {
      leuronPackResponseLeuron      = leuronToResponse leuron,
      leuronPackResponseLeuronId    = keyToInt64 leuron_id,
      leuronPackResponseUser        = userToSanitizedResponse leuron_user,
      leuronPackResponseUserId      = entityKeyToInt64 leuron_user,
      -- TODO FIXME
      leuronPackResponseTraining    = leuronTrainingToResponse leuron_training,
      -- TODO FIXME
      -- leuronPackResponseTrainingId
      leuronPackResponseStat        = leuron_stat,
      leuronPackResponseLike        = Nothing,
      leuronPackResponseStar        = Nothing,
      leuronPackResponsePermissions = emptyPermissions
  --    leuronPackResponseLike     = fmap leuronLikeToResponse leuron_like,
  --    leuronPackResponseStar     = fmap leuronStarToResponse leuron_star
    }
