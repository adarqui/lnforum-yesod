{-# LANGUAGE RecordWildCards #-}

module All.Pack.Leuron (
  -- Handler
  getLeuronPacksR,
  getLeuronPackR,

  -- Model
  getLeuronPacksM,
  getLeuronPackM
) where



import           All.Leuron
import           All.LeuronTraining
import           All.Prelude
import           All.User



--
-- Handler
--

getLeuronPacksR :: Handler Value
getLeuronPacksR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getLeuronPacksM user_id



getLeuronPackR :: LeuronId -> Handler Value
getLeuronPackR leuron_id = run $ do
  user_id <- _requireAuthId
  toJSON <$> getLeuronPackM user_id leuron_id







--
-- Model
--

getLeuronPacksM :: UserId -> HandlerEff LeuronPackResponses
getLeuronPacksM user_id = do

  sp <- lookupStandardParams

  leurons <- getLeuronsM user_id

  leuron_packs <- mapM (\leuron -> getLeuronPack_ByLeuronM user_id leuron sp) leurons


  return $ LeuronPackResponses {
    leuronPackResponses = leuron_packs
  }



getLeuronPackM :: UserId -> LeuronId -> HandlerEff LeuronPackResponse
getLeuronPackM user_id leuron_id = do

  leuron <- getLeuronM user_id leuron_id

  getLeuronPack_ByLeuronM user_id leuron defaultStandardParams



getLeuronPack_ByLeuronM :: UserId -> Entity Leuron -> StandardParams -> HandlerEff LeuronPackResponse
getLeuronPack_ByLeuronM user_id leuron@(Entity leuron_id Leuron{..}) _ = do

  leuron_user     <- getUserM user_id leuronUserId
  leuron_stat     <- getLeuronStatM user_id leuron_id
--  leuron_like <- getLeuronLike_ByLeuronM user_id leuron
--  leuron_star <- getLeuronStar_ByLeuronM user_id leuron
  leuron_training <- insertLeuronTrainingM user_id leuron_id $ LeuronTrainingRequest LTS_View 0

  return $ LeuronPackResponse {
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
