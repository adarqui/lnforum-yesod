{-# LANGUAGE RecordWildCards #-}

module Model.Pack.Leuron (
  getLeuronPacksM,
  getLeuronPackM
) where



import           Model.Prelude
import           Model.Leuron.Function
import           Model.Leuron.Internal
import           Model.User.Function
import           Model.User.Internal2



getLeuronPacksM :: UserId -> Handler LeuronPackResponses
getLeuronPacksM user_id = do

  sp <- lookupStandardParams

  leurons <- getLeuronsM user_id

  leuron_packs <- mapM (\leuron -> getLeuronPack_ByLeuronM user_id leuron sp) leurons


  return $ LeuronPackResponses {
    leuronPackResponses = leuron_packs
  }



getLeuronPackM :: UserId -> LeuronId -> Handler LeuronPackResponse
getLeuronPackM user_id leuron_id = do

  leuron <- getLeuronM user_id leuron_id

  getLeuronPack_ByLeuronM user_id leuron defaultStandardParams



getLeuronPack_ByLeuronM :: UserId -> Entity Leuron -> StandardParams -> Handler LeuronPackResponse
getLeuronPack_ByLeuronM user_id leuron@(Entity leuron_id Leuron{..}) _ = do

  leuron_user <- getUserM user_id leuronUserId
  leuron_stat <- getLeuronStatM user_id leuron_id
--  leuron_like <- getLeuronLike_ByLeuronM user_id leuron
--  leuron_star <- getLeuronStar_ByLeuronM user_id leuron

  return $ LeuronPackResponse {
    leuronPackResponseLeuron   = leuronToResponse leuron,
    leuronPackResponseLeuronId = keyToInt64 leuron_id,
    leuronPackResponseUser       = userToSanitizedResponse leuron_user,
    leuronPackResponseUserId     = entityKeyToInt64 leuron_user,
    leuronPackResponseStat       = leuron_stat,
    leuronPackResponseLike       = Nothing,
    leuronPackResponseStar       = Nothing
--    leuronPackResponseLike     = fmap leuronLikeToResponse leuron_like,
--    leuronPackResponseStar     = fmap leuronStarToResponse leuron_star
  }
