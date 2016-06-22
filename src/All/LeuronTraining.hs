{-# LANGUAGE RecordWildCards #-}

module All.LeuronTraining (
  -- Model/Function
  leuronTrainingRequestToLeuronTraining,
  leuronTrainingToResponse,
  leuronTrainingsToResponses,

  -- Model/Internal
  insertLeuronTrainingM,
) where



import           All.Leuron
import           All.Prelude
import           Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import qualified Database.Redis     as R
import qualified LN.T.Like          as L



--
-- Model/Function
--

leuronTrainingRequestToLeuronTraining :: UserId -> LeuronId -> LeuronTrainingRequest -> LeuronTraining
leuronTrainingRequestToLeuronTraining user_id leuron_id LeuronTrainingRequest{..} = LeuronTraining {
  leuronTrainingUserId        = user_id,
  leuronTrainingLeuronId      = leuron_id,
  leuronTrainingSummary       = leuronTrainingRequestSummary,
  leuronTrainingActive        = True,
  leuronTrainingGuard         = leuronTrainingRequestGuard,
  leuronTrainingCreatedAt     = Nothing,
  leuronTrainingModifiedAt    = Nothing
}



leuronTrainingToResponse :: Entity LeuronTraining -> LeuronTrainingResponse
leuronTrainingToResponse (Entity leuronTraining_id LeuronTraining{..}) = LeuronTrainingResponse {
  leuronTrainingResponseId            = keyToInt64 leuronTraining_id,
  leuronTrainingResponseUserId        = keyToInt64 leuronTrainingUserId,
  leuronTrainingResponseLeuronId      = keyToInt64 leuronTrainingLeuronId,
  leuronTrainingResponseSummary       = leuronTrainingSummary,
  leuronTrainingResponseGuard         = leuronTrainingGuard,
  leuronTrainingResponseCreatedAt     = leuronTrainingCreatedAt,
  leuronTrainingResponseModifiedAt    = leuronTrainingModifiedAt
}



leuronTrainingsToResponses :: [Entity LeuronTraining] -> LeuronTrainingResponses
leuronTrainingsToResponses leuronTrainings = LeuronTrainingResponses {
  leuronTrainingResponses = map leuronTrainingToResponse leuronTrainings
}








--
-- Model/Internal
--

insertLeuronTrainingM :: UserId -> LeuronId -> LeuronTrainingRequest -> HandlerEff (Entity LeuronTraining)
insertLeuronTrainingM user_id leuron_id leuron_training_request = do

  ts <- timestampH'

  let
    leuron_training = (leuronTrainingRequestToLeuronTraining user_id leuron_id leuron_training_request) { leuronTrainingCreatedAt = Just ts }

--  void $ notFoundMaybe =<< selectFirstDb [ LUserId ==. user_id, ResourceId ==. resource_id ] []

  insertEntityDb leuron_training
