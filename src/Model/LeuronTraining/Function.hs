{-# LANGUAGE RecordWildCards #-}

module Model.LeuronTraining.Function (
  leuronTrainingRequestToLeuronTraining,
  leuronTrainingToResponse,
  leuronTrainingsToResponses,
) where



import           Model.Prelude



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
