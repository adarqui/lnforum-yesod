{-# LANGUAGE RecordWildCards #-}

module LN.All.LeuronTraining (

  -- Handler
  getLeuronTrainingsR,
  postLeuronTrainingsR,
  getLeuronTrainingR,
  getLeuronTrainingCountR,

  -- Model/Function
  leuronTrainingRequestToLeuronTraining,
  leuronTrainingToResponse,
  leuronTrainingsToResponses,

  -- Model/Internal
  insertLeuronTrainingM,
) where



import           LN.All.Prelude



--
-- Handler
--

getLeuronTrainingsR :: Handler Value
getLeuronTrainingsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON leuronTrainingsToResponses $ getLeuronTrainingsM (pure sp) user_id



postLeuronTrainingsR :: Handler Value
postLeuronTrainingsR = run $ do
  user_id                 <- _requireAuthId
  sp                      <- lookupStandardParams
  leuron_training_request <- requireJsonBody
  case (spLeuronId sp) of
    Just leuron_id -> errorOrJSON leuronTrainingToResponse $ insertLeuronTrainingM user_id leuron_id leuron_training_request
    _              -> errorOrJSON id $ (leftA $ Error_InvalidArguments "leuron_id" :: HandlerErrorEff ())



getLeuronTrainingR :: LeuronTrainingId -> Handler Value
getLeuronTrainingR leuron_training_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON leuronTrainingToResponse $ getLeuronTrainingM user_id leuron_training_id



getLeuronTrainingCountR :: Handler Value
getLeuronTrainingCountR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ countLeuronTrainingM (pure sp) user_id




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

getLeuronTrainingsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity LeuronTraining]
getLeuronTrainingsM m_sp user_id = do
  case (lookupSpMay m_sp spUserId, lookupSpMay m_sp spResourceId) of
    (Just lookup_user_id, _) -> getLeuronTrainings_ByUserIdM m_sp user_id lookup_user_id
    {- TODO FIXME: (_, Just resource_id)    -> getLeuronTrainings_ByResourceId m_sp user_id resource_id -}
    _                        -> getLeuronTrainingsM_ByEverythingM m_sp



getLeuronTrainingsM_ByEverythingM :: Maybe StandardParams -> HandlerErrorEff [Entity LeuronTraining]
getLeuronTrainingsM_ByEverythingM m_sp = do
  selectListDbE m_sp [LeuronTrainingActive ==. True] [] LeuronTrainingId



getLeuronTrainings_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity LeuronTraining]
getLeuronTrainings_ByUserIdM m_sp _ lookup_user_id = do
  selectListDbE m_sp [LeuronTrainingUserId ==. lookup_user_id, LeuronTrainingActive ==. True] [] LeuronTrainingId



{-
-- TODO FIXME: LeuronTraining needs a ResourceId
getLeuronTrainings_ByResourceIdM :: Maybe StandardParams -> UserId -> ResourceId -> HandlerErrorEff [Entity LeuronTraining]
getLeuronTrainings_ByResourceIdM m_sp _ resource_id = do
  selectListDbE m_sp [LeuronTrainingUserId ==. resource_id, LeuronTrainingActive ==. True] [] LeuronTrainingId
-}



insertLeuronTrainingM :: UserId -> LeuronId -> LeuronTrainingRequest -> HandlerErrorEff (Entity LeuronTraining)
insertLeuronTrainingM user_id leuron_id leuron_training_request = do

  ts <- timestampH'

  let
    leuron_training = (leuronTrainingRequestToLeuronTraining user_id leuron_id leuron_training_request) { leuronTrainingCreatedAt = Just ts }

  insertEntityDbE leuron_training



getLeuronTrainingM :: UserId -> LeuronTrainingId -> HandlerErrorEff (Entity LeuronTraining)
getLeuronTrainingM _ leuron_training_id = do
  selectFirstDbE [LeuronTrainingId ==. leuron_training_id, LeuronTrainingActive ==. True] []



countLeuronTrainingM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countLeuronTrainingM m_sp _ = do

  case (lookupSpMay m_sp spUserId, lookupSpMay m_sp spResourceId) of

    -- TODO FIXME: not handling argument properly
    _ -> do
      n <- countDb [LeuronTrainingActive ==. True]
      rightA $ CountResponses [CountResponse 0 (fromIntegral n)]
