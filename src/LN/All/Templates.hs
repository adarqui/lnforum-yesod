{-# LANGUAGE RecordWildCards #-}

module LN.All.Templates (
  getTemplatesR
) where



import           LN.All.Prelude



--
-- Handler
--

getTemplatesR :: Handler Value
getTemplatesR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  let templates = Templates {
    resourceRequest       = defaultResourceRequest,
    leuronRequest         = defaultLeuronRequest,
    leuronTrainingRequest = defaultLeuronTrainingRequest,
    bucketRequest         = defaultBucketRequest
  }
  pure $ toJSON templates
