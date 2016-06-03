{-# LANGUAGE RecordWildCards #-}

module Model.Leuron.Function (
  leuronRequestToLeuron,
  leuronToResponse,
  leuronsToResponses,
) where



import           Model.Prelude



leuronRequestToLeuron :: UserId -> ResourceId -> LeuronRequest -> Leuron
leuronRequestToLeuron user_id resource_id LeuronRequest{..} = Leuron {
  leuronUserId        = user_id,
  leuronResourceId    = resource_id,
  leuronData          = encodeText leuronRequestData,
  leuronTitle         = leuronRequestTitle,
  leuronDescription   = leuronRequestDescription,
  leuronSection       = leuronRequestSection,
  leuronPage          = leuronRequestPage,
  leuronExamples      = encodeTextMaybe leuronRequestExamples,
  leuronStrengths     = encodeTextMaybe leuronRequestStrengths,
  leuronCategories    = if null leuronRequestCategories then Nothing else Just $ encodeText leuronRequestCategories,
  leuronSplits        = encodeTextMaybe leuronRequestSplits,
  leuronSubstitutions = encodeTextMaybe leuronRequestSubstitutions,
  leuronTags          = leuronRequestTags,
  leuronStyle         = encodeTextMaybe leuronRequestStyle,
  leuronActive        = True,
  leuronCreatedAt     = Nothing,
  leuronModifiedAt    = Nothing
}



leuronToResponse :: Entity Leuron -> LeuronResponse
leuronToResponse (Entity leuron_id Leuron{..}) = LeuronResponse {
  leuronResponseId            = keyToInt64 leuron_id,
  leuronResponseUserId        = keyToInt64 leuronUserId,
  leuronResponseResourceId    = keyToInt64 leuronResourceId,
  leuronResponseData          = maybe LnEmpty id (decodeText leuronData),
  leuronResponseTitle         = leuronTitle,
  leuronResponseDescription   = leuronDescription,
  leuronResponseSection       = leuronSection,
  leuronResponsePage          = leuronPage,
  leuronResponseExamples      = maybe Nothing decodeText leuronExamples,
  leuronResponseStrengths     = maybe Nothing decodeText leuronStrengths,
  leuronResponseCategories    = maybe [] (\s -> maybe [] id (decodeText s)) leuronCategories,
  leuronResponseSplits        = maybe Nothing decodeText leuronSplits,
  leuronResponseSubstitutions = maybe Nothing decodeText leuronSubstitutions,
  leuronResponseTags          = leuronTags,
  leuronResponseStyle         = maybe Nothing decodeText leuronStyle,
  leuronResponseCreatedAt     = leuronCreatedAt,
  leuronResponseModifiedAt    = leuronModifiedAt
}



leuronsToResponses :: [Entity Leuron] -> LeuronResponses
leuronsToResponses leurons = LeuronResponses {
  leuronResponses = map leuronToResponse leurons
}
