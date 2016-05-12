{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Pm.Function (
  pmRequestToPm,
  pmToResponse,
  pmsToResponses,
) where



import           Model.Prelude



pmRequestToPm :: UserId -> UserId -> PmRequest -> Pm
pmRequestToPm user_id to_user_id PmRequest{..} = Pm {
  pmUserId = user_id,
  pmToUserId = to_user_id,
  pmSubject = pmRequestSubject,
  pmBody = pmRequestBody,
  pmActive = True,
  pmCreatedAt = Nothing,
  pmModifiedAt = Nothing
}



pmToResponse :: Entity Pm -> PmResponse
pmToResponse (Entity pm_id Pm{..}) = PmResponse {
  pmResponseId = keyToInt64 pm_id,
  pmResponseUserId = keyToInt64 pmUserId,
  pmResponseToUserId = keyToInt64 pmToUserId,
  pmResponseSubject = pmSubject,
  pmResponseBody = pmBody,
  pmResponseCreatedAt = pmCreatedAt,
  pmResponseModifiedAt = pmModifiedAt
}



pmsToResponses :: [Entity Pm] -> PmResponses
pmsToResponses pms = PmResponses {
  pmResponses = map pmToResponse pms
}
