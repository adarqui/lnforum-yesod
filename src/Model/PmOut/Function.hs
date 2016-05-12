{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.PmOut.Function (
  pmOutRequestToPmOut,
  pmOutToResponse,
  pmOutsToResponses,
) where



import           Model.Prelude



pmOutRequestToPmOut :: UserId -> PmId -> PmOutRequest -> PmOut
pmOutRequestToPmOut user_id pm_id PmOutRequest{..} = PmOut {
  pmOutPmId = pm_id,
  pmOutUserId = user_id,
  pmOutLabel = pmOutRequestLabel,
  pmOutIsSaved = True,
  pmOutActive = True,
  pmOutCreatedAt = Nothing,
  pmOutModifiedAt = Nothing
}



pmOutToResponse :: Entity PmOut -> PmOutResponse
pmOutToResponse (Entity pm_out_id PmOut{..}) = PmOutResponse {
  pmOutResponseId = keyToInt64 pm_out_id,
  pmOutResponsePmId = keyToInt64 pmOutPmId,
  pmOutResponseUserId = keyToInt64 pmOutUserId,
  pmOutResponseLabel = pmOutLabel,
  pmOutResponseIsSaved = pmOutIsSaved,
  pmOutResponseCreatedAt = pmOutCreatedAt,
  pmOutResponseModifiedAt = pmOutModifiedAt
}



pmOutsToResponses :: [Entity PmOut] -> PmOutResponses
pmOutsToResponses pmOuts = PmOutResponses {
  pmOutResponses = map pmOutToResponse pmOuts
}
