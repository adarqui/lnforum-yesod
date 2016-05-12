{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.PmIn.Function (
  pmInRequestToPmIn,
  pmInToResponse,
  pmInsToResponses,
) where



import           Model.Prelude



pmInRequestToPmIn :: UserId -> PmId -> PmInRequest -> PmIn
pmInRequestToPmIn user_id pm_id PmInRequest{..} = PmIn {
  pmInPmId = pm_id,
  pmInUserId = user_id,
  pmInLabel = pmInRequestLabel,
  pmInIsRead = pmInRequestIsRead,
  pmInIsStarred = pmInRequestIsStarred,
  pmInIsNew = True,
  pmInIsSaved = True,
  pmInActive = True,
  pmInCreatedAt = Nothing,
  pmInModifiedAt = Nothing
}



pmInToResponse :: Entity PmIn -> PmInResponse
pmInToResponse (Entity pm_in_id PmIn{..}) = PmInResponse {
  pmInResponseId = keyToInt64 pm_in_id,
  pmInResponsePmId = keyToInt64 pmInPmId,
  pmInResponseUserId = keyToInt64 pmInUserId,
  pmInResponseLabel = pmInLabel,
  pmInResponseIsRead = pmInIsRead,
  pmInResponseIsStarred = pmInIsStarred,
  pmInResponseIsNew = pmInIsNew,
  pmInResponseIsSaved = pmInIsSaved,
  pmInResponseCreatedAt = pmInCreatedAt,
  pmInResponseModifiedAt = pmInModifiedAt
}



pmInsToResponses :: [Entity PmIn] -> PmInResponses
pmInsToResponses pmIns = PmInResponses {
  pmInResponses = map pmInToResponse pmIns
}
