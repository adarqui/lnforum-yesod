{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Empty.Function (
  emptyRequestToEmpty,
  emptyToResponse,
  emptysToResponses,
) where



import           Import
import           LN.T.Empty
import           Misc.Codec (keyToInt64)



emptyRequestToEmpty :: UserId -> EmptyRequest -> Empty
emptyRequestToEmpty user_id EmptyRequest{..} = Empty {
  emptyUserId = user_id,
  emptyValue  = emptyRequestValue,
  emptyCreatedAt = Nothing,
  emptyModifiedAt = Nothing
}



emptyToResponse :: Entity Empty -> EmptyResponse
emptyToResponse (Entity empty_id Empty{..}) = EmptyResponse {
  emptyResponseId = keyToInt64 empty_id,
  emptyResponseUserId = keyToInt64 emptyUserId,
  emptyResponseValue = emptyValue,
  emptyResponseCreatedAt = emptyCreatedAt,
  emptyResponseModifiedAt = emptyModifiedAt
}



emptysToResponses :: [Entity Empty] -> EmptyResponses
emptysToResponses emptys = EmptyResponses {
  emptyResponses = map emptyToResponse emptys
}
