{-# LANGUAGE RecordWildCards #-}

module Model.Api.Function (
  apiRequestToApi,
  apiToResponse,
  apisToResponses,
) where



import           Model.Prelude



apiRequestToApi :: UserId -> ApiRequest -> Api
apiRequestToApi user_id ApiRequest{..} = Api {
  apiUserId     = user_id,
  apiComment    = apiRequestComment,
  apiKey        = "",
  apiActive     = True,
  apiGuard      = apiRequestGuard,
  apiCreatedAt  = Nothing,
  apiModifiedAt = Nothing
}



apiToResponse :: Entity Api -> ApiResponse
apiToResponse (Entity api_id Api{..}) = ApiResponse {
  apiResponseId         = keyToInt64 api_id,
  apiResponseUserId     = keyToInt64 apiUserId,
  apiResponseComment    = apiComment,
  apiResponseKey        = apiKey,
  apiResponseGuard      = apiGuard,
  apiResponseCreatedAt  = apiCreatedAt,
  apiResponseModifiedAt = apiModifiedAt
}



apisToResponses :: [Entity Api] -> ApiResponses
apisToResponses apis = ApiResponses {
  apiResponses = map apiToResponse apis
}
