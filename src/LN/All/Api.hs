{-# LANGUAGE RecordWildCards #-}

module LN.All.Api (
  getApisR,
  postApisR,
  getApiR,
  putApiR,
  deleteApiR,

  -- LN.Model/Function
  apiRequestToApi,
  apiToResponse,
  apisToResponses,

  -- LN.Model/Internal
  getApisM,
  getApiM,
  insertApiM,
  updateApiM,
  deleteApiM
) where



import           LN.All.Prelude
import           Data.UUID    (toText)
import           Data.UUID.V4 (nextRandom)



getApisR :: Handler Value
getApisR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON apisToResponses $ getApisM (pure sp) user_id



postApisR :: Handler Value
postApisR = run $ do
  user_id     <- _requireAuthId
  api_request <- requireJsonBody
  errorOrJSON apiToResponse $ insertApiM user_id api_request



getApiR :: ApiId -> Handler Value
getApiR api_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON apiToResponse $ getApiM user_id api_id



putApiR :: ApiId -> Handler Value
putApiR api_id = run $ do
  user_id     <- _requireAuthId
  api_request <- requireJsonBody
  errorOrJSON apiToResponse $ updateApiM user_id api_id api_request



deleteApiR :: ApiId -> Handler Value
deleteApiR api_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteApiM user_id api_id





--
-- LN.Model/Function
--

apiRequestToApi :: UserId -> LN.ApiRequest -> LN.Api
apiRequestToApi user_id LN.ApiRequest{..} = LN.Api {
  apiUserId     = user_id,
  apiComment    = apiRequestComment,
  apiKey        = "",
  apiActive     = True,
  apiGuard      = apiRequestGuard,
  apiCreatedAt  = Nothing,
  apiModifiedAt = Nothing
}



apiToResponse :: Entity LN.Api -> LN.ApiResponse
apiToResponse (Entity api_id LN.Api{..}) = LN.ApiResponse {
  apiResponseId         = keyToInt64 api_id,
  apiResponseUserId     = keyToInt64 apiUserId,
  apiResponseComment    = apiComment,
  apiResponseKey        = apiKey,
  apiResponseGuard      = apiGuard,
  apiResponseCreatedAt  = apiCreatedAt,
  apiResponseModifiedAt = apiModifiedAt
}



apisToResponses :: [Entity LN.Api] -> LN.ApiResponses
apisToResponses apis = LN.ApiResponses {
  apiResponses = map apiToResponse apis
}



--
-- LN.Model/Internal
--

getApisM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity LN.Api]
getApisM m_sp user_id = do
  selectListDbE m_sp [ApiUserId ==. user_id, LN.ApiActive ==. True] [] ApiId



getApiM :: UserId -> ApiId -> HandlerErrorEff (Entity LN.Api)
getApiM user_id api_id = do
  selectFirstDbE [ApiUserId ==. user_id, ApiId ==. api_id, LN.ApiActive ==. True] []



insertApiM :: UserId -> LN.ApiRequest -> HandlerErrorEff (Entity LN.Api)
insertApiM user_id api_request = do
  ts <- timestampH'
  uuid1 <- liftIO nextRandom
  uuid2 <- liftIO nextRandom
  -- TODO: uuid5? namespace to user?
  let api = (apiRequestToApi user_id api_request) { apiKey = (toText uuid1 <> toText uuid2), apiCreatedAt = Just ts }
  insertEntityDbE api



updateApiM :: UserId -> ApiId -> LN.ApiRequest -> HandlerErrorEff (Entity LN.Api)
updateApiM user_id api_id api_request = do
  ts <- timestampH'
  updateWhereDb
    [ LN.ApiUserId ==. user_id, ApiId ==. api_id, LN.ApiActive ==. True ]
    [ LN.ApiComment =. (apiRequestComment api_request), LN.ApiModifiedAt =. Just ts ]
  selectFirstDbE [ApiUserId ==. user_id, ApiId ==. api_id, LN.ApiActive ==. True] []



deleteApiM :: UserId -> ApiId -> HandlerErrorEff ()
deleteApiM user_id api_id = do
  deleteWhereDbE [ApiUserId ==. user_id, ApiId ==. api_id, LN.ApiActive ==. True]
