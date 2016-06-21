{-# LANGUAGE RecordWildCards #-}

module All.Api (
  getApisR,
  postApisR,
  getApiR,
  putApiR,
  deleteApiR,

  -- Model/Function
  apiRequestToApi,
  apiToResponse,
  apisToResponses,

  -- Model/Internal
  getApisM,
  getApiM,
  insertApiM,
  updateApiM,
  deleteApiM
) where



import           All.Prelude
import           Data.UUID    (toText)
import           Data.UUID.V4 (nextRandom)
import           Import



getApisR :: Handler Value
getApisR = do
  user_id <- requireAuthId
  (toJSON . apisToResponses) <$> getApisM user_id



postApisR :: Handler Value
postApisR = do
  user_id <- requireAuthId
  api_request <- requireJsonBody
  (toJSON . apiToResponse) <$> insertApiM user_id api_request



getApiR :: ApiId -> Handler Value
getApiR api_id = do
  user_id <- requireAuthId
  (toJSON . apiToResponse) <$> getApiM user_id api_id



putApiR :: ApiId -> Handler Value
putApiR api_id = do
  user_id <- requireAuthId
  api_request <- requireJsonBody
  (toJSON . apiToResponse) <$> updateApiM user_id api_id api_request



deleteApiR :: ApiId -> Handler Value
deleteApiR api_id = do
  user_id <- requireAuthId
  void $ deleteApiM user_id api_id
  pure $ toJSON ()





--
-- Model/Function
--

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



--
-- Model/Internal
--

getApisM :: UserId -> Handler [Entity Api]
getApisM user_id = do
  selectListDb' [ ApiUserId ==. user_id ] [] ApiId



getApiM :: UserId -> ApiId -> Handler (Entity Api)
getApiM user_id api_id = do
  notFoundMaybe =<< selectFirstDb [ ApiUserId ==. user_id, ApiId ==. api_id ] []



insertApiM :: UserId -> ApiRequest -> Handler (Entity Api)
insertApiM user_id api_request = do
  ts <- timestampH'
  uuid1 <- liftIO nextRandom
  uuid2 <- liftIO nextRandom
  -- TODO: uuid5? namespace to user?
  let api = (apiRequestToApi user_id api_request) { apiKey = (toText uuid1 <> toText uuid2), apiCreatedAt = Just ts }
  insertEntityDb api



updateApiM :: UserId -> ApiId -> ApiRequest -> Handler (Entity Api)
updateApiM user_id api_id api_request = do
  ts <- timestampH'
  updateWhereDb
    [ ApiUserId ==. user_id, ApiId ==. api_id ]
    [ ApiComment =. (apiRequestComment api_request), ApiModifiedAt =. Just ts ]
  notFoundMaybe =<< selectFirstDb [ ApiUserId ==. user_id, ApiId ==. api_id ] []



deleteApiM :: UserId -> ApiId -> Handler ()
deleteApiM user_id api_id = do
  deleteWhereDb [ ApiUserId ==. user_id, ApiId ==. api_id ]
