module Model.Api.Internal (
  getApisM,
  getApiM,
  insertApiM,
  updateApiM,
  deleteApiM
) where



import           Model.Prelude
import           Data.UUID           (toText)
import           Data.UUID.V4        (nextRandom)
import           Model.Api.Function



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
