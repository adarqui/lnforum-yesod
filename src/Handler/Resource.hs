module Handler.Resource (
  getResourcesR,
  postResourcesR,
  getResourceR,
  putResourceR,
  deleteResourceR,
) where



import           Handler.Prelude
import           Model.Resource



getResourcesR :: Handler Value
getResourcesR = do
  user_id <- requireAuthId
  (toJSON . resourcesToResponses) <$> getResourcesM user_id



postResourcesR :: Handler Value
postResourcesR = do
  user_id <- requireAuthId
  resource_request <- requireJsonBody :: Handler ResourceRequest
  (toJSON . resourceToResponse) <$> insertResourceM user_id resource_request



getResourceR :: ResourceId -> Handler Value
getResourceR resource_id = do
  user_id <- requireAuthId
  (toJSON . resourceToResponse) <$> getResourceM user_id resource_id



putResourceR :: ResourceId -> Handler Value
putResourceR resource_id = do
  user_id <- requireAuthId
  resource_request <- requireJsonBody
  (toJSON . resourceToResponse) <$> updateResourceM user_id resource_id resource_request



deleteResourceR :: ResourceId -> Handler Value
deleteResourceR resource_id = do
  user_id <- requireAuthId
  void $ deleteResourceM user_id resource_id
  sendResponseStatus status200 ("DELETED" :: Text)
