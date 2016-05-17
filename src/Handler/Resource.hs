module Handler.Resource (
  getResourcesR,
  postResourcesR,
  getResourceR,
  putResourceR,
  deleteResourceR,

  getCountResourcesR,

  getResourceStatsR,
  getResourceStatR,

  getResourcePacksR,
  getResourcePackR
) where



import           Handler.Prelude
import           Model.Resource
import           Model.Pack.Resource



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



getCountResourcesR :: Handler Value
getCountResourcesR = do
  user_id <- requireAuthId
  toJSON <$> countResourcesM user_id



getResourceStatsR :: Handler Value
getResourceStatsR = do
  user_id <- requireAuthId
  toJSON <$> getResourceStatsM user_id



getResourceStatR :: ResourceId -> Handler Value
getResourceStatR thread_post_id = do
  user_id <- requireAuthId
  toJSON <$> getResourceStatM user_id thread_post_id



getResourcePacksR :: Handler Value
getResourcePacksR = do
  user_id <- requireAuthId
  toJSON <$> getResourcePacksM user_id



getResourcePackR :: ResourceId -> Handler Value
getResourcePackR thread_post_id = do
  user_id <- requireAuthId
  toJSON <$> getResourcePackM user_id thread_post_id
