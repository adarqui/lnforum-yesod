{-# LANGUAGE RecordWildCards #-}

module Handler.Api (
  getApisR,
  postApisR,
  getApiR,
  putApiR,
  deleteApiR
) where



import           Import
import           Model.Api



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
  sendResponseStatus status200 ("DELETED" :: Text)
