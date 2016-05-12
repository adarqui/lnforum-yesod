module Handler.Empty (
  getEmptysR,
  postEmptysR,
  getEmptyR,
  putEmptyR,
  deleteEmptyR
) where



import           Import
import           LN.T.Empty
import           Model.Empty



getEmptysR :: Handler Value
getEmptysR = do
  user_id <- requireAuthId
  (toJSON . emptysToResponses) <$> getEmptysM user_id



postEmptysR :: Handler Value
postEmptysR = do
  user_id <- requireAuthId
  empty_request <- requireJsonBody :: Handler EmptyRequest
  (toJSON . emptyToResponse) <$> insertEmptyM user_id empty_request



getEmptyR :: EmptyId -> Handler Value
getEmptyR empty_id = do
  user_id <- requireAuthId
  (toJSON . emptyToResponse) <$> getEmptyM user_id empty_id



putEmptyR :: EmptyId -> Handler Value
putEmptyR empty_id = do
  user_id <- requireAuthId
  empty_request <- requireJsonBody
  (toJSON . emptyToResponse) <$> updateEmptyM user_id empty_id empty_request



deleteEmptyR :: EmptyId -> Handler Value
deleteEmptyR empty_id = do
  user_id <- requireAuthId
  void $ deleteEmptyM user_id empty_id
  sendResponseStatus status200 ("DELETED" :: Text)
