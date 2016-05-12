module Handler.Pm (
  getPmsR,
  postPmsR,
  getPmR,
  putPmR,
  deletePmR
) where



import           Handler.Prelude
import           Model.Pm



getPmsR :: Handler Value
getPmsR = do
  user_id <- requireAuthId
  (toJSON . pmsToResponses) <$> getPmsM user_id



postPmsR :: Handler Value
postPmsR = do
  user_id <- requireAuthId
  sp <- lookupStandardParams
  -- can handle groups, users, user .. if we want
  case (spUserId sp) of
    Nothing -> notFound
    Just to_user_id ->
      if user_id == to_user_id
        then do
          -- can't send a pm to yourself
          permissionDenied "Can't send a PM to yourself"
        else do
          pm_request <- requireJsonBody :: Handler PmRequest
          (toJSON . pmToResponse) <$> insertPmM user_id to_user_id pm_request



getPmR :: PmId -> Handler Value
getPmR pm_id = do
  user_id <- requireAuthId
  (toJSON . pmToResponse) <$> getPmM user_id pm_id



putPmR :: PmId -> Handler Value
putPmR pm_id = do
  user_id <- requireAuthId
  pm_request <- requireJsonBody
  (toJSON . pmToResponse) <$> updatePmM user_id pm_id pm_request



deletePmR :: PmId -> Handler Value
deletePmR pm_id = do
  user_id <- requireAuthId
  void $ deletePmM user_id pm_id
  sendResponseStatus status200 ("DELETED" :: Text)
