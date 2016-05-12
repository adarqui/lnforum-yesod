module Handler.PmOut (
  getPmOutsR,
  postPmOutsR,
  getPmOutR,
  putPmOutR,
  deletePmOutR
) where



import           Handler.Prelude
import           Model.PmOut



getPmOutsR :: Handler Value
getPmOutsR = do
  user_id <- requireAuthId
  (toJSON . pmOutsToResponses) <$> getPmOutsM user_id



postPmOutsR :: Handler Value
postPmOutsR = do
  user_id <- requireAuthId
  sp <- lookupStandardParams
  case (spPmId sp) of
    Nothing -> notFound
    Just pm_id -> do
      pmOut_request <- requireJsonBody :: Handler PmOutRequest
      (toJSON . pmOutToResponse) <$> insertPmOutM user_id pm_id pmOut_request



getPmOutR :: PmOutId -> Handler Value
getPmOutR pmOut_id = do
  user_id <- requireAuthId
  (toJSON . pmOutToResponse) <$> getPmOutM user_id pmOut_id



putPmOutR :: PmOutId -> Handler Value
putPmOutR pmOut_id = do
  user_id <- requireAuthId
  pmOut_request <- requireJsonBody
  (toJSON . pmOutToResponse) <$> updatePmOutM user_id pmOut_id pmOut_request



deletePmOutR :: PmOutId -> Handler Value
deletePmOutR pmOut_id = do
  user_id <- requireAuthId
  void $ deletePmOutM user_id pmOut_id
  sendResponseStatus status200 ("DELETED" :: Text)
