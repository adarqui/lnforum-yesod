module Handler.PmIn (
  getPmInsR,
  postPmInsR,
  getPmInR,
  putPmInR,
  deletePmInR
) where



import           Handler.Prelude
import           Model.PmIn



getPmInsR :: Handler Value
getPmInsR = do
  user_id <- requireAuthId
  (toJSON . pmInsToResponses) <$> getPmInsM user_id



postPmInsR :: Handler Value
postPmInsR = do
  user_id <- requireAuthId
  sp <- lookupStandardParams
  case (spPmId sp) of
    Nothing -> notFound
    Just pm_id -> do
      pmIn_request <- requireJsonBody :: Handler PmInRequest
      (toJSON . pmInToResponse) <$> insertPmInM user_id pm_id pmIn_request



getPmInR :: PmInId -> Handler Value
getPmInR pmIn_id = do
  user_id <- requireAuthId
  (toJSON . pmInToResponse) <$> getPmInM user_id pmIn_id



putPmInR :: PmInId -> Handler Value
putPmInR pmIn_id = do
  user_id <- requireAuthId
  pmIn_request <- requireJsonBody
  (toJSON . pmInToResponse) <$> updatePmInM user_id pmIn_id pmIn_request



deletePmInR :: PmInId -> Handler Value
deletePmInR pmIn_id = do
  user_id <- requireAuthId
  void $ deletePmInM user_id pmIn_id
  pure $ toJSON ()
