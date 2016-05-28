module Handler.Leuron (
  getLeuronsR,
  postLeuronR0,
  getLeuronR,
  putLeuronR,
  deleteLeuronR,

  getCountLeuronsR,

  getLeuronStatsR,
  getLeuronStatR,

  getLeuronPacksR,
  getLeuronPackR,
) where



import           Import
import           Api.Params
import           Model.Leuron
import           Model.Pack.Leuron



getLeuronsR :: Handler Value
getLeuronsR = do

  user_id <- requireAuthId

  (toJSON . leuronsToResponses) <$> getLeuronsM user_id



postLeuronR0 :: Handler Value
postLeuronR0 = do

  user_id <- requireAuthId

  sp <- lookupStandardParams

  case (spResourceId sp) of

    Nothing -> notFound

    Just resource_id -> do
      leuron_request <- requireJsonBody
      (toJSON . leuronToResponse) <$> insertLeuronM user_id resource_id leuron_request




getLeuronR :: LeuronId -> Handler Value
getLeuronR leuron_id = do
  user_id <- requireAuthId
  (toJSON . leuronToResponse) <$> getLeuronM user_id leuron_id



putLeuronR :: LeuronId -> Handler Value
putLeuronR leuron_id = do
  user_id <- requireAuthId
  leuron_request <- requireJsonBody
  (toJSON . leuronToResponse) <$> updateLeuronM user_id leuron_id leuron_request



deleteLeuronR :: LeuronId -> Handler Value
deleteLeuronR leuron_id = do
  user_id <- requireAuthId
  void $ deleteLeuronM user_id leuron_id
  sendResponseStatus status200 ("DELETED" :: Text)



getCountLeuronsR :: Handler Value
getCountLeuronsR = do
  user_id <- requireAuthId
  toJSON <$> countLeuronsM user_id



getLeuronStatsR :: Handler Value
getLeuronStatsR = do
  user_id <- requireAuthId
  toJSON <$> getLeuronStatsM user_id



getLeuronStatR :: LeuronId -> Handler Value
getLeuronStatR leuron_id = do
  user_id <- requireAuthId
  toJSON <$> getLeuronStatM user_id leuron_id



getLeuronPacksR :: Handler Value
getLeuronPacksR = do
  user_id <- requireAuthId
  toJSON <$> getLeuronPacksM user_id



getLeuronPackR :: LeuronId -> Handler Value
getLeuronPackR leuron_id = do
  user_id <- requireAuthId
  toJSON <$> getLeuronPackM user_id leuron_id
