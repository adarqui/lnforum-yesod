module Handler.Leuron (
  getLeuronsR,
  postLeuronsR,
  getLeuronR,
  putLeuronR,
  deleteLeuronR
) where



import           Import
import           Api.Params
import           Model.Leuron



getLeuronsR :: Handler Value
getLeuronsR = do
  user_id <- requireAuthId

  sp <- lookupStandardParams

  (toJSON . leuronsToResponses) <$> getLeuronsM user_id (spResourceId sp)



postLeuronsR :: Handler Value
postLeuronsR = do

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
