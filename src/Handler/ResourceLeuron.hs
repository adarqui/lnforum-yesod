module Handler.ResourceLeuron (
  getResourceLeuronsR,
  postResourceLeuronsR
) where



import           Handler.Prelude
import           Model.Leuron



getResourceLeuronsR :: ResourceId -> Handler Value
getResourceLeuronsR resource_id = do
  user_id <- requireAuthId
  (toJSON . leuronsToResponses) <$> getLeuronsM user_id (Just resource_id)



postResourceLeuronsR :: ResourceId -> Handler Value
postResourceLeuronsR resource_id = do
  user_id <- requireAuthId
  leuron_request <- requireJsonBody :: Handler LeuronRequest
  (toJSON . leuronToResponse) <$> insertLeuronM user_id resource_id leuron_request
