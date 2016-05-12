module Handler.Organization (
  getOrganizationsR,
  postOrganizationsR,
  getOrganizationR,
  getOrganizationH,
  putOrganizationR,
  putOrganizationH,
  deleteOrganizationR,
  deleteOrganizationH,
  getOrganizationCountR,
) where



import           Handler.Prelude
import           Model.Organization



getOrganizationsR :: Handler Value
getOrganizationsR = do
  user_id <- requireAuthId
  (toJSON . organizationsToResponses) <$> getOrganizationsM user_id



postOrganizationsR :: Handler Value
postOrganizationsR = do

  user_id <- requireAuthId

  organization_request <- requireJsonBody :: Handler OrganizationRequest
  (toJSON . organizationToResponse) <$> insertOrganizationM user_id organization_request



getOrganizationR :: OrganizationId -> Handler Value
getOrganizationR org_id = getOrganizationR' getOrganizationM org_id
--  user_id <- requireAuthId
--  (toJSON . organizationToResponse) <$> getOrganizationM user_id organization_id



getOrganizationH :: Text -> Handler Value
getOrganizationH org_name = getOrganizationR' getOrganizationMH org_name
--  user_id <- requireAuthId
--  (toJSON . organizationToResponse) <$> getOrganizationMH user_id org_name



getOrganizationR' :: forall master t.
                     YesodAuth master
                  => (AuthId master -> t -> HandlerT master IO (Entity Organization))
                  -> t
                  -> HandlerT master IO Value
getOrganizationR' f a = do
  user_id <- requireAuthId
  (toJSON . organizationToResponse <$> f user_id a)



putOrganizationR :: OrganizationId -> Handler Value
putOrganizationR organization_id = do
  user_id <- requireAuthId
  organization_request <- requireJsonBody
  (toJSON . organizationToResponse) <$> updateOrganizationM user_id organization_id organization_request



putOrganizationH :: Text -> Handler Value
putOrganizationH _ = do
  sendResponseStatus status200 ("ok." :: Text)



deleteOrganizationR :: OrganizationId -> Handler Value
deleteOrganizationR organization_id = do
  user_id <- requireAuthId
  void $ deleteOrganizationM user_id organization_id
  sendResponseStatus status200 ("DELETED" :: Text)


deleteOrganizationH :: Text -> Handler Value
deleteOrganizationH _ = do
  sendResponseStatus status200 ("ok." :: Text)



getOrganizationCountR :: Handler Value
getOrganizationCountR = do
  user_id <- requireAuthId
  toJSON <$> countOrganizationsM user_id
