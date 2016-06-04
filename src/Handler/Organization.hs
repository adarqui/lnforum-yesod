module Handler.Organization (
  getOrganizationsR,
  postOrganizationR0,
  getOrganizationR,
  getOrganizationH,
  putOrganizationR,
  deleteOrganizationR,

  getOrganizationCountR,

  getOrganizationStatsR,
  getOrganizationStatR,

  getOrganizationPacksR,
  getOrganizationPackR,
  getOrganizationPackH,
) where



import           Handler.Prelude
import           Model.Organization
import           Model.Pack.Organization



getOrganizationsR :: Handler Value
getOrganizationsR = do
  user_id <- requireAuthId
  (toJSON . organizationsToResponses) <$> getOrganizationsM user_id



postOrganizationR0 :: Handler Value
postOrganizationR0 = do

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



deleteOrganizationR :: OrganizationId -> Handler Value
deleteOrganizationR organization_id = do
  user_id <- requireAuthId
  void $ deleteOrganizationM user_id organization_id
  sendResponseStatus status200 ("DELETED" :: Text)



getOrganizationCountR :: Handler Value
getOrganizationCountR = do
  user_id <- requireAuthId
  toJSON <$> countOrganizationsM user_id



getOrganizationStatsR :: Handler Value
getOrganizationStatsR = do
  user_id <- requireAuthId
  toJSON <$> getOrganizationStatsM user_id



getOrganizationStatR :: OrganizationId -> Handler Value
getOrganizationStatR organization_id = do
  user_id <- requireAuthId
  toJSON <$> getOrganizationStatM user_id organization_id



getOrganizationPacksR :: Handler Value
getOrganizationPacksR = do
  user_id <- requireAuthId
  toJSON <$> getOrganizationPacksM user_id



getOrganizationPackR :: OrganizationId -> Handler Value
getOrganizationPackR organization_id = do
  user_id <- requireAuthId
  toJSON <$> getOrganizationPackM user_id organization_id



getOrganizationPackH :: Text -> Handler Value
getOrganizationPackH org_name = do
  user_id <- requireAuthId
  toJSON <$> getOrganizationPackMH user_id org_name
