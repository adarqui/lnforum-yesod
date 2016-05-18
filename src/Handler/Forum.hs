module Handler.Forum (
  getForumsR,
  postForumsR,
  getForumR,
  getForumH,
  putForumR,
  deleteForumR,

  getCountForumsR,

  getForumStatsR,
  getForumStatR,

  getForumPacksR,
  getForumPackR,
  getForumPackH
) where


import           Handler.Prelude
import           Model.Forum
import           Model.Pack.Forum



getForumsR :: Handler Value
getForumsR = do
  user_id <- requireAuthId
  (toJSON . forumsToResponses) <$> getForumsM user_id



postForumsR :: Handler Value
postForumsR = do

  user_id <- requireAuthId

  sp <- lookupStandardParams

  case (spOrganizationId sp) of
    Nothing -> permissionDenied "Must supply org_id"

    (Just org_id) -> do
      forum_request <- requireJsonBody :: Handler ForumRequest
      (toJSON . forumToResponse) <$> insertForumM user_id org_id forum_request



getForumR :: ForumId -> Handler Value
getForumR forum_id = do
  user_id <- requireAuthId
  (toJSON . forumToResponse) <$> getForumM user_id forum_id



getForumH :: Text -> Handler Value
getForumH forum_name = do -- getForumR' getForumMH forum_name
  user_id <- requireAuthId
  (toJSON . forumToResponse) <$> getForumMH user_id forum_name



putForumR :: ForumId -> Handler Value
putForumR forum_id = do
  user_id <- requireAuthId
  forum_request <- requireJsonBody
  (toJSON . forumToResponse) <$> updateForumM user_id forum_id forum_request



deleteForumR :: ForumId -> Handler Value
deleteForumR forum_id = do
  user_id <- requireAuthId
  void $ deleteForumM user_id forum_id
  sendResponseStatus status200 ("DELETED" :: Text)



getCountForumsR :: Handler Value
getCountForumsR = do
  user_id <- requireAuthId
  toJSON <$> countForumsM user_id



getForumStatsR :: Handler Value
getForumStatsR = do
  user_id <- requireAuthId
  toJSON <$> getForumStatsM user_id



getForumStatR :: ForumId -> Handler Value
getForumStatR forum_id = do
  user_id <- requireAuthId
  toJSON <$> getForumStatM user_id forum_id



getForumPacksR :: Handler Value
getForumPacksR = do
  user_id <- requireAuthId
  toJSON <$> getForumPacksM user_id



getForumPackR :: ForumId -> Handler Value
getForumPackR forum_id = do
  user_id <- requireAuthId
  toJSON <$> getForumPackM user_id forum_id



getForumPackH :: Text -> Handler Value
getForumPackH forum_name = do
  user_id <- requireAuthId
  toJSON <$> getForumPackMH user_id forum_name
