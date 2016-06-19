module Handler.User (
  getUsersR,
  postUserR0,
  getUserR,
  getUserH,
  putUserR,
  deleteUserR,

  getCountUsersR,

  getUserStatsR,
  getUserStatR,

  getUserPacksR,
  getUserPackR,
  getUserPackH,
) where



import           Handler.Prelude
import           Model.User
import           Model.User.Internal2
import           Model.Pack.Sanitized.User



getUsersR :: Handler Value
getUsersR = do
  user_id <- requireAuthId
  (toJSON . usersToSanitizedResponses) <$> getUsersM user_id



postUserR0 :: Handler Value
postUserR0 = do
  user_id <- requireAuthId
  user_request <- requireJsonBody :: Handler UserRequest
  (toJSON . userToResponse) <$> insertUsersM user_id user_request



getUserR :: UserId -> Handler Value
getUserR lookup_user_id = do
  user_id <- requireAuthId
  response <- getUserM user_id lookup_user_id
  if (user_id == lookup_user_id)
    then
      return $ toJSON $ userToResponse response
    else
      return $ toJSON $ userToSanitizedResponse response



getUserH :: Text -> Handler Value
getUserH _ = notFound



putUserR :: UserId -> Handler Value
putUserR lookup_user_id = do
  user_id <- requireAuthId
  user_request <- requireJsonBody :: Handler UserRequest
  (toJSON . userToResponse) <$> updateUserM user_id lookup_user_id user_request



deleteUserR :: UserId -> Handler Value
deleteUserR lookup_user_id = do

  user_id <- requireAuthId

  void $ deleteUserM user_id lookup_user_id
  pure $ toJSON ()



getCountUsersR :: Handler Value
getCountUsersR = do
  user_id <- requireAuthId
  toJSON <$> countUsersM user_id



getUserStatsR :: Handler Value
getUserStatsR = do
  user_id <- requireAuthId
  toJSON <$> getUserStatsM user_id



getUserStatR :: UserId -> Handler Value
getUserStatR lookup_user_id = do
  user_id <- requireAuthId
  toJSON <$> getUserStatM user_id lookup_user_id



getUserPacksR :: Handler Value
getUserPacksR = do
  user_id <- requireAuthId
  toJSON <$> getUsersSanitizedPacksM user_id



getUserPackR :: UserId -> Handler Value
getUserPackR lookup_user_id = do
  user_id <- requireAuthId
  toJSON <$> getUserSanitizedPackM user_id lookup_user_id



getUserPackH :: Text -> Handler Value
getUserPackH lookup_user_nick = do
  user_id <- requireAuthId
  toJSON <$> getUserSanitizedPackMH user_id lookup_user_nick
