{-# LANGUAGE RecordWildCards #-}

module Model.User.Function (
  profileNameToNick,
  userRequestToUser,
  userToResponse,
  usersToResponses,
  userToSanitizedResponse,
  usersToSanitizedResponses,
  validateUserRequest
) where



import           Data.Char           (isAlphaNum)
import qualified Data.Text           as T (filter, toLower)
import           Import.NoFoundation
import           LN.T
import           Misc.Codec          (keyToInt64)
import           LN.Lib.Validate



profileNameToNick :: Text -> Text
profileNameToNick = T.toLower . T.filter isAlphaNum



userRequestToUser :: UserRequest -> User
userRequestToUser UserRequest{..} = User {
  userNick = userRequestNick,
  userDisplayNick = userRequestDisplayNick,
  userName = userRequestName,
  userEmail = userRequestEmail,
  userEmailMD5 = "md5",
  userPlugin = userRequestPlugin,
  userIdent = userRequestIdent,
  userActive = False,
  userCreatedAt = Nothing,
  userModifiedAt = Nothing
}



userToResponse :: Entity User -> UserResponse
userToResponse (Entity user_id User{..}) = UserResponse {
  userResponseId = keyToInt64 user_id,
  userResponseNick = userNick,
  userResponseDisplayNick = userDisplayNick,
  userResponseName = userName,
  userResponseEmail = userEmail,
  userResponseEmailMD5 = userEmailMD5,
  userResponsePlugin = userPlugin,
  userResponseIdent = userIdent,
  userResponseActive = userActive,
  userResponseCreatedAt = userCreatedAt,
  userResponseModifiedAt = userModifiedAt,
  userResponseDeactivatedAt = Nothing
}



usersToResponses :: [Entity User] -> UserResponses
usersToResponses users = UserResponses {
  userResponses = map userToResponse users
}



userToSanitizedResponse :: Entity User -> UserSanitizedResponse
userToSanitizedResponse (Entity user_id User{..}) = UserSanitizedResponse {
  userSanitizedResponseId = keyToInt64 user_id,
  userSanitizedResponseNick = userNick,
  userSanitizedResponseDisplayNick = userDisplayNick,
  userSanitizedResponseEmailMD5 = userEmailMD5,
  userSanitizedResponseActive = userActive,
  userSanitizedResponseCreatedAt = userCreatedAt
}



usersToSanitizedResponses :: [Entity User] -> UserSanitizedResponses
usersToSanitizedResponses users = UserSanitizedResponses {
  userSanitizedResponses = map userToSanitizedResponse users
}



validateUserRequest :: UserRequest -> Either Text UserRequest
validateUserRequest z@UserRequest{..} = do
  _ <- isValidNick userRequestNick
  _ <- isValidName userRequestDisplayNick
  _ <- isValidName userRequestName
  _ <- isValidEmail userRequestEmail
  Right z
