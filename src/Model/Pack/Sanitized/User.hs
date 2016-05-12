{-# LANGUAGE RecordWildCards #-}

module Model.Pack.Sanitized.User (
  getUsersSanitizedPacksM,
  getUserSanitizedPackM,
  getUserSanitizedPackMH,
) where



import           Model.Prelude
import           Model.Profile.Function
import           Model.Profile.Internal
import           Model.User.Function
import           Model.User.Internal2



getUsersSanitizedPacksM :: UserId -> Handler UserSanitizedPackResponses
getUsersSanitizedPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spUserIds of

    Just user_ids  -> getUsersSanitizedPacksBy_UserIdsM user_id user_ids sp
    _              -> getUsersSanitizedPacksBy_EverythingM user_id sp



getUsersSanitizedPacksBy_EverythingM :: UserId -> StandardParams -> Handler UserSanitizedPackResponses
getUsersSanitizedPacksBy_EverythingM user_id sp = do

  users_ids <- selectKeysListDb sp [] [] UserId
  users_packs <- mapM (\key -> getUserSanitizedPackBy_UserIdM user_id key sp) users_ids
  return $ UserSanitizedPackResponses {
    userSanitizedPackResponses = users_packs
  }




getUserSanitizedPackM :: UserId -> UserId -> Handler UserSanitizedPackResponse
getUserSanitizedPackM user_id lookup_user_id = do

  sp <- lookupStandardParams

  getUserSanitizedPackBy_UserIdM user_id lookup_user_id (sp { spLimit = Just 1 })




getUserSanitizedPackMH :: UserId -> Text -> Handler UserSanitizedPackResponse
getUserSanitizedPackMH user_id lookup_user_nick = do

  sp <- lookupStandardParams

  getUserSanitizedPackBy_UserNickM user_id lookup_user_nick (sp { spLimit = Just 1 })



getUsersSanitizedPacksBy_UserIdsM :: UserId -> [UserId] -> StandardParams -> Handler UserSanitizedPackResponses
getUsersSanitizedPacksBy_UserIdsM user_id user_ids sp = do
  users_packs <- mapM (\key -> getUserSanitizedPackBy_UserIdM user_id key sp) user_ids
  return $ UserSanitizedPackResponses {
    userSanitizedPackResponses = users_packs
  }




getUserSanitizedPackBy_UserIdM :: UserId -> UserId -> StandardParams -> Handler UserSanitizedPackResponse
getUserSanitizedPackBy_UserIdM user_id lookup_user_id _ = do

  lookup_user <- getUserM user_id lookup_user_id
  stats <- getUserStatM user_id lookup_user_id
  profile <- getProfileBy_UserIdM user_id lookup_user_id

  return $ UserSanitizedPackResponse {
    userSanitizedPackResponseUser = userToSanitizedResponse lookup_user,
    userSanitizedPackResponseUserStat = stats,
    userSanitizedPackResponseUserProfile = profileToResponse profile
  }




getUserSanitizedPackBy_UserNickM :: UserId -> Text -> StandardParams -> Handler UserSanitizedPackResponse
getUserSanitizedPackBy_UserNickM user_id lookup_user_nick _ = do

  lookup_user@(Entity lookup_user_id _) <- getUserMH user_id lookup_user_nick
  stats <- getUserStatM user_id lookup_user_id
  profile <- getProfileBy_UserIdM user_id lookup_user_id

  return $ UserSanitizedPackResponse {
    userSanitizedPackResponseUser = userToSanitizedResponse lookup_user,
    userSanitizedPackResponseUserStat = stats,
    userSanitizedPackResponseUserProfile = profileToResponse profile
  }
