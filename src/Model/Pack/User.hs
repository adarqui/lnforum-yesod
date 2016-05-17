{-# LANGUAGE RecordWildCards #-}

module Model.Pack.User (
  getUserPacksM,
  getUserPackM,

  getUserPackBy_UserIdM
) where



import           Model.Prelude
import           Model.Profile.Function
import           Model.Profile.Internal
import           Model.User.Function
import           Model.User.Internal2



getUserPacksM :: UserId -> Handler UserPackResponses
getUserPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spUserIds of

    Just user_ids  -> getUserPacksBy_UserIdsM user_id user_ids sp
    _              -> notFound



getUserPackM :: UserId -> UserId -> Handler UserPackResponse
getUserPackM user_id lookup_user_id = do

  sp <- lookupStandardParams

  getUserPackBy_UserIdM user_id lookup_user_id (sp { spLimit = Just 1 })



getUserPacksBy_UserIdsM :: UserId -> [UserId] -> StandardParams -> Handler UserPackResponses
getUserPacksBy_UserIdsM user_id user_ids sp = do
  users_packs <- mapM (\key -> getUserPackBy_UserIdM user_id key sp) user_ids
  return $ UserPackResponses {
    userPackResponses = users_packs
  }




getUserPackBy_UserIdM :: UserId -> UserId -> StandardParams -> Handler UserPackResponse
getUserPackBy_UserIdM user_id lookup_user_id _ = do

  lookup_user <- getUserM user_id lookup_user_id
  stats       <- getUserStatM user_id lookup_user_id
  profile     <- getProfileBy_UserIdM user_id lookup_user_id

  return $ UserPackResponse {
    userPackResponseUser       = userToResponse lookup_user,
    userPackResponseUserId     = entityKeyToInt64 lookup_user,
    userPackResponseStat       = stats,
    userPackResponseProfile    = profileToResponse profile,
    userPackResponseProfileId  = entityKeyToInt64 profile
  }
