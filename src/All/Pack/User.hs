{-# LANGUAGE RecordWildCards #-}

module All.Pack.User (
  -- Model
  getUserPacksM,
  getUserPackM,
  getUserPack_ByUserIdM
) where



import           All.Prelude
import           All.Profile
import           Model.User.Function
import           Model.User.Internal2



--
-- Model
--

getUserPacksM :: UserId -> Handler UserPackResponses
getUserPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spUserIds of

    Just user_ids  -> getUserPacks_ByUserIdsM user_id user_ids sp
    _              -> notFound



getUserPackM :: UserId -> UserId -> Handler UserPackResponse
getUserPackM user_id lookup_user_id = do

  sp <- lookupStandardParams

  getUserPack_ByUserIdM user_id lookup_user_id (sp { spLimit = Just 1 })



getUserPacks_ByUserIdsM :: UserId -> [UserId] -> StandardParams -> Handler UserPackResponses
getUserPacks_ByUserIdsM user_id user_ids sp = do
  users_packs <- mapM (\key -> getUserPack_ByUserIdM user_id key sp) user_ids
  return $ UserPackResponses {
    userPackResponses = users_packs
  }




getUserPack_ByUserIdM :: UserId -> UserId -> StandardParams -> Handler UserPackResponse
getUserPack_ByUserIdM user_id lookup_user_id _ = do

  lookup_user <- getUserM user_id lookup_user_id
  stats       <- getUserStatM user_id lookup_user_id
  profile     <- getProfile_ByUserIdM user_id lookup_user_id

  return $ UserPackResponse {
    userPackResponseUser       = userToResponse lookup_user,
    userPackResponseUserId     = entityKeyToInt64 lookup_user,
    userPackResponseStat       = stats,
    userPackResponseProfile    = profileToResponse profile,
    userPackResponseProfileId  = entityKeyToInt64 profile
  }
