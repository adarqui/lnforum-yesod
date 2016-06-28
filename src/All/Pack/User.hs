{-# LANGUAGE RecordWildCards #-}

module All.Pack.User (
  -- Model
  getUserPacksM,
  getUserPackM,
  getUserPack_ByUserIdM
) where



import           All.Prelude
import           All.Profile
import           All.User



--
-- Model
--

getUserPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff UserPackResponses
getUserPacksM m_sp user_id = do

  case (lookupSpMay m_sp spUserIds) of

    Just user_ids  -> getUserPacks_ByUserIdsM m_sp user_id user_ids
    _              -> left $ Error_InvalidArguments "user_ids"



getUserPackM :: UserId -> UserId -> HandlerEff UserPackResponse
getUserPackM user_id lookup_user_id = do

  getUserPack_ByUserIdM user_id lookup_user_id -- (sp { spLimit = Just 1 })



getUserPacks_ByUserIdsM :: Maybe StandardParams -> UserId -> [UserId] -> HandlerErrorEff UserPackResponses
getUserPacks_ByUserIdsM m_sp user_id user_ids sp = do
  users_packs <- rights <$> mapM (\key -> getUserPack_ByUserIdM user_id key) user_ids
  return $ UserPackResponses {
    userPackResponses = users_packs
  }




getUserPack_ByUserIdM :: UserId -> UserId -> StandardParams -> HandlerEff UserPackResponse
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
