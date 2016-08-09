{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.User (
  -- Model
  getUserPacksM,
  getUserPackM,
  getUserPack_ByUserIdM
) where



import           LN.All.Prelude
import           LN.All.Profile
import           LN.All.User



--
-- Model
--

getUserPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff UserPackResponses
getUserPacksM m_sp user_id = do

  case (lookupSpMay m_sp spUserIds) of

    Just user_ids  -> getUserPacks_ByUserIdsM m_sp user_id user_ids
    _              -> leftA $ Error_InvalidArguments "user_ids"



getUserPackM :: UserId -> UserId -> HandlerErrorEff UserPackResponse
getUserPackM user_id lookup_user_id = do

  getUserPack_ByUserIdM user_id lookup_user_id



getUserPacks_ByUserIdsM :: Maybe StandardParams -> UserId -> [UserId] -> HandlerErrorEff UserPackResponses
getUserPacks_ByUserIdsM _ user_id user_ids = do
  users_packs <- rights <$> mapM (\key -> getUserPack_ByUserIdM user_id key) user_ids
  rightA $ UserPackResponses {
    userPackResponses = users_packs
  }




getUserPack_ByUserIdM :: UserId -> UserId -> HandlerErrorEff UserPackResponse
getUserPack_ByUserIdM user_id lookup_user_id = do

  lr <- runEitherT $ do
    lookup_user <- mustT $ getUserM user_id lookup_user_id
    stats       <- mustT $ getUserStatM user_id lookup_user_id
    profile     <- mustT $ getProfile_ByUserIdM user_id lookup_user_id

    pure (lookup_user
         ,stats
         ,profile)

  rehtie lr leftA $ \(lookup_user, stats, profile) -> do
    rightA $ UserPackResponse {
      userPackResponseUser       = userToResponse lookup_user,
      userPackResponseUserId     = entityKeyToInt64 lookup_user,
      userPackResponseStat       = stats,
      userPackResponseProfile    = profileToResponse profile,
      userPackResponseProfileId  = entityKeyToInt64 profile
    }
