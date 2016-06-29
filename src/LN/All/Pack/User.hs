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

getUserPacksM :: Maybe StandardParams -> UserId -> LN.HandlerErrorEff UserPackResponses
getUserPacksM m_sp user_id = do

  case (lookupSpMay m_sp spUserIds) of

    Just user_ids  -> getUserPacks_ByUserIdsM m_sp user_id user_ids
    _              -> left $ Error_InvalidArguments "user_ids"



getUserPackM :: UserId -> UserId -> LN.HandlerErrorEff UserPackResponse
getUserPackM user_id lookup_user_id = do

  getUserPack_ByUserIdM user_id lookup_user_id



getUserPacks_ByUserIdsM :: Maybe StandardParams -> UserId -> [UserId] -> LN.HandlerErrorEff UserPackResponses
getUserPacks_ByUserIdsM _ user_id user_ids = do
  users_packs <- rights <$> mapM (\key -> getUserPack_ByUserIdM user_id key) user_ids
  right $ UserPackResponses {
    userPackResponses = users_packs
  }




getUserPack_ByUserIdM :: UserId -> UserId -> LN.HandlerErrorEff UserPackResponse
getUserPack_ByUserIdM user_id lookup_user_id = do

  lr <- runEitherT $ do
    lookup_user <- isT $ getUserM user_id lookup_user_id
    stats       <- isT $ getUserStatM user_id lookup_user_id
    profile     <- isT $ getProfile_ByUserIdM user_id lookup_user_id

    pure (lookup_user
         ,stats
         ,profile)


  rehtie lr left $ \(lookup_user, stats, profile) -> do
    right $ UserPackResponse {
      userPackResponseUser       = userToResponse lookup_user,
      userPackResponseUserId     = entityKeyToInt64 lookup_user,
      userPackResponseStat       = stats,
      userPackResponseProfile    = profileToResponse profile,
      userPackResponseProfileId  = entityKeyToInt64 profile
    }
