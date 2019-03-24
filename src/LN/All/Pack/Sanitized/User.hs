{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.Sanitized.User (
  -- Handler
  getUserSanitizedPacksR,
  getUserSanitizedPackR,
  getUserSanitizedPackH,

  -- Model
  getUsersSanitizedPacksM,
  getUserSanitizedPackM,
  getUserSanitizedPackMH,
) where



import           LN.All.Internal
import           LN.All.Prelude
import           LN.All.Profile
import           LN.All.User



--
-- Handler
--

getUserSanitizedPacksR :: Handler Value
getUserSanitizedPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getUsersSanitizedPacksM (pure sp) user_id



getUserSanitizedPackR :: UserId -> Handler Value
getUserSanitizedPackR lookup_user_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getUserSanitizedPackM user_id lookup_user_id



getUserSanitizedPackH :: Text -> Handler Value
getUserSanitizedPackH lookup_user_name = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getUserSanitizedPackMH user_id lookup_user_name







--
-- Model
--

getUsersSanitizedPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff UserSanitizedPackResponses
getUsersSanitizedPacksM m_sp user_id = do

  case (lookupSpMay m_sp spUserIds) of

    Just user_ids  -> getUsersSanitizedPacks_ByUserIdsM m_sp user_id user_ids
    _              -> getUsersSanitizedPacks_ByEverythingM m_sp user_id



getUsersSanitizedPacks_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff UserSanitizedPackResponses
getUsersSanitizedPacks_ByEverythingM m_sp user_id = do

  e_users <- getUsers_ByEverythingM m_sp user_id
  rehtie e_users leftA $ \users -> do
    user_packs <- rights <$> forConcurrently users (getUserSanitizedPack_ByUserM user_id)
    rightA $ UserSanitizedPackResponses {
      userSanitizedPackResponses = user_packs
    }



getUserSanitizedPackM :: UserId -> UserId -> HandlerErrorEff UserSanitizedPackResponse
getUserSanitizedPackM user_id lookup_user_id = getUserSanitizedPack_ByUserIdM user_id lookup_user_id




getUserSanitizedPackMH :: UserId -> Text -> HandlerErrorEff UserSanitizedPackResponse
getUserSanitizedPackMH user_id lookup_user_name = getUserSanitizedPack_ByUserNameM user_id lookup_user_name



getUsersSanitizedPacks_ByUserIdsM :: Maybe StandardParams -> UserId -> [UserId] -> HandlerErrorEff UserSanitizedPackResponses
getUsersSanitizedPacks_ByUserIdsM _ user_id user_ids = do
  e_users <- getUsers_ByUserIdsM Nothing user_id user_ids
  rehtie e_users leftA $ \users -> do
    user_packs <- rights <$> forConcurrently users (getUserSanitizedPack_ByUserM user_id)
    rightA $ UserSanitizedPackResponses {
      userSanitizedPackResponses = user_packs
    }



getUserSanitizedPack_ByUserIdM :: UserId -> UserId -> HandlerErrorEff UserSanitizedPackResponse
getUserSanitizedPack_ByUserIdM user_id lookup_user_id = do

  e_lookup_user <- getUserM user_id lookup_user_id
  rehtie e_lookup_user leftA $ getUserSanitizedPack_ByUserM user_id




getUserSanitizedPack_ByUserNameM :: UserId -> Text -> HandlerErrorEff UserSanitizedPackResponse
getUserSanitizedPack_ByUserNameM user_id lookup_user_name = do

  e_lookup_user <- getUserMH user_id lookup_user_name
  rehtie e_lookup_user leftA $ getUserSanitizedPack_ByUserM user_id




getUserSanitizedPack_ByUserM :: UserId -> Entity User -> HandlerErrorEff UserSanitizedPackResponse
getUserSanitizedPack_ByUserM user_id lookup_user = do

  lr <- runEitherT $ do
    stats   <- mustT $ getUserStatM user_id lookup_user_id
    profile <- mustT $ getProfile_ByUserIdM user_id lookup_user_id
    pure (stats, profile)

  rehtie lr leftA $ \(stats, profile) -> do

    rightA $ UserSanitizedPackResponse {
      userSanitizedPackResponseUser      = userToSanitizedResponse lookup_user,
      userSanitizedPackResponseUserId    = keyToInt64 lookup_user_id,
      userSanitizedPackResponseStat      = stats,
      userSanitizedPackResponseLike      = Nothing,
      userSanitizedPackResponseProfile   = profileToResponse profile,
      userSanitizedPackResponseProfileId = entityKeyToInt64 profile
    }

  where
  lookup_user_id  = entityKey lookup_user
