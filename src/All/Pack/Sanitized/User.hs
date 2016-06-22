{-# LANGUAGE RecordWildCards #-}

module All.Pack.Sanitized.User (
  -- Handler
  getUserSanitizedPacksR,
  getUserSanitizedPackR,
  getUserSanitizedPackH,

  -- Model
  getUsersSanitizedPacksM,
  getUserSanitizedPackM,
  getUserSanitizedPackMH,
) where



import           All.Prelude
import           All.Profile
import           All.User



--
-- Handler
--

getUserSanitizedPacksR :: Handler Value
getUserSanitizedPacksR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getUsersSanitizedPacksM user_id



getUserSanitizedPackR :: UserId -> Handler Value
getUserSanitizedPackR lookup_user_id = run $ do
  user_id <- _requireAuthId
  toJSON <$> getUserSanitizedPackM user_id lookup_user_id



getUserSanitizedPackH :: Text -> Handler Value
getUserSanitizedPackH lookup_user_nick = run $ do
  user_id <- _requireAuthId
  toJSON <$> getUserSanitizedPackMH user_id lookup_user_nick







--
-- Model
--

getUsersSanitizedPacksM :: UserId -> HandlerEff UserSanitizedPackResponses
getUsersSanitizedPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spUserIds of

    Just user_ids  -> getUsersSanitizedPacks_ByUserIdsM user_id user_ids sp
    _              -> getUsersSanitizedPacks_ByEverythingM user_id sp



getUsersSanitizedPacks_ByEverythingM :: UserId -> StandardParams -> HandlerEff UserSanitizedPackResponses
getUsersSanitizedPacks_ByEverythingM user_id sp = do

  users_ids <- selectKeysListDb sp [] [] UserId
  users_packs <- mapM (\key -> getUserSanitizedPack_ByUserIdM user_id key sp) users_ids
  return $ UserSanitizedPackResponses {
    userSanitizedPackResponses = users_packs
  }




getUserSanitizedPackM :: UserId -> UserId -> HandlerEff UserSanitizedPackResponse
getUserSanitizedPackM user_id lookup_user_id = do

  sp <- lookupStandardParams

  getUserSanitizedPack_ByUserIdM user_id lookup_user_id (sp { spLimit = Just 1 })




getUserSanitizedPackMH :: UserId -> Text -> HandlerEff UserSanitizedPackResponse
getUserSanitizedPackMH user_id lookup_user_nick = do

  sp <- lookupStandardParams

  getUserSanitizedPack_ByUserNickM user_id lookup_user_nick (sp { spLimit = Just 1 })



getUsersSanitizedPacks_ByUserIdsM :: UserId -> [UserId] -> StandardParams -> HandlerEff UserSanitizedPackResponses
getUsersSanitizedPacks_ByUserIdsM user_id user_ids sp = do
  users_packs <- mapM (\key -> getUserSanitizedPack_ByUserIdM user_id key sp) user_ids
  return $ UserSanitizedPackResponses {
    userSanitizedPackResponses = users_packs
  }




getUserSanitizedPack_ByUserIdM :: UserId -> UserId -> StandardParams -> HandlerEff UserSanitizedPackResponse
getUserSanitizedPack_ByUserIdM user_id lookup_user_id sp = do

  lookup_user <- getUserM user_id lookup_user_id
  getUserSanitizedPack_ByUserM user_id lookup_user sp




getUserSanitizedPack_ByUserNickM :: UserId -> Text -> StandardParams -> HandlerEff UserSanitizedPackResponse
getUserSanitizedPack_ByUserNickM user_id lookup_user_nick sp = do

  lookup_user <- getUserMH user_id lookup_user_nick
  getUserSanitizedPack_ByUserM user_id lookup_user sp




getUserSanitizedPack_ByUserM :: UserId -> Entity User -> StandardParams -> HandlerEff UserSanitizedPackResponse
getUserSanitizedPack_ByUserM user_id lookup_user _ = do

  stats       <- getUserStatM user_id lookup_user_id
  profile     <- getProfile_ByUserIdM user_id lookup_user_id

  return $ UserSanitizedPackResponse {
    userSanitizedPackResponseUser      = userToSanitizedResponse lookup_user,
    userSanitizedPackResponseUserId    = keyToInt64 lookup_user_id,
    userSanitizedPackResponseStat      = stats,
    userSanitizedPackResponseLike      = Nothing,
    userSanitizedPackResponseStar      = Nothing,
    userSanitizedPackResponseProfile   = profileToResponse profile,
    userSanitizedPackResponseProfileId = entityKeyToInt64 profile
  }

  where
  lookup_user_id  = entityKey lookup_user
