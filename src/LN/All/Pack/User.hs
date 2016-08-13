{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.User (
  -- Handler
  getUserPacksR,
  getUserPackR,
  getUserPackH,
  -- Model
  getUserPacksM,
  getUserPackM,
  getUserPack_ByUserIdM
) where



import           LN.All.Prelude
import           LN.All.Profile
import           LN.All.User



--
-- Handler
--

getUserPacksR :: Handler Value
getUserPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getUserPacksM (pure sp) user_id



getUserPackR :: UserId -> Handler Value
getUserPackR lookup_user_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getUserPackM user_id lookup_user_id



getUserPackH :: Text -> Handler Value
getUserPackH lookup_user_name = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getUserPack_ByUserNameM user_id lookup_user_name





--
-- Model
--

-- | TODO ACCESS: Ensure only super users can make these queries
--
getUserPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff UserPackResponses
getUserPacksM m_sp user_id = do

  case (lookupSpMay m_sp spUserIds, lookupSpMay m_sp spEmail) of

    (Just user_ids, _)    -> getUserPacks_ByUserIdsM m_sp user_id user_ids
    (Nothing, Just email) -> getUserPacks_ByEmailM m_sp user_id email
    _                     -> leftA $ Error_InvalidArguments "user_ids"



getUserPackM :: UserId -> UserId -> HandlerErrorEff UserPackResponse
getUserPackM user_id lookup_user_id = do

  getUserPack_ByUserIdM user_id lookup_user_id



getUserPacks_ByUserIdsM :: Maybe StandardParams -> UserId -> [UserId] -> HandlerErrorEff UserPackResponses
getUserPacks_ByUserIdsM _ user_id user_ids = do
  users_packs <- rights <$> mapM (\key -> getUserPack_ByUserIdM user_id key) user_ids
  rightA $ UserPackResponses {
    userPackResponses = users_packs
  }



getUserPacks_ByEmailM :: Maybe StandardParams -> UserId -> Text -> HandlerErrorEff UserPackResponses
getUserPacks_ByEmailM m_sp user_id email = do
  lr <- selectFirstDbE [UserEmail ==. email] []
  rehtie lr leftA $ \user -> do
    lr' <- getUserPack_ByUserM m_sp user_id user
    rehtie lr' leftA $ \user_pack -> do
      rightA $ UserPackResponses {
        userPackResponses = [user_pack]
      }



getUserPack_ByUserNameM :: UserId -> Text -> HandlerErrorEff UserPackResponse
getUserPack_ByUserNameM user_id lookup_user_name = do

  e_lookup_user <- getUserMH user_id lookup_user_name
  rehtie e_lookup_user leftA $ getUserPack_ByUserM Nothing user_id



getUserPack_ByUserIdM :: UserId -> UserId -> HandlerErrorEff UserPackResponse
getUserPack_ByUserIdM user_id lookup_user_id = do

  lr <- getUserM user_id lookup_user_id
  rehtie lr leftA $ getUserPack_ByUserM Nothing user_id



getUserPack_ByUserM :: Maybe StandardParams -> UserId -> Entity User -> HandlerErrorEff UserPackResponse
getUserPack_ByUserM _ user_id lookup_user@(Entity lookup_user_id _) = do

  lr <- runEitherT $ do
    stats       <- mustT $ getUserStatM user_id lookup_user_id
    profile     <- mustT $ getProfile_ByUserIdM user_id lookup_user_id

    pure (stats
         ,profile)

  rehtie lr leftA $ \(stats, profile) -> do
    rightA $ UserPackResponse {
      userPackResponseUser       = userToResponse lookup_user,
      userPackResponseUserId     = entityKeyToInt64 lookup_user,
      userPackResponseStat       = stats,
      userPackResponseProfile    = profileToResponse profile,
      userPackResponseProfileId  = entityKeyToInt64 profile
    }
