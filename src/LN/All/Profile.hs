{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module LN.All.Profile (
  -- LN.Handler
  getProfilesR,
  getProfileR,
  putProfileR,

  -- LN.Model/Function
  profileRequestToProfile,
  profileToResponse,
  profilesToResponses,

  -- LN.Model/Internal
  getProfilesM,
  getProfileM,
  getProfile_ByUserIdM,
  insertProfileM,
  updateProfileM,
) where



import           LN.All.Prelude



--
-- LN.Handler
--

getProfilesR :: Handler Value
getProfilesR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON profilesToResponses $ getProfilesM (pure sp) user_id



getProfileR :: ProfileId -> Handler Value
getProfileR profile_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON profileToResponse $ getProfileM user_id profile_id



putProfileR :: ProfileId -> Handler Value
putProfileR profile_id = run $ do
  user_id         <- _requireAuthId
  profile_request <- requireJsonBody
  errorOrJSON profileToResponse $ updateProfileM user_id profile_id profile_request






--
-- LN.Model/Function
--

profileRequestToProfile :: UserId -> ProfileRequest -> Profile
profileRequestToProfile user_id ProfileRequest{..} = Profile {
  profileUserId     = user_id,
  profileGender     = profileRequestGender,
  profileBirthdate  = profileRequestBirthdate,
  profileWebsite    = profileRequestWebsite,
  profileLocation   = profileRequestLocation,
  profileSignature  = profileRequestSignature,
  profileDebug      = profileRequestDebug,
  profileKarmaGood  = 0,
  profileKarmaBad   = 0,
  profileGuard      = profileRequestGuard,
  profileCreatedAt  = Nothing,
  profileModifiedAt = Nothing
}



profileToResponse :: Entity Profile -> ProfileResponse
profileToResponse (Entity profile_id Profile{..}) = ProfileResponse {
  profileResponseId         = keyToInt64 profile_id,
  profileResponseEnt        = Ent_None,
  profileResponseEntId      = keyToInt64 profileUserId,
  profileResponseGender     = profileGender,
  profileResponseBirthdate  = profileBirthdate,
  profileResponseWebsite    = profileWebsite,
  profileResponseLocation   = profileLocation,
  profileResponseSignature  = profileSignature,
  profileResponseDebug      = profileDebug,
  profileResponseKarmaGood  = profileKarmaGood,
  profileResponseKarmaBad   = profileKarmaBad,
  profileResponseGuard      = profileGuard,
  profileResponseCreatedAt  = profileCreatedAt,
  profileResponseModifiedAt = profileModifiedAt
}



profilesToResponses :: [Entity Profile] -> ProfileResponses
profilesToResponses profiles = ProfileResponses {
  profileResponses = map profileToResponse profiles
}






--
-- LN.Model/Internal
--

getProfilesM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Profile]
getProfilesM m_sp user_id = do
  case (lookupSpMay m_sp spUserId) of
    Just lookup_user_id -> getProfiles_ByUserIdM m_sp user_id lookup_user_id
    _                   -> getProfiles_ByEverythingM m_sp user_id


getProfiles_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Profile]
getProfiles_ByEverythingM m_sp _ = do
  selectListDbE m_sp [] [] ProfileId



getProfiles_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity Profile]
getProfiles_ByUserIdM m_sp _ lookup_user_id = do
  selectListDbE m_sp [ProfileUserId ==. lookup_user_id] [] ProfileId



getProfileM :: UserId -> ProfileId -> HandlerErrorEff (Entity Profile)
getProfileM _ profile_id = do
  selectFirstDbE [ProfileId ==. profile_id] []



getProfile_ByUserIdM :: UserId -> UserId -> HandlerErrorEff (Entity Profile)
getProfile_ByUserIdM _ lookup_user_id = do
  selectFirstDbE [ProfileUserId ==. lookup_user_id] []



insertProfileM :: UserId -> ProfileRequest -> HandlerErrorEff (Entity Profile)
insertProfileM user_id profile_request = do
  ts <- timestampH'
  let
    profile = (profileRequestToProfile user_id profile_request) { profileCreatedAt = Just ts }
  insertEntityDbE profile



updateProfileM :: UserId -> ProfileId -> ProfileRequest -> HandlerErrorEff (Entity Profile)
updateProfileM user_id profile_id profile_request = do

  ts <- timestampH'

  let
    Profile{..} = (profileRequestToProfile user_id profile_request) { profileModifiedAt = Just ts }

  updateWhereDb
    [ ProfileUserId ==. user_id, ProfileId ==. profile_id ]
    [ ProfileModifiedAt =. profileModifiedAt
    , ProfileGender     =. profileGender
    , ProfileBirthdate  =. profileBirthdate
    , ProfileWebsite    =. profileWebsite
    , ProfileLocation   =. profileLocation
    , ProfileSignature  =. profileSignature
    , ProfileDebug      =. profileDebug
    ]

  selectFirstDbE [ProfileUserId ==. user_id, ProfileId ==. profile_id] []
