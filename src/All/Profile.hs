{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module All.Profile (
  -- Handler
  getProfilesR,
  getProfileR,
  putProfileR,

  -- Model/Function
  profileRequestToProfile,
  profileToResponse,
  profilesToResponses,

  -- Model/Internal
  getProfilesM,
  getProfileM,
  getProfile_ByUserIdM,
  updateProfileM,
) where



import           All.Prelude



--
-- Handler
--

getProfilesR :: HandlerEff Value
getProfilesR = do
  user_id <- requireAuthId
  (toJSON . profilesToResponses) <$> getProfilesM user_id



getProfileR :: ProfileId -> HandlerEff Value
getProfileR profile_id = do
  user_id <- requireAuthId
  (toJSON . profileToResponse) <$> getProfileM user_id profile_id



putProfileR :: ProfileId -> HandlerEff Value
putProfileR profile_id = do
  user_id <- requireAuthId
  profile_request <- requireJsonBody
  (toJSON . profileToResponse) <$> updateProfileM user_id profile_id profile_request






--
-- Model/Function
--

profileRequestToProfile :: UserId -> ProfileRequest -> Profile
profileRequestToProfile user_id ProfileRequest{..} = Profile {
  profileUserId     = user_id,
  profileGender     = profileRequestGender,
  profileBirthdate  = profileRequestBirthdate,
  profileWebsite    = profileRequestWebsite,
  profileLocation   = profileRequestLocation,
  profileSignature  = profileRequestSignature,
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
-- Model/Internal
--

getProfilesM :: UserId -> HandlerEff [Entity Profile]
getProfilesM _ = do
  selectListDb' [] [] ProfileId



getProfileM :: UserId -> ProfileId -> HandlerEff (Entity Profile)
getProfileM _ profile_id = do
  notFoundMaybe =<< selectFirstDb [ ProfileId ==. profile_id ] []



getProfile_ByUserIdM :: UserId -> UserId -> HandlerEff (Entity Profile)
getProfile_ByUserIdM _ lookup_user_id = do
  notFoundMaybe =<< selectFirstDb [ProfileUserId ==. lookup_user_id] []



updateProfileM :: UserId -> ProfileId -> ProfileRequest -> HandlerEff (Entity Profile)
updateProfileM user_id profile_id profile_request = do

  ts <- timestampH'

  let
    Profile{..} = (profileRequestToProfile user_id profile_request) { profileModifiedAt = Just ts }

  updateWhereDb
    [ ProfileUserId ==. user_id, ProfileId ==. profile_id ]
    [ ProfileModifiedAt =. profileModifiedAt
    , ProfileGender =. profileGender
    , ProfileBirthdate =. profileBirthdate
    , ProfileWebsite =. profileWebsite
    , ProfileLocation =. profileLocation
    , ProfileSignature =. profileSignature
    ]

  notFoundMaybe =<< selectFirstDb [ ProfileUserId ==. user_id, ProfileId ==. profile_id ] []
