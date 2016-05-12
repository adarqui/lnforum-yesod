{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Profile.Function (
  profileRequestToProfile,
  profileToResponse,
  profilesToResponses,
) where



import           Model.Prelude



profileRequestToProfile :: UserId -> ProfileRequest -> Profile
profileRequestToProfile user_id ProfileRequest{..} = Profile {
  profileUserId  = user_id,
  profileGender  = profileRequestGender,
  profileBirthdate = profileRequestBirthdate,
  profileWebsite = profileRequestWebsite,
  profileLocation = profileRequestLocation,
  profileSignature = profileRequestSignature,
  profileKarmaGood = 0,
  profileKarmaBad = 0,
  profileCreatedAt = Nothing,
  profileModifiedAt = Nothing
}



profileToResponse :: Entity Profile -> ProfileResponse
profileToResponse (Entity profile_id Profile{..}) = ProfileResponse {
  profileResponseId = keyToInt64 profile_id,
  profileResponseEntityId = keyToInt64 profileUserId,
  profileResponseGender = profileGender,
  profileResponseBirthdate = profileBirthdate,
  profileResponseWebsite = profileWebsite,
  profileResponseLocation = profileLocation,
  profileResponseSignature = profileSignature,
  profileResponseKarmaGood = profileKarmaGood,
  profileResponseKarmaBad = profileKarmaBad,
  profileResponseCreatedAt = profileCreatedAt,
  profileResponseModifiedAt = profileModifiedAt
}



profilesToResponses :: [Entity Profile] -> ProfileResponses
profilesToResponses profiles = ProfileResponses {
  profileResponses = map profileToResponse profiles
}
