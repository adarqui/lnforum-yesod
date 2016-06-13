{-# LANGUAGE RecordWildCards #-}

module Model.Profile.Internal (
  getProfilesM,
  getProfileM,
  getProfile_ByUserIdM,
  updateProfileM,
) where



import           Model.Prelude
import           Model.Profile.Function



getProfilesM :: UserId -> Handler [Entity Profile]
getProfilesM _ = do
  selectListDb' [] [] ProfileId



getProfileM :: UserId -> ProfileId -> Handler (Entity Profile)
getProfileM _ profile_id = do
  notFoundMaybe =<< selectFirstDb [ ProfileId ==. profile_id ] []



getProfile_ByUserIdM :: UserId -> UserId -> Handler (Entity Profile)
getProfile_ByUserIdM _ lookup_user_id = do
  notFoundMaybe =<< selectFirstDb [ProfileUserId ==. lookup_user_id] []



updateProfileM :: UserId -> ProfileId -> ProfileRequest -> Handler (Entity Profile)
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
