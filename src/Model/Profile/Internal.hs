{-# LANGUAGE RecordWildCards #-}

module Model.Profile.Internal (
  getProfilesM,
  getProfileM,
  getProfileBy_UserIdM,
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



getProfileBy_UserIdM :: UserId -> UserId -> Handler (Entity Profile)
getProfileBy_UserIdM _ lookup_user_id = do
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
