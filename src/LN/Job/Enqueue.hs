{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Job.Enqueue (
  mkJob_CreateUserProfile,
  mkJob_CreateUserApi,
) where



import           LN.Import.NoFoundation
import           LN.Job.Shared
import           LN.T.Api               (ApiRequest (..))
import           LN.T.Job
import           LN.T.Profile           (ProfileRequest (..))




mkJob_CreateUserProfile :: UserId -> ProfileRequest -> IO ()
mkJob_CreateUserProfile user_id profile_request = do
  bgRunEnq QCreateUserProfile $ bgEnq (user_id, profile_request)
  pure ()



mkJob_CreateUserApi :: UserId -> ApiRequest -> IO ()
mkJob_CreateUserApi user_id api_request = do
  bgRunEnq QCreateUserApi $ bgEnq (user_id, api_request)
  pure ()
