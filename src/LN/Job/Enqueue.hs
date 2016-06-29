{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Job.Enqueue (
  mkJob_CreateUserProfile
) where



import           LN.Import.NoFoundation
import           LN.Job.Shared
import           LN.T.Internal.Types
import           LN.T.Job




mkJob_CreateUserProfile :: UserId -> ProfileRequest -> IO ()
mkJob_CreateUserProfile user_id profile_request = do
  bgRunEnq QCreateUserProfile $ bgEnq (user_id, profile_request)
  pure ()
