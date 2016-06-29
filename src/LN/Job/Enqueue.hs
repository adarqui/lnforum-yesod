{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Job.Enqueue (
  mkJob_CreateUserProfile
) where



import qualified Data.ByteString.Lazy.Char8 as BL
import           Import.NoFoundation
import           LN.Job.Shared
import           LN.T.Internal.Types
import           LN.T.Internal.JSON
import           LN.T.Job
import           LN.Misc.Codec                 (int64ToKey')
import           Network.AMQP




mkJob_CreateUserProfile :: UserId -> ProfileRequest -> IO ()
mkJob_CreateUserProfile user_id profile_request = do
  bgRunEnq QCreateUserProfile $ bgEnq (user_id, profile_request)
  pure ()
