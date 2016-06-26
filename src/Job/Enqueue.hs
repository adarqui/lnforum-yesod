{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Job.Enqueue (
  mkJob_CreateUserProfile
) where



import           All.Organization
import           Api.Params
import           Control
import           Control
import qualified Data.ByteString.Lazy.Char8   as BL
import           Data.List                    (nub)
import           Import.NoFoundation
import           LN.T.Internal.Types
import           LN.T.Job
import           Misc.Codec                   (int64ToKey')
import           Model.Misc
import           Network.AMQP
import Job.Shared




mkJob_CreateUserProfile :: UserId -> ProfileRequest -> IO ()
mkJob_CreateUserProfile user_id profile_request = do

  -- publish a message to our new exchange
--  publishMsg chan "myExchange" "myKey"
--      newMsg {msgBody = (BL.pack "hello world"),
--              msgDeliveryMode = Just Persistent}

--  closeConnection conn
--  putStrLn "connection closed"

  bgRunEnq QCreateUserProfile (bgEnq "hello")

  return ()
