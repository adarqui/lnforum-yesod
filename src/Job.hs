{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Job (
  enq_CreateUserProfile,
  deq_CreateUserProfile
) where



import           Api.Params
import           Control
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.List                  (nub)
import           Import
import           LN.T.Internal.Types
import           LN.T.Job
import           Model.Misc
import           Network.AMQP



enq_CreateUserProfile :: Int64 -> ProfileRequest -> IO ()
enq_CreateUserProfile user_id profile_request = do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn

    -- declare a queue, exchange and binding
    declareQueue chan newQueue {queueName = "myQueue"}
    declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
    bindQueue chan "myQueue" "myExchange" "myKey"

    -- publish a message to our new exchange
    publishMsg chan "myExchange" "myKey"
        newMsg {msgBody = (BL.pack "hello world"),
                msgDeliveryMode = Just Persistent}

    closeConnection conn
    putStrLn "connection closed"




deq_CreateUserProfile :: IO ()
deq_CreateUserProfile = do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn

    -- declare a queue, exchange and binding
--    declareQueue chan newQueue {queueName = "myQueue"}
--    declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
--    bindQueue chan "myQueue" "myExchange" "myKey"

    -- subscribe to the queue
    consumeMsgs chan "myQueue" Ack myCallback

    getLine :: IO Text
    closeConnection conn
    putStrLn "connection closed"




myCallback :: (Message,Envelope) -> IO ()
myCallback (msg, env) = do
  putStrLn "callback"
--    putStrLn $ "received message: " ++ (BL.unpack $ msgBody msg)
    -- acknowledge receiving the message
  ackEnv env
