{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Job (
  enq_CreateUserProfile,
  deq_CreateUserProfile,
  profilex'
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



import Import hiding (loadConfig)
import Database.Persist.Postgresql as P
import Yesod.Default.Config
import qualified Database.Persist
import Database.Persist.Postgresql (PostgresConf)
import Settings
import Model
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Yaml

-- https://github.com/yesodweb/yesod/wiki/Using-Database.Persist.runPool-without-Foundation

-- also

-- Probably the best way to run this is the same way that the scaffolded site runs the migration code:

-- Perform database migration using our application's logging settings.
-- runLoggingT
--    (Database.Persist.runPool dbconf (runMigration migrateAll) p)
--    (messageLoggerSource foundation logger)



runQueries = do
  selectList [UserNick ==. "adarqui"] []



mainx :: IO ()
mainx = do
  conf <- Yesod.Default.Config.loadConfig $ (configSettings Production)

  dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
            Database.Persist.loadConfig >>=
            Database.Persist.applyEnv
  p <- Database.Persist.createPoolConfig (dbconf :: PostgresConf)

  runStdoutLoggingT $ runResourceT $ Database.Persist.runPool dbconf go p
  where
  go = do
    liftIO $ enq_CreateUserProfile 0 defaultProfileRequest
    void $ runQueries




profilex :: IO ()
profilex = do
  conf <- Yesod.Default.Config.loadConfig $ (configSettings Production)

  dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
            Database.Persist.loadConfig >>=
            Database.Persist.applyEnv
  p <- Database.Persist.createPoolConfig (dbconf :: PostgresConf)

  runStdoutLoggingT $ runResourceT $ Database.Persist.runPool dbconf go p
  where
  go = do
    liftIO $ enq_CreateUserProfile 0 defaultProfileRequest
    void $ runQueries



profilex' :: IO ()
profilex' = do

  Just yaml <- decodeFile "config/workers.yaml"
  conf <- parseMonad P.loadConfig yaml
  dbconf <- applyEnv (conf :: PostgresConf)
  p <- createPoolConfig dbconf

  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn

  -- subscribe to the queue
  consumeMsgs chan "myQueue" Ack (myCallback' dbconf p)

  getLine :: IO Text
  closeConnection conn
  putStrLn "connection closed"



-- myCallback' :: (Message,Envelope) -> IO ()
myCallback' dbconf p (msg, env) = do
  putStrLn "callback .. "
  runStdoutLoggingT $ runResourceT $ Database.Persist.runPool dbconf go p
  ackEnv env
  where
  go = do
    users <- runQueries
    liftIO $ print users




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
