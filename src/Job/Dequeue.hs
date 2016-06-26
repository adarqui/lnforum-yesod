{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Job.Dequeue (
) where



import All.Organization
import Misc.Codec (int64ToKey')
import Control
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



runQueries = do
  selectList [UserNick ==. "adarqui"] []


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
--    void $ run $ getOrganizationsM (int64ToKey' 0)
    liftIO $ print users
