{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Job.Dequeue (
  runJobs,
  runJob_CreateUserProfile
) where



import           All.Organization
import           Api.Params
import           Control
import           Control
import qualified Data.ByteString.Lazy.Char8   as BL
import           Data.List                    (nub)
import           Import
import           LN.T.Internal.Types
import           LN.T.Job
import           Misc.Codec                   (int64ToKey')
import           Model.Misc
import           Network.AMQP
import Job.Shared



import           Control.Monad.Logger         (runStdoutLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Yaml
import qualified Database.Persist
import           Database.Persist.Postgresql  as P
import           Database.Persist.Postgresql  (PostgresConf)
import           Import                       hiding (loadConfig)
import           Model
import           Settings
import           Yesod.Default.Config



runQueries = do
  selectList [UserNick ==. "adarqui"] []



runJobs :: FilePath -> IO ()
runJobs path = do

  Just yaml <- decodeFile path
  conf      <- parseMonad P.loadConfig yaml
  dbconf    <- applyEnv (conf :: PostgresConf)
  pool      <- createPoolConfig dbconf

  bgRunDeq QCreateUserProfile (bgDeq $ runJob_CreateUserProfile dbconf pool)

--  closeConnection conn



runJob_CreateUserProfile dbconf pool (msg, env) = do
  liftIO $ putStrLn "runJob_CreateUserProfile"
  liftIO $ runStdoutLoggingT $ runResourceT $ Database.Persist.runPool dbconf go pool
  ackEnv env
  where
  go = do
    users <- runQueries
    liftIO $ print users
