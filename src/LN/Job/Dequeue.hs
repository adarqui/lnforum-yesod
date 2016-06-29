{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module LN.Job.Dequeue (
  runJobs,
  runJob_CreateUserProfile
) where



import           Control.Monad.Logger        (runStdoutLoggingT)
import           Data.Aeson
import           Data.Yaml
import qualified Database.Persist
import           Database.Persist.Postgresql as P
import           LN.All.Profile              (profileRequestToProfile)
import           LN.Import
import           LN.Job.Shared
import           LN.T.Internal.Types
import           LN.T.Job
import           Network.AMQP
import qualified Prelude                     as Prelude



runJobs :: FilePath -> IO ()
runJobs path = do

  Just yaml <- decodeFile path
  conf      <- parseMonad P.loadConfig yaml
  dbconf    <- applyEnv (conf :: PostgresConf)
  pool      <- createPoolConfig dbconf

  bgRunDeq QCreateUserProfile (bgDeq $ runJob_CreateUserProfile dbconf pool)

--  closeConnection conn ??



runJob_CreateUserProfile
  :: forall c. (PersistConfig c, PersistConfigBackend c ~ ReaderT SqlBackend)
  => c
  -> PersistConfigPool c
  -> (Message, Envelope)
  -> IO ()
runJob_CreateUserProfile dbconf pool (Message{..}, env) = do
  liftIO $ putStrLn "runJob_CreateUserProfile"
  let e_resp = eitherDecode msgBody :: Either String (UserId, ProfileRequest)
  case e_resp of
    Left err                         -> liftIO $ Prelude.putStrLn err
    Right (user_id, profile_request) -> do
      liftIO $ putStrLn "success"
      void $ liftIO $ runStdoutLoggingT $ runResourceT $ Database.Persist.runPool dbconf go pool
      ackEnv env
      where
      go = do
        let profile = profileRequestToProfile user_id profile_request
        insertEntity profile
