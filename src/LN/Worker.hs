{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module LN.Worker (
  workerMain
) where



import           Data.Aeson
import           LN.All.Api     (insertApiM)
import           LN.All.Profile (insertProfileM)
import           LN.Application (handler)
import           LN.Control
import           LN.Import
import           LN.Job.Shared
import           LN.T.Api       (ApiRequest (..))
import           LN.T.Job
import           LN.T.Profile   (ProfileRequest (..))
import           Network.AMQP
import qualified Prelude        as Prelude



initializeWorkers :: IO ()
initializeWorkers = do
  initializeWorker_CreateUserProfile
  initializeWorker_CreateUserApi
  initializeWorker_AddThreadPostToSet
  initializeWorker_RemoveThreadPostFromSet

initializeWorker_CreateUserProfile :: IO ()
initializeWorker_CreateUserProfile = do
  bgRunDeq QCreateUserProfile (bgDeq runWorker_CreateUserProfile)

initializeWorker_CreateUserApi :: IO ()
initializeWorker_CreateUserApi = do
  bgRunDeq QCreateUserApi (bgDeq runWorker_CreateUserApi)

initializeWorker_AddThreadPostToSet :: IO ()
initializeWorker_AddThreadPostToSet =
  bgRunDeq QAddThreadPostToSet (bgDeq runWorker_AddThreadPostToSet)

initializeWorker_RemoveThreadPostFromSet :: IO ()
initializeWorker_RemoveThreadPostFromSet = pure ()



runWorker_CreateUserProfile :: (Message, Envelope) -> IO ()
runWorker_CreateUserProfile (Message{..}, env) = do
  void $ (try (handler $ do
    liftIO $ putStrLn "runJob_CreateUserProfile"
    let e_resp = eitherDecode msgBody :: Either String (UserId, ProfileRequest)
    case e_resp of
      Left err                         -> liftIO $ Prelude.putStrLn err
      Right (user_id, profile_request) -> do
        liftIO $ putStrLn "success"
        void $ run $ insertProfileM user_id profile_request
        pure ()) :: IO (Either SomeException ()))
  ackEnv env



runWorker_CreateUserApi :: (Message, Envelope) -> IO ()
runWorker_CreateUserApi (Message{..}, env) = do
  void $ (try (handler $ do
    liftIO $ putStrLn "runJob_CreateUserApi"
    let e_resp = eitherDecode msgBody :: Either String (UserId, ApiRequest)
    case e_resp of
      Left err                     -> liftIO $ Prelude.putStrLn err
      Right (user_id, api_request) -> do
        liftIO $ putStrLn "success"
        void $ run $ insertApiM user_id api_request
        pure ()) :: IO (Either SomeException ()))
  ackEnv env



runWorker_AddThreadPostToSet :: (Message, Envelope) -> IO ()
runWorker_AddThreadPostToSet (Message{..}, env) = do
  void $ (try (handler $ do
    liftIO $ putStrLn "runJob_AddThreadPostToSet"
    pure ()
   ) :: IO (Either SomeException ()))
  ackEnv env



workerMain :: IO ()
workerMain = do
  putStrLn $ "Launching workers.."
  initializeWorkers
  forever (getLine :: IO String)
