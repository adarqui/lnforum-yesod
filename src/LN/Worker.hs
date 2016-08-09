{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module LN.Worker (
  workerMain
) where



import           Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Database.Redis        as Redis
import           Network.AMQP
import qualified Prelude               as Prelude

import           LN.All.Api            (insertApiM)
import           LN.All.Empty          (emptyM)
import           LN.All.Profile        (insertProfileM)
import           LN.Application        (handler)
import           LN.Control
import           LN.Import
import           LN.Job.Shared
import           LN.Misc.Codec         (keyToInt64)
import           LN.T.Api              (ApiRequest (..))
import           LN.T.Job
import           LN.T.Profile          (ProfileRequest (..))



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
initializeWorker_RemoveThreadPostFromSet =
  bgRunDeq QRemoveThreadPostFromSet (bgDeq runWorker_RemoveThreadPostFromSet)



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
    let lr = eitherDecode msgBody :: Either String (ThreadId, ThreadPostId)
    case lr of
      Left err                   -> liftIO $ Prelude.putStrLn err
      Right (thread_id, post_id) -> do
        void $ ((run $ do
          emptyM
          red <- getsYesod appRed
          let
            thread_id' = keyToInt64 thread_id
            post_id'   = fromIntegral $ keyToInt64 post_id
          liftIO $ Prelude.putStrLn "zadd 1.."
          void $ liftIO $ Redis.runRedis red $ Redis.zadd ("thread_posts:"<>(BSC.pack $ show thread_id')) [(post_id', BSC.pack $ show post_id')]
          liftIO $ Prelude.putStrLn "zadd 2.."
          pure ()) :: Handler ())
   ) :: IO (Either SomeException ()))
  ackEnv env



runWorker_RemoveThreadPostFromSet :: (Message, Envelope) -> IO ()
runWorker_RemoveThreadPostFromSet (Message{..}, env) = do
  void $ (try (handler $ do
    liftIO $ putStrLn "runJob_RemoveThreadPostFromSet"
    let lr = eitherDecode msgBody :: Either String (ThreadId, ThreadPostId)
    case lr of
      Left err                   -> liftIO $ Prelude.putStrLn err
      Right (thread_id, post_id) -> do
        void $ ((run $ do
          emptyM
          red <- getsYesod appRed
          let
            thread_id' = keyToInt64 thread_id
            post_id'   = keyToInt64 post_id
          void $ liftIO $ Redis.runRedis red $ Redis.zrem ("thread_posts:"<>(BSC.pack $ show thread_id')) [BSC.pack $ show post_id']
          pure ()) :: Handler ())
   ) :: IO (Either SomeException ()))
  ackEnv env



workerMain :: IO ()
workerMain = do
  putStrLn $ "Launching workers.."
  initializeWorkers
  forever (getLine :: IO String)
