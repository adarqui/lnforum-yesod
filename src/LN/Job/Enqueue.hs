{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Job.Enqueue (
  mkJob_CreateUserProfile,
  mkJob_CreateUserApi,
  mkJob_AddThreadPostToSet,
  mkJob_RemoveThreadPostFromSet
) where



import           LN.Import.NoFoundation
import           LN.Job.Shared
import           LN.T.Api               (ApiRequest (..))
import           LN.T.Job
import           LN.T.Profile           (ProfileRequest (..))




mkJob_CreateUserProfile :: UserId -> ProfileRequest -> IO ()
mkJob_CreateUserProfile user_id profile_request = do
  bgRunEnq QCreateUserProfile $ bgEnq (user_id, profile_request)
  pure ()



mkJob_CreateUserApi :: UserId -> ApiRequest -> IO ()
mkJob_CreateUserApi user_id api_request = do
  bgRunEnq QCreateUserApi $ bgEnq (user_id, api_request)
  pure ()



mkJob_AddThreadPostToSet :: ThreadId -> ThreadPostId -> IO ()
mkJob_AddThreadPostToSet thread_id post_id = do
  bgRunEnq QAddThreadPostToSet $ bgEnq (thread_id, post_id)



mkJob_RemoveThreadPostFromSet :: ThreadId -> ThreadPostId -> IO ()
mkJob_RemoveThreadPostFromSet thread_id post_id = do
  bgRunEnq QRemoveThreadPostFromSet $ bgEnq (thread_id, post_id)
