{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.ThreadPostStar.Internal (
  getThreadPostStarsM,
  insertThreadPostStarM,
  getThreadPostStarM,
  getThreadPostStar_ByThreadPostM,
  getThreadPostStar_ByThreadPostIdM,
  updateThreadPostStarM,
  deleteThreadPostStarM
) where



import           Model.Prelude
import           Model.ThreadPostStar.Function



getThreadPostStarsM :: UserId -> Handler [Entity ThreadPostStar]
getThreadPostStarsM user_id = do
  selectListDb' [ ThreadPostStarUserId ==. user_id ] [] ThreadPostStarId



insertThreadPostStarM :: UserId -> ThreadPostId -> ThreadPostStarRequest -> Handler (Entity ThreadPostStar)
insertThreadPostStarM user_id thread_post_id star_request = do

    ts <- timestampH'
    let
      thread_post_star = (threadPostStarRequestToThreadPostStar user_id thread_post_id star_request) { threadPostStarCreatedAt = Just ts }
    insertEntityDb thread_post_star



getThreadPostStarM :: UserId -> ThreadPostStarId -> Handler (Entity ThreadPostStar)
getThreadPostStarM user_id thread_post_star_id = do
  notFoundMaybe =<< selectFirstDb [ ThreadPostStarId ==. thread_post_star_id, ThreadPostStarUserId ==. user_id ] []



getThreadPostStar_ByThreadPostM :: UserId -> Entity ThreadPost -> Handler (Maybe (Entity ThreadPostStar))
getThreadPostStar_ByThreadPostM user_id (Entity thread_post_id _) = do
  selectFirstDb [ ThreadPostStarThreadPostId ==. thread_post_id, ThreadPostStarUserId ==. user_id ] []



getThreadPostStar_ByThreadPostIdM :: UserId -> ThreadPostId -> Handler (Maybe (Entity ThreadPostStar))
getThreadPostStar_ByThreadPostIdM user_id thread_post_id = do
  selectFirstDb [ ThreadPostStarThreadPostId ==. thread_post_id, ThreadPostStarUserId ==. user_id ] []



updateThreadPostStarM :: UserId -> ThreadPostStarId -> ThreadPostStarRequest -> Handler (Entity ThreadPostStar)
updateThreadPostStarM user_id thread_post_star_id ThreadPostStarRequest{..} = do

  ts <- timestampH'

  void $ runDB $ updateWhere
    [ ThreadPostStarId ==. thread_post_star_id, ThreadPostStarUserId ==. user_id ]

    [ ThreadPostStarModifiedAt =. Just ts
    , ThreadPostStarReason =. threadPostStarRequestReason
    ]

  notFoundMaybe =<< selectFirstDb [ ThreadPostStarId ==. thread_post_star_id ] []



deleteThreadPostStarM :: UserId -> ThreadPostStarId -> Handler ()
deleteThreadPostStarM user_id thread_post_star_id = do
  deleteWhereDb [ ThreadPostStarUserId ==. user_id, ThreadPostStarId ==. thread_post_star_id ]
