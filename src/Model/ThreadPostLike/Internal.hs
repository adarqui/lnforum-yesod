{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.ThreadPostLike.Internal (
  getThreadPostLikesM,
  insertThreadPostLikeM,
  getThreadPostLikeM,
  getThreadPostLike_ByThreadPostM,
  getThreadPostLike_ByThreadPostIdM,
  updateThreadPostLikeM,
  deleteThreadPostLikeM
) where



import           Model.Prelude
import           Model.ThreadPostLike.Function



getThreadPostLikesM :: UserId -> Handler [Entity ThreadPostLike]
getThreadPostLikesM user_id = do
  selectListDb' [ ThreadPostLikeUserId ==. user_id ] [] ThreadPostLikeId



insertThreadPostLikeM :: UserId -> ThreadPostId -> ThreadPostLikeRequest -> Handler (Entity ThreadPostLike)
insertThreadPostLikeM user_id thread_post_id like_request = do

    ts <- timestampH'
    let
      thread_post_like = (threadPostLikeRequestToThreadPostLike user_id thread_post_id like_request) { threadPostLikeCreatedAt = Just ts }
    insertEntityDb thread_post_like



getThreadPostLikeM :: UserId -> ThreadPostLikeId -> Handler (Entity ThreadPostLike)
getThreadPostLikeM user_id thread_post_like_id = do
  notFoundMaybe =<< selectFirstDb [ ThreadPostLikeId ==. thread_post_like_id, ThreadPostLikeUserId ==. user_id ] []



getThreadPostLike_ByThreadPostM :: UserId -> Entity ThreadPost -> Handler (Maybe (Entity ThreadPostLike))
getThreadPostLike_ByThreadPostM user_id (Entity thread_post_id _) = do
  selectFirstDb [ ThreadPostLikeThreadPostId ==. thread_post_id, ThreadPostLikeUserId ==. user_id ] []



getThreadPostLike_ByThreadPostIdM :: UserId -> ThreadPostId -> Handler (Maybe (Entity ThreadPostLike))
getThreadPostLike_ByThreadPostIdM user_id thread_post_id = do
  selectFirstDb [ ThreadPostLikeThreadPostId ==. thread_post_id, ThreadPostLikeUserId ==. user_id ] []



updateThreadPostLikeM :: UserId -> ThreadPostLikeId -> ThreadPostLikeRequest -> Handler (Entity ThreadPostLike)
updateThreadPostLikeM user_id thread_post_like_id ThreadPostLikeRequest{..} = do

  ts <- timestampH'

  void $ runDB $ updateWhere
    [ ThreadPostLikeId ==. thread_post_like_id, ThreadPostLikeUserId ==. user_id ]

    [ ThreadPostLikeModifiedAt =. Just ts
    , ThreadPostLikeOpt =. threadPostLikeRequestOpt
    , ThreadPostLikeReason =. threadPostLikeRequestReason
    , ThreadPostLikeScore =. likeOptToScore threadPostLikeRequestOpt
    ]

  notFoundMaybe =<< selectFirstDb [ ThreadPostLikeId ==. thread_post_like_id ] []



deleteThreadPostLikeM :: UserId -> ThreadPostLikeId -> Handler ()
deleteThreadPostLikeM user_id thread_post_like_id = do
  deleteWhereDb [ ThreadPostLikeUserId ==. user_id, ThreadPostLikeId ==. thread_post_like_id ]
