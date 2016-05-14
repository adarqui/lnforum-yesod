{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.ResourceLike.Internal (
  getResourceLikesM,
  insertResourceLikeM,
  getResourceLikeM,
  getResourceLike_ByResourceM,
  getResourceLike_ByResourceIdM,
  updateResourceLikeM,
  deleteResourceLikeM
) where



import           Model.Prelude
import           Model.ResourceLike.Function



getResourceLikesM :: UserId -> Handler [Entity ResourceLike]
getResourceLikesM user_id = do
  selectListDb' [ ResourceLikeUserId ==. user_id ] [] ResourceLikeId



insertResourceLikeM :: UserId -> ResourceId -> ResourceLikeRequest -> Handler (Entity ResourceLike)
insertResourceLikeM user_id thread_post_id like_request = do

    ts <- timestampH'
    let
      thread_post_like = (resourceLikeRequestToResourceLike user_id thread_post_id like_request) { resourceLikeCreatedAt = Just ts }
    insertEntityDb thread_post_like



getResourceLikeM :: UserId -> ResourceLikeId -> Handler (Entity ResourceLike)
getResourceLikeM user_id thread_post_like_id = do
  notFoundMaybe =<< selectFirstDb [ ResourceLikeId ==. thread_post_like_id, ResourceLikeUserId ==. user_id ] []



getResourceLike_ByResourceM :: UserId -> Entity Resource -> Handler (Maybe (Entity ResourceLike))
getResourceLike_ByResourceM user_id (Entity thread_post_id _) = do
  selectFirstDb [ ResourceLikeResourceId ==. thread_post_id, ResourceLikeUserId ==. user_id ] []



getResourceLike_ByResourceIdM :: UserId -> ResourceId -> Handler (Maybe (Entity ResourceLike))
getResourceLike_ByResourceIdM user_id thread_post_id = do
  selectFirstDb [ ResourceLikeResourceId ==. thread_post_id, ResourceLikeUserId ==. user_id ] []



updateResourceLikeM :: UserId -> ResourceLikeId -> ResourceLikeRequest -> Handler (Entity ResourceLike)
updateResourceLikeM user_id thread_post_like_id ResourceLikeRequest{..} = do

  ts <- timestampH'

  void $ runDB $ updateWhere
    [ ResourceLikeId ==. thread_post_like_id, ResourceLikeUserId ==. user_id ]

    [ ResourceLikeModifiedAt =. Just ts
    , ResourceLikeOpt =. resourceLikeRequestOpt
    , ResourceLikeReason =. resourceLikeRequestReason
    , ResourceLikeScore =. likeOptToScore resourceLikeRequestOpt
    ]

  notFoundMaybe =<< selectFirstDb [ ResourceLikeId ==. thread_post_like_id ] []



deleteResourceLikeM :: UserId -> ResourceLikeId -> Handler ()
deleteResourceLikeM user_id thread_post_like_id = do
  deleteWhereDb [ ResourceLikeUserId ==. user_id, ResourceLikeId ==. thread_post_like_id ]
