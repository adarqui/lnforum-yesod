{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.ResourceStar.Internal (
  getResourceStarsM,
  insertResourceStarM,
  getResourceStarM,
  getResourceStar_ByResourceM,
  getResourceStar_ByResourceIdM,
  updateResourceStarM,
  deleteResourceStarM
) where



import           Model.Prelude
import           Model.ResourceStar.Function



getResourceStarsM :: UserId -> Handler [Entity ResourceStar]
getResourceStarsM user_id = do
  selectListDb' [ ResourceStarUserId ==. user_id ] [] ResourceStarId



insertResourceStarM :: UserId -> ResourceId -> ResourceStarRequest -> Handler (Entity ResourceStar)
insertResourceStarM user_id thread_post_id star_request = do

    ts <- timestampH'
    let
      thread_post_star = (resourceStarRequestToResourceStar user_id thread_post_id star_request) { resourceStarCreatedAt = Just ts }
    insertEntityDb thread_post_star



getResourceStarM :: UserId -> ResourceStarId -> Handler (Entity ResourceStar)
getResourceStarM user_id thread_post_star_id = do
  notFoundMaybe =<< selectFirstDb [ ResourceStarId ==. thread_post_star_id, ResourceStarUserId ==. user_id ] []



getResourceStar_ByResourceM :: UserId -> Entity Resource -> Handler (Maybe (Entity ResourceStar))
getResourceStar_ByResourceM user_id (Entity thread_post_id _) = do
  selectFirstDb [ ResourceStarResourceId ==. thread_post_id, ResourceStarUserId ==. user_id ] []



getResourceStar_ByResourceIdM :: UserId -> ResourceId -> Handler (Maybe (Entity ResourceStar))
getResourceStar_ByResourceIdM user_id thread_post_id = do
  selectFirstDb [ ResourceStarResourceId ==. thread_post_id, ResourceStarUserId ==. user_id ] []



updateResourceStarM :: UserId -> ResourceStarId -> ResourceStarRequest -> Handler (Entity ResourceStar)
updateResourceStarM user_id thread_post_star_id ResourceStarRequest{..} = do

  ts <- timestampH'

  void $ runDB $ updateWhere
    [ ResourceStarId ==. thread_post_star_id, ResourceStarUserId ==. user_id ]

    [ ResourceStarModifiedAt =. Just ts
    , ResourceStarReason =. resourceStarRequestReason
    ]

  notFoundMaybe =<< selectFirstDb [ ResourceStarId ==. thread_post_star_id ] []



deleteResourceStarM :: UserId -> ResourceStarId -> Handler ()
deleteResourceStarM user_id thread_post_star_id = do
  deleteWhereDb [ ResourceStarUserId ==. user_id, ResourceStarId ==. thread_post_star_id ]
