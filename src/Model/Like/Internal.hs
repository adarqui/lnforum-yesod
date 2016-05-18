{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Like.Internal (
  getLikesM,
  insertLikeM,
  getLikeM,
  getLike_ByThreadPostM,
  getLike_ByThreadPostIdM,
  updateLikeM,
  deleteLikeM
) where



import           Model.Prelude
import           Model.Like.Function



getLikesM :: UserId -> Handler [Entity Like]
getLikesM user_id = do
  selectListDb' [ LikeUserId ==. user_id ] [] LikeId



insertLikeM :: UserId -> Ent -> Int64 -> LikeRequest -> Handler (Entity Like)
insertLikeM user_id ent ent_id like_request = do

  ts <- timestampH'
  let
    like = (likeRequestToLike user_id ent ent_id like_request) { likeCreatedAt = Just ts }

  insertEntityDb like



getLikeM :: UserId -> LikeId -> Handler (Entity Like)
getLikeM user_id like_id = do
  notFoundMaybe =<< selectFirstDb [ LikeId ==. like_id, LikeUserId ==. user_id ] []



getLike_ByThreadPostM :: UserId -> Entity ThreadPost -> Handler (Maybe (Entity Like))
getLike_ByThreadPostM user_id thread_post = do
  selectFirstDb [ LikeUserId ==. user_id, LikeEnt ==. Ent_ThreadPost, LikeEntId ==. thread_post_id ] []
  where
  thread_post_id = entityKeyToInt64 thread_post



getLike_ByThreadPostIdM :: UserId -> ThreadPostId -> Handler (Maybe (Entity Like))
getLike_ByThreadPostIdM user_id thread_post_id = do
  selectFirstDb [ LikeUserId ==. user_id, LikeEnt ==. Ent_ThreadPost, LikeEntId ==. thread_post_id' ] []
  where
  thread_post_id' = keyToInt64 thread_post_id



updateLikeM :: UserId -> LikeId -> LikeRequest -> Handler (Entity Like)
updateLikeM user_id like_id LikeRequest{..} = do

  ts <- timestampH'

  void $ runDB $ updateWhere
    [ LikeId ==. like_id, LikeUserId ==. user_id ]

    [ LikeModifiedAt =. Just ts
    , LikeOpt        =. likeRequestOpt
    , LikeReason     =. likeRequestReason
    , LikeScore      =. likeOptToScore likeRequestOpt
    ]

  notFoundMaybe =<< selectFirstDb [ LikeId ==. like_id ] []



deleteLikeM :: UserId -> LikeId -> Handler ()
deleteLikeM user_id like_id = do
  deleteWhereDb [ LikeUserId ==. user_id, LikeId ==. like_id ]
