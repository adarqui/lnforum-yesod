{-# LANGUAGE RecordWildCards #-}

module All.Like (
  -- handler
  getLikesR,
  postLikeR0,
  getLikeR,
  putLikeR,
  deleteLikeR,

  getLikeStatsR,
  getLikeStatR,

  -- model/functions
  likeRequestToLike,
  likeToResponse,
  likesToResponses,

  -- model/internal
  getLikesM,
  insertLikeM,
  getLikeM,
  getLike_ByThreadPostM,
  getLike_ByThreadPostIdM,
  updateLikeM,
  deleteLikeM,

  getLikeStatsM,
  getLikeStatM
) where



import           Handler.Prelude
import           Model.Prelude
import qualified LN.T.Like as L



--
-- Handler
--

getLikesR :: Handler Value
getLikesR = do
  user_id <- requireAuthId
  (toJSON . likesToResponses) <$> getLikesM user_id



postLikeR0 :: Handler Value
postLikeR0 = do

  sp <- lookupStandardParams

  case (lookupLikeEnt sp) of

    Nothing            -> permissionDenied "Must supply a entity information"

    Just (ent, ent_id) -> do

      user_id <- requireAuthId
      like_request <- requireJsonBody
      (toJSON . likeToResponse) <$> insertLikeM user_id ent ent_id like_request



getLikeR :: LikeId -> Handler Value
getLikeR like_id = do
  user_id <- requireAuthId
  (toJSON . likeToResponse) <$> getLikeM user_id like_id



putLikeR :: LikeId -> Handler Value
putLikeR like_id = do
  user_id <- requireAuthId
  like_request <- requireJsonBody
  (toJSON . likeToResponse) <$> updateLikeM user_id like_id like_request



deleteLikeR :: LikeId -> Handler Value
deleteLikeR like_id = do
  user_id <- requireAuthId
  void $ deleteLikeM user_id like_id
  pure $ toJSON ()



getLikeStatsR :: Handler Value
getLikeStatsR = do
  user_id <- requireAuthId
  toJSON <$> getLikeStatsM user_id



getLikeStatR :: LikeId -> Handler Value
getLikeStatR like_id = do
  user_id <- requireAuthId
  toJSON <$> getLikeStatM user_id like_id





--
-- Model/Function
--

likeRequestToLike :: UserId -> Ent -> Int64 -> LikeRequest -> Like
likeRequestToLike user_id ent ent_id LikeRequest{..} = Like {
  likeUserId         = user_id,
  likeEnt            = ent,
  likeEntId          = ent_id,
  likeOpt            = likeRequestOpt,
  likeScore          = likeOptToScore likeRequestOpt,
  likeReason         = likeRequestReason,
  likeActive         = True,
  likeGuard          = likeRequestGuard,
  likeCreatedAt      = Nothing,
  likeModifiedAt     = Nothing
}



likeToResponse :: Entity Like -> LikeResponse
likeToResponse (Entity like_id Like{..}) = LikeResponse {
  likeResponseId         = keyToInt64 like_id,
  likeResponseUserId     = keyToInt64 likeUserId,
  likeResponseEnt        = likeEnt,
  likeResponseEntId      = likeEntId,
  likeResponseOpt        = likeOpt,
  likeResponseScore      = likeScore,
  likeResponseReason     = likeReason,
  likeResponseActive     = likeActive,
  likeResponseGuard      = likeGuard,
  likeResponseCreatedAt  = likeCreatedAt,
  likeResponseModifiedAt = likeModifiedAt
}



likesToResponses :: [Entity Like] -> LikeResponses
likesToResponses likes = LikeResponses {
  likeResponses = map likeToResponse likes
}



--
-- Model/Internal
--

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



getLikeStatsM :: UserId -> Handler LikeStatResponses
getLikeStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spBoardId of

    Just _  -> notFound
    Nothing -> notFound




getLikeStatM :: UserId -> LikeId -> Handler LikeStatResponse
getLikeStatM user_id _ = do

  sp@StandardParams{..} <- lookupStandardParams

  case spThreadPostId of
    Just thread_post_id -> getLikeStat_ByThreadPostIdM user_id thread_post_id
    _                   -> notFound



getLikeStat_ByThreadPostIdM :: UserId -> ThreadPostId -> Handler LikeStatResponse
getLikeStat_ByThreadPostIdM user_id thread_post_id = do
--  <- countDb [ LikePostLikeId ==. like_id ]
  likes <- selectListDb' [LikeEnt ==. Ent_ThreadPost, LikeEntId ==. i64] [] LikeId
  let
    opts   = map (\(Entity _ Like{..}) -> likeOpt) likes
    scores = map (\(Entity _ Like{..}) -> likeScore) likes

  return $ LikeStatResponse {
    likeStatResponseEnt     = Ent_ThreadPost,
    likeStatResponseEntId   = i64,
    likeStatResponseScore   = fromIntegral $ sum scores,
    likeStatResponseLike    = fromIntegral $ length $ filter (==L.Like) opts,
    likeStatResponseDislike = fromIntegral $ length $ filter (==L.Dislike) opts
  }
  where
  i64 = keyToInt64 thread_post_id