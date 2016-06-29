{-# LANGUAGE RecordWildCards #-}

module LN.All.Like (
  -- LN.Handler
  getLikesR,
  postLikeR0,
  getLikeR,
  putLikeR,
  deleteLikeR,
  getLikeStatsR,
  getLikeStatR,

  -- Model/Function
  likeRequestToLike,
  likeToResponse,
  likesToResponses,

  -- Model/Internal
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



import           LN.All.Prelude
import qualified LN.T.Like   as L



--
-- LN.Handler
--

getLikesR :: LN.Handler Value
getLikesR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON likesToResponses $ getLikesM (pure sp) user_id



postLikeR0 :: LN.Handler Value
postLikeR0 = run $ do
  user_id      <- _requireAuthId
  like_request <- requireJsonBody
  sp           <- lookupStandardParams
  errorOrJSON likeToResponse $ insertLikeM (pure sp) user_id like_request



getLikeR :: LikeId -> LN.Handler Value
getLikeR like_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON likeToResponse $ getLikeM user_id like_id



putLikeR :: LikeId -> LN.Handler Value
putLikeR like_id = run $ do
  user_id      <- _requireAuthId
  like_request <- requireJsonBody
  errorOrJSON likeToResponse $ updateLikeM user_id like_id like_request



deleteLikeR :: LikeId -> LN.Handler Value
deleteLikeR like_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteLikeM user_id like_id



getLikeStatsR :: LN.Handler Value
getLikeStatsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getLikeStatsM (pure sp) user_id



getLikeStatR :: LikeId -> LN.Handler Value
getLikeStatR like_id = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getLikeStatM (pure sp) user_id like_id





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

getLikesM :: Maybe StandardParams -> UserId -> LN.HandlerErrorEff [Entity Like]
getLikesM m_sp user_id = do
  selectListDbE m_sp [LikeUserId ==. user_id, LikeActive ==. True] [] LikeId



insertLikeM :: Maybe StandardParams -> UserId -> LikeRequest -> LN.HandlerErrorEff (Entity Like)
insertLikeM m_sp user_id like_request = do

  case (lookupLikeEntMay m_sp) of
    Just (ent, ent_id) -> do
      ts <- timestampH'
      let
        like = (likeRequestToLike user_id ent ent_id like_request) { likeCreatedAt = Just ts }

      insertEntityDbE like

    _ -> left $ Error_InvalidArguments "ent, ent_id"



getLikeM :: UserId -> LikeId -> LN.HandlerErrorEff (Entity Like)
getLikeM user_id like_id = do
  selectFirstDbE [LikeId ==. like_id, LikeUserId ==. user_id , LikeActive ==. True] []



getLike_ByThreadPostM :: UserId -> Entity ThreadPost -> LN.HandlerErrorEff (Entity Like)
getLike_ByThreadPostM user_id (Entity thread_post_id _) = getLike_ByThreadPostIdM user_id thread_post_id



getLike_ByThreadPostIdM :: UserId -> ThreadPostId -> LN.HandlerErrorEff (Entity Like)
getLike_ByThreadPostIdM user_id thread_post_id = do
  selectFirstDbE [LikeUserId ==. user_id, LikeEnt ==. Ent_ThreadPost, LikeEntId ==. thread_post_id', LikeActive ==. True ] []
  where
  thread_post_id' = keyToInt64 thread_post_id



updateLikeM :: UserId -> LikeId -> LikeRequest -> LN.HandlerErrorEff (Entity Like)
updateLikeM user_id like_id LikeRequest{..} = do

  ts <- timestampH'

  void $ updateWhereDb
    [ LikeId ==. like_id, LikeUserId ==. user_id ]

    [ LikeModifiedAt =. Just ts
    , LikeOpt        =. likeRequestOpt
    , LikeReason     =. likeRequestReason
    , LikeScore      =. likeOptToScore likeRequestOpt
    ]

  selectFirstDbE [LikeId ==. like_id, LikeActive ==. True] []



deleteLikeM :: UserId -> LikeId -> LN.HandlerErrorEff ()
deleteLikeM user_id like_id = do
  deleteWhereDbE [LikeUserId ==. user_id, LikeId ==. like_id, LikeActive ==. True]



getLikeStatsM :: Maybe StandardParams -> UserId -> LN.HandlerErrorEff LikeStatResponses
getLikeStatsM _ _ = left Error_NotImplemented




getLikeStatM :: Maybe StandardParams -> UserId -> LikeId -> LN.HandlerErrorEff LikeStatResponse
getLikeStatM m_sp user_id _ = do

  case (lookupSpMay m_sp spThreadPostId) of
    Just thread_post_id -> getLikeStat_ByThreadPostIdM user_id thread_post_id
    _                   -> left $ Error_InvalidArguments "thread_post_id"



getLikeStat_ByThreadPostIdM :: UserId -> ThreadPostId -> LN.HandlerErrorEff LikeStatResponse
getLikeStat_ByThreadPostIdM user_id thread_post_id = do

  likes <- selectListDb Nothing [LikeEnt ==. Ent_ThreadPost, LikeEntId ==. i64, LikeActive ==. True] [] LikeId
  let
    opts   = map (\(Entity _ Like{..}) -> likeOpt) likes
    scores = map (\(Entity _ Like{..}) -> likeScore) likes

  right $ LikeStatResponse {
    likeStatResponseEnt     = Ent_ThreadPost,
    likeStatResponseEntId   = i64,
    likeStatResponseScore   = fromIntegral $ sum scores,
    likeStatResponseLike    = fromIntegral $ length $ filter (==L.Like) opts,
    likeStatResponseDislike = fromIntegral $ length $ filter (==L.Dislike) opts
  }
  where
  i64 = keyToInt64 thread_post_id
