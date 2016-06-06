{-# LANGUAGE RecordWildCards #-}

module Model.Like.Function (
  likeRequestToLike,
  likeToResponse,
  likesToResponses
) where



import           Model.Prelude



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
