{-# LANGUAGE RecordWildCards #-}

module Model.ResourceLike.Function (
  resourceLikeRequestToResourceLike,
  resourceLikeToResponse,
  resourceLikesToResponses
) where



import           Model.Prelude



resourceLikeRequestToResourceLike :: UserId -> ResourceId -> ResourceLikeRequest -> ResourceLike
resourceLikeRequestToResourceLike user_id thread_post_id ResourceLikeRequest{..} = ResourceLike {
  resourceLikeUserId = user_id,
  resourceLikeResourceId = thread_post_id,
  resourceLikeOpt = resourceLikeRequestOpt,
  resourceLikeScore = likeOptToScore resourceLikeRequestOpt,
  resourceLikeReason = resourceLikeRequestReason,
  resourceLikeActive = True,
  resourceLikeCreatedAt = Nothing,
  resourceLikeModifiedAt = Nothing
}



resourceLikeToResponse :: Entity ResourceLike -> ResourceLikeResponse
resourceLikeToResponse (Entity thread_post_resourceLike_id ResourceLike{..}) = ResourceLikeResponse {
  resourceLikeResponseId = keyToInt64 thread_post_resourceLike_id,
  resourceLikeResponseUserId = keyToInt64 resourceLikeUserId,
  resourceLikeResponseOpt = resourceLikeOpt,
  resourceLikeResponseScore = resourceLikeScore,
  resourceLikeResponseReason = resourceLikeReason,
  resourceLikeResponseResourceId = keyToInt64 resourceLikeResourceId,
  resourceLikeResponseCreatedAt = resourceLikeCreatedAt,
  resourceLikeResponseModifiedAt = resourceLikeModifiedAt
}



resourceLikesToResponses :: [Entity ResourceLike] -> ResourceLikeResponses
resourceLikesToResponses thread_posts_resourceLikes = ResourceLikeResponses {
  resourceLikeResponses = map resourceLikeToResponse thread_posts_resourceLikes
}
