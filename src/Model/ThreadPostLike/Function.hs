{-# LANGUAGE RecordWildCards #-}

module Model.ThreadPostLike.Function (
  threadPostLikeRequestToThreadPostLike,
  threadPostLikeToResponse,
  threadPostLikesToResponses
) where



import           Model.Prelude



threadPostLikeRequestToThreadPostLike :: UserId -> ThreadPostId -> ThreadPostLikeRequest -> ThreadPostLike
threadPostLikeRequestToThreadPostLike user_id thread_post_id ThreadPostLikeRequest{..} = ThreadPostLike {
  threadPostLikeUserId = user_id,
  threadPostLikeThreadPostId = thread_post_id,
  threadPostLikeOpt = threadPostLikeRequestOpt,
  threadPostLikeScore = likeOptToScore threadPostLikeRequestOpt,
  threadPostLikeReason = threadPostLikeRequestReason,
  threadPostLikeActive = True,
  threadPostLikeCreatedAt = Nothing,
  threadPostLikeModifiedAt = Nothing
}



threadPostLikeToResponse :: Entity ThreadPostLike -> ThreadPostLikeResponse
threadPostLikeToResponse (Entity thread_post_threadPostLike_id ThreadPostLike{..}) = ThreadPostLikeResponse {
  threadPostLikeResponseId = keyToInt64 thread_post_threadPostLike_id,
  threadPostLikeResponseUserId = keyToInt64 threadPostLikeUserId,
  threadPostLikeResponseOpt = threadPostLikeOpt,
  threadPostLikeResponseScore = threadPostLikeScore,
  threadPostLikeResponseReason = threadPostLikeReason,
  threadPostLikeResponseThreadPostId = keyToInt64 threadPostLikeThreadPostId,
  threadPostLikeResponseCreatedAt = threadPostLikeCreatedAt,
  threadPostLikeResponseModifiedAt = threadPostLikeModifiedAt
}



threadPostLikesToResponses :: [Entity ThreadPostLike] -> ThreadPostLikeResponses
threadPostLikesToResponses thread_posts_threadPostLikes = ThreadPostLikeResponses {
  threadPostLikeResponses = map threadPostLikeToResponse thread_posts_threadPostLikes
}
