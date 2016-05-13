{-# LANGUAGE RecordWildCards #-}

module Model.ThreadPostStar.Function (
  threadPostStarRequestToThreadPostStar,
  threadPostStarToResponse,
  threadPostStarsToResponses
) where



import           Model.Prelude



threadPostStarRequestToThreadPostStar :: UserId -> ThreadPostId -> ThreadPostStarRequest -> ThreadPostStar
threadPostStarRequestToThreadPostStar user_id thread_post_id ThreadPostStarRequest{..} = ThreadPostStar {
  threadPostStarUserId = user_id,
  threadPostStarThreadPostId = thread_post_id,
  threadPostStarReason = threadPostStarRequestReason,
  threadPostStarActive = True,
  threadPostStarCreatedAt = Nothing,
  threadPostStarModifiedAt = Nothing
}



threadPostStarToResponse :: Entity ThreadPostStar -> ThreadPostStarResponse
threadPostStarToResponse (Entity thread_post_threadPostStar_id ThreadPostStar{..}) = ThreadPostStarResponse {
  threadPostStarResponseId = keyToInt64 thread_post_threadPostStar_id,
  threadPostStarResponseUserId = keyToInt64 threadPostStarUserId,
  threadPostStarResponseReason = threadPostStarReason,
  threadPostStarResponseThreadPostId = keyToInt64 threadPostStarThreadPostId,
  threadPostStarResponseCreatedAt = threadPostStarCreatedAt,
  threadPostStarResponseModifiedAt = threadPostStarModifiedAt
}



threadPostStarsToResponses :: [Entity ThreadPostStar] -> ThreadPostStarResponses
threadPostStarsToResponses thread_posts_threadPostStars = ThreadPostStarResponses {
  threadPostStarResponses = map threadPostStarToResponse thread_posts_threadPostStars
}
