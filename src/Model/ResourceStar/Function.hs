{-# LANGUAGE RecordWildCards #-}

module Model.ResourceStar.Function (
  resourceStarRequestToResourceStar,
  resourceStarToResponse,
  resourceStarsToResponses
) where



import           Model.Prelude



resourceStarRequestToResourceStar :: UserId -> ResourceId -> ResourceStarRequest -> ResourceStar
resourceStarRequestToResourceStar user_id thread_post_id ResourceStarRequest{..} = ResourceStar {
  resourceStarUserId = user_id,
  resourceStarResourceId = thread_post_id,
  resourceStarReason = resourceStarRequestReason,
  resourceStarActive = True,
  resourceStarCreatedAt = Nothing,
  resourceStarModifiedAt = Nothing
}



resourceStarToResponse :: Entity ResourceStar -> ResourceStarResponse
resourceStarToResponse (Entity thread_post_resourceStar_id ResourceStar{..}) = ResourceStarResponse {
  resourceStarResponseId = keyToInt64 thread_post_resourceStar_id,
  resourceStarResponseUserId = keyToInt64 resourceStarUserId,
  resourceStarResponseReason = resourceStarReason,
  resourceStarResponseResourceId = keyToInt64 resourceStarResourceId,
  resourceStarResponseCreatedAt = resourceStarCreatedAt,
  resourceStarResponseModifiedAt = resourceStarModifiedAt
}



resourceStarsToResponses :: [Entity ResourceStar] -> ResourceStarResponses
resourceStarsToResponses thread_posts_resourceStars = ResourceStarResponses {
  resourceStarResponses = map resourceStarToResponse thread_posts_resourceStars
}
