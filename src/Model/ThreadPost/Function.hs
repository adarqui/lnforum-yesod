{-# LANGUAGE RecordWildCards #-}

module Model.ThreadPost.Function (
  threadPostRequestToThreadPost,
  threadPostToResponse,
  threadPostsToResponses,
) where



import           Model.Prelude



threadPostRequestToThreadPost :: UserId -> ThreadId -> Maybe ThreadPostId -> ThreadPostRequest -> ThreadPost
threadPostRequestToThreadPost user_id thread_id _ ThreadPostRequest{..} = ThreadPost {
  threadPostUserId      = user_id,
  threadPostThreadId    = thread_id,
  threadPostParentId    = Nothing,
  threadPostTitle       = threadPostRequestTitle,
  threadPostBody        = encodeText threadPostRequestBody,
  threadPostTags        = threadPostRequestTags,
  threadPostPrivateTags = threadPostRequestPrivateTags,
  threadPostActive      = True,
  threadPostGuard       = threadPostRequestGuard,
  threadPostCreatedAt   = Nothing,
  threadPostModifiedBy  = Nothing,
  threadPostModifiedAt  = Nothing,
  threadPostActivityAt  = Nothing
}



threadPostToResponse :: Entity ThreadPost -> ThreadPostResponse
threadPostToResponse (Entity thread_post_id ThreadPost{..}) = ThreadPostResponse {
  threadPostResponseId          = keyToInt64 thread_post_id,
  threadPostResponseUserId      = keyToInt64 threadPostUserId,
  threadPostResponseThreadId    = keyToInt64 threadPostThreadId,
  threadPostResponseParentId    = fmap keyToInt64 threadPostParentId,
  threadPostResponseTitle       = threadPostTitle,
  threadPostResponseBody        = maybe PostDataEmpty id $ decodeText threadPostBody,
  threadPostResponseTags        = threadPostTags,
  threadPostResponsePrivateTags = threadPostPrivateTags,
  threadPostResponseActive      = threadPostActive,
  threadPostResponseGuard       = threadPostGuard,
  threadPostResponseCreatedAt   = threadPostCreatedAt,
  threadPostResponseModifiedBy  = fmap keyToInt64 threadPostModifiedBy,
  threadPostResponseModifiedAt  = threadPostModifiedAt,
  threadPostResponseActivityAt  = threadPostActivityAt
}



threadPostsToResponses :: [Entity ThreadPost] -> ThreadPostResponses
threadPostsToResponses thread_posts = ThreadPostResponses {
  threadPostResponses = map threadPostToResponse thread_posts
}
