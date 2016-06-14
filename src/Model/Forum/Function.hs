{-# LANGUAGE RecordWildCards #-}

module Model.Forum.Function (
  forumRequestToForum,
  forumToResponse,
  forumsToResponses,
) where



import           Model.Prelude




forumRequestToForum :: UserId -> OrganizationId -> ForumRequest -> Forum
forumRequestToForum user_id org_id ForumRequest{..} = Forum {
  forumUserId               = user_id,
  forumOrgId                = org_id,
  forumName                 = toPrettyUrl forumRequestDisplayName,
  forumDisplayName          = forumRequestDisplayName,
  forumDescription          = forumRequestDescription,
  forumThreadsPerBoard      = forumRequestThreadsPerBoard,
  forumThreadPostsPerThread = forumRequestThreadPostsPerThread,
  forumIcon                 = forumRequestIcon,
  forumTags                 = forumRequestTags,
  forumVisibility           = forumRequestVisibility,
  forumActive               = True,
  forumGuard                = forumRequestGuard,
  forumCreatedAt            = Nothing,
  forumModifiedBy           = Nothing,
  forumModifiedAt           = Nothing,
  forumActivityAt           = Nothing
}



forumToResponse :: Entity Forum -> ForumResponse
forumToResponse (Entity forum_id Forum{..}) = ForumResponse {
  forumResponseUserId               = keyToInt64 forumUserId,
  forumResponseId                   = keyToInt64 forum_id,
  forumResponseOrgId                = keyToInt64 forumOrgId,
  forumResponseName                 = forumName,
  forumResponseDisplayName          = forumDisplayName,
  forumResponseDescription          = forumDescription,
  forumResponseThreadsPerBoard      = forumThreadsPerBoard,
  forumResponseThreadPostsPerThread = forumThreadPostsPerThread,
  forumResponseIcon                 = forumIcon,
  forumResponseTags                 = forumTags,
  forumResponseVisibility           = forumVisibility,
  forumResponseActive               = forumActive,
  forumResponseGuard                = forumGuard,
  forumResponseCreatedAt            = forumCreatedAt,
  forumResponseModifiedBy           = fmap keyToInt64 forumModifiedBy,
  forumResponseModifiedAt           = forumModifiedAt,
  forumResponseActivityAt           = forumActivityAt
}



forumsToResponses :: [Entity Forum] -> ForumResponses
forumsToResponses forums = ForumResponses {
  forumResponses = map forumToResponse forums
}
