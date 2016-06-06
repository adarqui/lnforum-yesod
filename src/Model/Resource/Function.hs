{-# LANGUAGE RecordWildCards #-}

module Model.Resource.Function (
  resourceRequestToResource,
  resourceToResponse,
  resourcesToResponses,
) where



import           Import
import           LN.Lib.Url (toPrettyUrl)
import           LN.T
import           Misc.Codec (decodeText, encodeText, keyToInt64)



resourceRequestToResource :: UserId -> ResourceRequest -> Resource
resourceRequestToResource user_id ResourceRequest{..} = Resource {
  resourceUserId         = user_id,
  resourceName           = toPrettyUrl resourceRequestDisplayName,
  resourceDisplayName    = resourceRequestDisplayName,
  resourceDescription    = resourceRequestDescription,
  resourceSource         = encodeText resourceRequestSource,
  resourceAuthor         = resourceRequestAuthor,
  resourcePrerequisites  = [], -- resourceRequestPrerequisites,
  resourceCategories     = [], -- resourceRequestCategories,
  resourceVisibility     = resourceRequestVisibility,
  resourceCounter        = resourceRequestCounter,
  resourceVersion        = resourceRequestVersion,
  resourceUrls           = resourceRequestUrls,
  resourceIcon           = resourceRequestIcon,
  resourceTags           = resourceRequestTags,
  resourceActive         = True,
  resourceGuard          = resourceRequestGuard,
  resourceCreatedAt      = Nothing,
  resourceModifiedAt     = Nothing,
  resourceActivityAt     = Nothing
}



resourceToResponse :: Entity Resource -> ResourceResponse
resourceToResponse (Entity resource_id Resource{..}) = ResourceResponse {
  resourceResponseId            = keyToInt64 resource_id,
  resourceResponseUserId        = keyToInt64 resourceUserId,
  resourceResponseName          = resourceName,
  resourceResponseDisplayName   = resourceDisplayName,
  resourceResponseDescription   = resourceDescription,
  resourceResponseSource        = maybe SourceNone id (decodeText resourceSource),
  resourceResponseAuthor        = resourceAuthor,
  resourceResponsePrerequisites = map (\preq -> maybe [] id (decodeText preq)) resourcePrerequisites,
  resourceResponseCategories    = map (\cat -> maybe [] id (decodeText cat)) resourceCategories,
  resourceResponseVisibility    = resourceVisibility,
  resourceResponseCounter       = resourceCounter,
  resourceResponseVersion       = resourceVersion,
  resourceResponseUrls          = resourceUrls,
  resourceResponseIcon          = resourceIcon,
  resourceResponseTags          = resourceTags,
  resourceResponseActive        = resourceActive,
  resourceResponseGuard         = resourceGuard,
  resourceResponseCreatedAt     = resourceCreatedAt,
  resourceResponseModifiedAt    = resourceModifiedAt,
  resourceResponseActivityAt    = resourceActivityAt
}



resourcesToResponses :: [Entity Resource] -> ResourceResponses
resourcesToResponses resources = ResourceResponses {
  resourceResponses = map resourceToResponse resources
}
