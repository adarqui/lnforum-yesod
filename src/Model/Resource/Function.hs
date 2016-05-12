{-# LANGUAGE RecordWildCards #-}

module Model.Resource.Function (
  resourceRequestToResource,
  resourceToResponse,
  resourcesToResponses,
) where



import           Import
import           LN.T
import           Misc.Codec (decodeText, encodeText, keyToInt64)



resourceRequestToResource :: UserId -> ResourceRequest -> Resource
resourceRequestToResource user_id ResourceRequest{..} = Resource {
  resourceUserId = user_id,
  resourceTitle = resourceRequestTitle,
  resourceDescription = resourceRequestDescription,
  resourceSource = encodeText resourceRequestSource,
  resourceAuthor = resourceRequestAuthor,
  resourcePrerequisites = [], -- resourceRequestPrerequisites,
  resourceCategories = [], -- resourceRequestCategories,
  resourceVisibility = encodeText Public, -- resourceRequestVisibility,
  resourceCounter = resourceRequestCounter,
  resourceVersion = resourceRequestVersion,
  resourceUrls = resourceRequestUrls,
  resourceActive = True,
  resourceCreatedAt = Nothing,
  resourceModifiedAt = Nothing
}



resourceToResponse :: Entity Resource -> ResourceResponse
resourceToResponse (Entity resource_id Resource{..}) = ResourceResponse {
  resourceResponseId = keyToInt64 resource_id,
  resourceResponseUserId = keyToInt64 resourceUserId,
  resourceResponseTitle = resourceTitle,
  resourceResponseDescription = resourceDescription,
  resourceResponseSource = maybe SourceNone id (decodeText resourceSource),
  resourceResponseAuthor = resourceAuthor,
  resourceResponsePrerequisites = map (\preq -> maybe [] id (decodeText preq)) resourcePrerequisites,
  resourceResponseCategories = map (\cat -> maybe [] id (decodeText cat)) resourceCategories,
  resourceResponseVisibility = maybe Public id (decodeText resourceVisibility),
  resourceResponseCounter = resourceCounter,
  resourceResponseVersion = resourceVersion,
  resourceResponseUrls = resourceUrls,
  resourceResponseCreatedAt = resourceCreatedAt,
  resourceResponseModifiedAt = resourceModifiedAt
}



resourcesToResponses :: [Entity Resource] -> ResourceResponses
resourcesToResponses resources = ResourceResponses {
  resourceResponses = map resourceToResponse resources
}
