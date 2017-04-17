{-# LANGUAGE RecordWildCards #-}

module LN.All.Resource (
  -- Handler
  getResourcesR,
  postResourcesR,
  getResourceR,
  putResourceR,
  deleteResourceR,
  getResourcesCountR,
  getResourceStatsR,
  getResourceStatR,

  -- Model/Function
  resourceRequestToResource,
  resourceToResponse,
  resourcesToResponses,

  -- Model/Internal
  getResourcesM,
  getResources_ByEverythingM,
  getResources_ByUserIdM,
  getResources_ParameterizedM,
  getResourceM,
  insertResourceM,
  updateResourceM,
  deleteResourceM,
  countResourcesM,
  getResourceStatsM,
  getResourceStatM
) where



import Control.Monad.Trans.State
import qualified Control.Monad.Trans.State as StateT

import           LN.All.Prelude
import qualified LN.T.Like      as L



--
-- Handler
--

getResourcesR :: Handler Value
getResourcesR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON resourcesToResponses $ getResourcesM (pure sp) user_id



postResourcesR :: Handler Value
postResourcesR = run $ do
  user_id          <- _requireAuthId
  resource_request <- requireJsonBody
  errorOrJSON resourceToResponse $ insertResourceM user_id resource_request



getResourceR :: ResourceId -> Handler Value
getResourceR resource_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON resourceToResponse $ getResourceM user_id resource_id



putResourceR :: ResourceId -> Handler Value
putResourceR resource_id = run $ do
  user_id          <- _requireAuthId
  resource_request <- requireJsonBody
  errorOrJSON resourceToResponse $ updateResourceM user_id resource_id resource_request



deleteResourceR :: ResourceId -> Handler Value
deleteResourceR resource_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteResourceM user_id resource_id



getResourcesCountR :: Handler Value
getResourcesCountR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ countResourcesM (pure sp) user_id



getResourceStatsR :: Handler Value
getResourceStatsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getResourceStatsM (pure sp) user_id



getResourceStatR :: ResourceId -> Handler Value
getResourceStatR thread_post_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getResourceStatM user_id thread_post_id







--
-- Model/Function
--

resourceRequestToResource :: UserId -> ResourceRequest -> Resource
resourceRequestToResource user_id ResourceRequest{..} = Resource {
  resourceUserId         = user_id,
  resourceName           = toSafeUrl resourceRequestDisplayName,
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








--
-- Model/Internal
--

getResourcesM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Resource]
getResourcesM m_sp user_id = getResources_ParameterizedM m_sp user_id

{-
  case (lookupSpMay m_sp spUserId) of

    Just lookup_user_id -> getResources_ByUserIdM m_sp user_id lookup_user_id
    _                   -> getResources_ByEverythingM m_sp user_id
    -}



getResources_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Resource]
getResources_ByEverythingM m_sp _ = do
  selectListDbE m_sp [ResourceActive ==. True] [] ResourceId



getResources_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity Resource]
getResources_ByUserIdM m_sp _ lookup_user_id = do
  selectListDbE m_sp [ResourceUserId ==. lookup_user_id, ResourceActive ==. True] [] ResourceId



getResources_ParameterizedM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Resource]
getResources_ParameterizedM m_sp _ = do
  evalStateT go []
  where
  go = do
    case lookupSpMay m_sp spUserId of
      Just lookup_user_id -> modify (\st->st <> [ResourceUserId ==. lookup_user_id])
      _ -> pure ()
    params <- ([ResourceActive ==. True] <>) <$> StateT.get
    lift $ selectListDbE m_sp params [] ResourceId



getResourceM :: UserId -> ResourceId -> HandlerErrorEff (Entity Resource)
getResourceM _ resource_id = do
  selectFirstDbE [ResourceId ==. resource_id, ResourceActive ==. True] []



insertResourceM :: UserId -> ResourceRequest -> HandlerErrorEff (Entity Resource)
insertResourceM user_id resource_request = do

  ts <- timestampH'

  let
    resource = (resourceRequestToResource user_id resource_request) { resourceCreatedAt = Just ts }

  insertEntityDbE resource



updateResourceM :: UserId -> ResourceId -> ResourceRequest -> HandlerErrorEff (Entity Resource)
updateResourceM user_id resource_id resource_request = do

  ts <- timestampH'

  let
    Resource{..} = (resourceRequestToResource user_id resource_request) { resourceModifiedAt = Just ts }

  updateWhereDb
    [ ResourceUserId ==. user_id, ResourceId ==. resource_id, ResourceActive ==. True ]
    [ ResourceModifiedAt    =. resourceModifiedAt
    , ResourceName          =. resourceName
    , ResourceDisplayName   =. resourceDisplayName
    , ResourceDescription   =. resourceDescription
    , ResourceSource        =. resourceSource
    , ResourceAuthor        =. resourceAuthor
    , ResourcePrerequisites =. resourcePrerequisites
    , ResourceCategories    =. resourceCategories
    , ResourceVisibility    =. resourceVisibility
    , ResourceCounter       =. resourceCounter
    , ResourceVersion       =. resourceVersion
    , ResourceUrls          =. resourceUrls
    , ResourceIcon          =. resourceIcon
    , ResourceTags          =. resourceTags
    , ResourceGuard        +=. 1
    ]

  selectFirstDbE [ResourceUserId ==. user_id, ResourceId ==. resource_id, ResourceActive ==. True] []



deleteResourceM :: UserId -> ResourceId -> HandlerErrorEff ()
deleteResourceM user_id resource_id = do
  deleteWhereDbE [ResourceUserId ==. user_id, ResourceId ==. resource_id, ResourceActive ==. True]



countResourcesM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countResourcesM m_sp _ = do

  case (lookupSpMay m_sp spUserId, lookupSpMay m_sp spUserIds) of

    -- TODO FIXME: not handling argument properly
    _ -> do
      n <- countDb [ResourceActive ==. True]
      rightA $ CountResponses [CountResponse 0 (fromIntegral n)]



getResourceStatsM :: Maybe StandardParams -> UserId -> HandlerErrorEff ResourceStatResponse
getResourceStatsM _ _ = leftA Error_NotImplemented



getResourceStatM :: UserId -> ResourceId -> HandlerErrorEff ResourceStatResponse
getResourceStatM _ resource_id = do

  -- leuron counts
  leuron_count <- countDb [LeuronResourceId ==. resource_id, LeuronActive ==. True]

  rightA $ ResourceStatResponse {
    resourceStatResponseResourceId = keyToInt64 resource_id,
    resourceStatResponseLeurons    = fromIntegral leuron_count,
    resourceStatResponseLikes      = 0,
    resourceStatResponseNeutral    = 0,
    resourceStatResponseDislikes   = 0,
    resourceStatResponseStars      = 0,
    resourceStatResponseViews      = 0
  }
