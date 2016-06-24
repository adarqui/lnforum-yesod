{-# LANGUAGE RecordWildCards #-}

module All.Resource (
  -- Handler
  getResourcesR,
  postResourceR0,
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
  getResourceM,
  insertResourceM,
  updateResourceM,
  deleteResourceM,
  countResourcesM,
  getResourceStatsM,
  getResourceStatM
) where



import           All.Prelude
import           Import
import           LN.Lib.Url        (toPrettyUrl)
import           LN.T              hiding (LikeOpt(..))
import qualified LN.T.Like         as L
import           Misc.Codec        (decodeText, encodeText, keyToInt64)



--
-- Handler
--

getResourcesR :: Handler Value
getResourcesR = run $ do
  user_id <- _requireAuthId
  (toJSON . resourcesToResponses) <$> getResourcesM user_id



postResourceR0 :: Handler Value
postResourceR0 = run $ do
  user_id <- _requireAuthId
  resource_request <- requireJsonBody :: HandlerEff ResourceRequest
  (toJSON . resourceToResponse) <$> insertResourceM user_id resource_request



getResourceR :: ResourceId -> Handler Value
getResourceR resource_id = run $ do
  user_id <- _requireAuthId
  (toJSON . resourceToResponse) <$> getResourceM user_id resource_id



putResourceR :: ResourceId -> Handler Value
putResourceR resource_id = run $ do
  user_id <- _requireAuthId
  resource_request <- requireJsonBody
  (toJSON . resourceToResponse) <$> updateResourceM user_id resource_id resource_request



deleteResourceR :: ResourceId -> Handler Value
deleteResourceR resource_id = run $ do
  user_id <- _requireAuthId
  void $ deleteResourceM user_id resource_id
  pure $ toJSON ()



getResourcesCountR :: Handler Value
getResourcesCountR = run $ do
  user_id <- _requireAuthId
  toJSON <$> countResourcesM user_id



getResourceStatsR :: Handler Value
getResourceStatsR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getResourceStatsM user_id



getResourceStatR :: ResourceId -> Handler Value
getResourceStatR thread_post_id = run $ do
  user_id <- _requireAuthId
  toJSON <$> getResourceStatM user_id thread_post_id







--
-- Model/Function
--

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








--
-- Model/Internal
--

getResourcesM :: UserId -> HandlerEff [Entity Resource]
getResourcesM user_id = do
  sp@StandardParams{..} <- lookupStandardParams

  case spUserId of

    Just lookup_user_id -> getResources_ByUserIdM user_id lookup_user_id sp

    _                   -> getResources_ByEverythingM user_id sp

--    (_, Just resource_ids)   -> getResource_ByResourceIds user_id resource_ids sp



getResources_ByEverythingM :: UserId -> StandardParams -> HandlerEff [Entity Resource]
getResources_ByEverythingM _ sp = do
  selectListDb sp [] [] ResourceId



getResources_ByUserIdM :: UserId -> UserId -> StandardParams -> HandlerEff [Entity Resource]
getResources_ByUserIdM _ lookup_user_id sp = do
  selectListDb sp [ ResourceUserId ==. lookup_user_id ] [] ResourceId



getResourceM :: UserId -> ResourceId -> HandlerEff (Entity Resource)
getResourceM _ resource_id = do
  notFoundMaybe =<< selectFirstDb [ ResourceId ==. resource_id ] []



insertResourceM :: UserId -> ResourceRequest -> HandlerEff (Entity Resource)
insertResourceM user_id resource_request = do

  ts <- timestampH'

  let
    resource = (resourceRequestToResource user_id resource_request) { resourceCreatedAt = Just ts }

  insertEntityDb resource



updateResourceM :: UserId -> ResourceId -> ResourceRequest -> HandlerEff (Entity Resource)
updateResourceM user_id resource_id resource_request = do

  ts <- timestampH'

  let
    Resource{..} = (resourceRequestToResource user_id resource_request) { resourceModifiedAt = Just ts }

  updateWhereDb
    [ ResourceUserId ==. user_id, ResourceId ==. resource_id ]
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

  notFoundMaybe =<< selectFirstDb [ ResourceUserId ==. user_id, ResourceId ==. resource_id ] []



deleteResourceM :: UserId -> ResourceId -> HandlerEff ()
deleteResourceM user_id resource_id = do
  deleteWhereDb [ ResourceUserId ==. user_id, ResourceId ==. resource_id ]



countResourcesM :: UserId -> HandlerEff CountResponses
countResourcesM _ = do

  StandardParams{..} <- lookupStandardParams

  case (spUserId, spUserIds) of

    (_, _) -> do
      n <- countDb [ ResourceActive ==. True ]
      return $ CountResponses [CountResponse 0 (fromIntegral n)]



getResourceStatsM :: UserId -> HandlerEff ResourceStatResponse
getResourceStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spThreadId of

    Just _  -> notFound
    Nothing -> notFound



getResourceStatM :: UserId -> ResourceId -> HandlerEff ResourceStatResponse
getResourceStatM _ resource_id = do

  -- leuron counts
  leuron_count <- countDb [ LeuronResourceId ==. resource_id ]

  -- get like counts
  likes <- selectListDb defaultStandardParams [ LikeEntId ==. keyToInt64 resource_id ] [] LikeId

  -- get star counts
-- TODO FIXME  stars <- selectListDb defaultStandardParams [ ResourceStarResourceId ==. resource_id ] [] ResourceStarId

  let
    likes_flat = map (\(Entity _ Like{..}) -> likeOpt) likes

  return $ ResourceStatResponse {
    resourceStatResponseResourceId = keyToInt64 resource_id,
    resourceStatResponseLeurons    = fromIntegral leuron_count,
    resourceStatResponseLikes      = fromIntegral $ length $ filter (==L.Like) likes_flat,
    resourceStatResponseNeutral    = fromIntegral $ length $ filter (==L.Neutral) likes_flat,
    resourceStatResponseDislikes   = fromIntegral $ length $ filter (==L.Dislike) likes_flat,
    resourceStatResponseStars      = 0, -- TODO FIXME fromIntegral $ length stars,
    resourceStatResponseViews      = 0
  }
