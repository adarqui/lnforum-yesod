{-# LANGUAGE RecordWildCards #-}

module Model.Resource.Internal (
  getResourcesM,
  getResourcesBy_EverythingM,
  getResourcesBy_UserIdM,
  getResourceM,
  insertResourceM,
  updateResourceM,
  deleteResourceM,
  countResourcesM,
  getResourceStatsM,
  getResourceStatM
) where



import qualified LN.T.Like               as L
import           Model.Prelude
import           Model.Resource.Function



getResourcesM :: UserId -> Handler [Entity Resource]
getResourcesM user_id = do
  sp@StandardParams{..} <- lookupStandardParams

  case spUserId of

    Just lookup_user_id -> getResourcesBy_UserIdM user_id lookup_user_id sp

    _                   -> getResourcesBy_EverythingM user_id sp

--    (_, Just resource_ids)   -> getResourceBy_ResourceIds user_id resource_ids sp



getResourcesBy_EverythingM :: UserId -> StandardParams -> Handler [Entity Resource]
getResourcesBy_EverythingM _ sp = do
  selectListDb sp [] [] ResourceId



getResourcesBy_UserIdM :: UserId -> UserId -> StandardParams -> Handler [Entity Resource]
getResourcesBy_UserIdM user_id lookup_user_id sp = do
  selectListDb sp [ ResourceUserId ==. lookup_user_id ] [] ResourceId



getResourceM :: UserId -> ResourceId -> Handler (Entity Resource)
getResourceM _ resource_id = do
  notFoundMaybe =<< selectFirstDb [ ResourceId ==. resource_id ] []



insertResourceM :: UserId -> ResourceRequest -> Handler (Entity Resource)
insertResourceM user_id resource_request = do

  ts <- timestampH'

  let
    resource = (resourceRequestToResource user_id resource_request) { resourceCreatedAt = Just ts }

  insertEntityDb resource



updateResourceM :: UserId -> ResourceId -> ResourceRequest -> Handler (Entity Resource)
updateResourceM user_id resource_id resource_request = do

  ts <- timestampH'

  let
    Resource{..} = (resourceRequestToResource user_id resource_request) { resourceModifiedAt = Just ts }

  updateWhereDb
    [ ResourceUserId ==. user_id, ResourceId ==. resource_id ]
    [ ResourceModifiedAt =. resourceModifiedAt
    , ResourceTitle =. resourceTitle
    , ResourceDescription =. resourceDescription
    , ResourceSource =. resourceSource
    , ResourceAuthor =. resourceAuthor
    , ResourcePrerequisites =. resourcePrerequisites
    , ResourceCategories =. resourceCategories
    , ResourceVisibility =. resourceVisibility
    , ResourceCounter =. resourceCounter
    , ResourceVersion =. resourceVersion
    , ResourceUrls =. resourceUrls
    ]

  notFoundMaybe =<< selectFirstDb [ ResourceUserId ==. user_id, ResourceId ==. resource_id ] []



deleteResourceM :: UserId -> ResourceId -> Handler ()
deleteResourceM user_id resource_id = do
  deleteWhereDb [ ResourceUserId ==. user_id, ResourceId ==. resource_id ]



countResourcesM :: UserId -> Handler CountResponses
countResourcesM _ = do

  StandardParams{..} <- lookupStandardParams

  case (spUserId, spUserIds) of

    (_, _) -> do
      n <- countDb [ ResourceActive ==. True ]
      return $ CountResponses [CountResponse 0 (fromIntegral n)]



getResourceStatsM :: UserId -> Handler ResourceStatResponse
getResourceStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spThreadId of

    Just _  -> notFound
    Nothing -> notFound



getResourceStatM :: UserId -> ResourceId -> Handler ResourceStatResponse
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
