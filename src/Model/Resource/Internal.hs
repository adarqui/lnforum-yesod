{-# LANGUAGE RecordWildCards #-}

module Model.Resource.Internal (
  getResourcesM,
  getResourceM,
  insertResourceM,
  updateResourceM,
  deleteResourceM,
) where



import           Model.Prelude
import           Model.Resource.Function



getResourcesM :: UserId -> Handler [Entity Resource]
getResourcesM user_id = do
  sp <- lookupStandardParams
  getResourcesBy_EverythingM user_id sp



getResourcesBy_EverythingM :: UserId -> StandardParams -> Handler [Entity Resource]
getResourcesBy_EverythingM _ sp = do
  selectListDb sp [] [] ResourceId



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
