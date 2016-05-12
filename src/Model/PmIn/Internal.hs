{-# LANGUAGE RecordWildCards #-}

module Model.PmIn.Internal (
  getPmInsM,
  getPmInM,
  insertPmInM,
  updatePmInM,
  deletePmInM
) where



import           Model.Prelude
import           Model.PmIn.Function



getPmInsM :: UserId -> Handler [Entity PmIn]
getPmInsM user_id = do
  selectListDb' [ PmInUserId ==. user_id ] [] PmInId



getPmInM :: UserId -> PmInId -> Handler (Entity PmIn)
getPmInM user_id pmIn_id = do
  notFoundMaybe =<< selectFirstDb [ PmInUserId ==. user_id, PmInId ==. pmIn_id ] []



insertPmInM :: UserId -> PmId -> PmInRequest -> Handler (Entity PmIn)
insertPmInM user_id pm_id pmIn_request = do

  ts <- timestampH'

  let
    pmIn = (pmInRequestToPmIn user_id pm_id pmIn_request) { pmInCreatedAt = Just ts }

  insertEntityDb pmIn



updatePmInM :: UserId -> PmInId -> PmInRequest -> Handler (Entity PmIn)
updatePmInM user_id pmIn_id pmIn_request = do

  ts <- timestampH'

  let
    PmIn{..} = (pmInRequestToPmIn user_id dummyId pmIn_request) { pmInModifiedAt = Just ts }

  updateWhereDb
    [ PmInUserId ==. user_id, PmInId ==. pmIn_id ]
    [ PmInModifiedAt =. pmInModifiedAt
    , PmInLabel =. pmInLabel
    , PmInIsRead =. pmInIsRead
    , PmInIsStarred =. pmInIsStarred
    ]

  notFoundMaybe =<< selectFirstDb [ PmInUserId ==. user_id, PmInId ==. pmIn_id ] []



deletePmInM :: UserId -> PmInId -> Handler ()
deletePmInM _ _ = do
  return ()
--  deleteWhereDb [ PmInUserId ==. user_id, PmInId ==. pmIn_id ]
