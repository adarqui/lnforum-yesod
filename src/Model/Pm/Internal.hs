{-# LANGUAGE RecordWildCards #-}

module Model.Pm.Internal (
  getPmsM,
  getPmM,
  insertPmM,
  updatePmM,
  deletePmM
) where



import           Model.Prelude
import           Model.Pm.Function



getPmsM :: UserId -> Handler [Entity Pm]
getPmsM user_id = do
  selectListDb' [ PmUserId ==. user_id ] [] PmId



getPmM :: UserId -> PmId -> Handler (Entity Pm)
getPmM user_id pm_id = do
  notFoundMaybe =<< selectFirstDb [ PmUserId ==. user_id, PmId ==. pm_id ] []



insertPmM :: UserId -> UserId -> PmRequest -> Handler (Entity Pm)
insertPmM user_id to_user_id pm_request = do

  ts <- timestampH'

  let
    pm = (pmRequestToPm user_id to_user_id pm_request) { pmCreatedAt = Just ts }

  insertEntityDb pm



updatePmM :: UserId -> PmId -> PmRequest -> Handler (Entity Pm)
updatePmM user_id pm_id pm_request = do

  ts <- timestampH'

  let
    Pm{..} = (pmRequestToPm user_id dummyId pm_request) { pmModifiedAt = Just ts }

  updateWhereDb
    [ PmUserId ==. user_id, PmId ==. pm_id ]
    [ PmModifiedAt =. pmModifiedAt
    , PmSubject =. pmSubject
    , PmBody =. pmBody
    ]

  notFoundMaybe =<< selectFirstDb [ PmUserId ==. user_id, PmId ==. pm_id ] []



deletePmM :: UserId -> PmId -> Handler ()
deletePmM _ _ = do
  return ()
--  deleteWhereDb [ PmUserId ==. user_id, PmId ==. pm_id ]
