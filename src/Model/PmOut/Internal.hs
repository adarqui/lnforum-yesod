{-# LANGUAGE RecordWildCards #-}

module Model.PmOut.Internal (
  getPmOutsM,
  getPmOutM,
  insertPmOutM,
  updatePmOutM,
  deletePmOutM
) where



import           Model.Prelude
import           Model.PmOut.Function



getPmOutsM :: UserId -> Handler [Entity PmOut]
getPmOutsM user_id = do
  selectListDb' [ PmOutUserId ==. user_id ] [] PmOutId



getPmOutM :: UserId -> PmOutId -> Handler (Entity PmOut)
getPmOutM user_id pmOut_id = do
  notFoundMaybe =<< selectFirstDb [ PmOutUserId ==. user_id, PmOutId ==. pmOut_id ] []



insertPmOutM :: UserId -> PmId -> PmOutRequest -> Handler (Entity PmOut)
insertPmOutM user_id pm_id pmOut_request = do

  ts <- timestampH'

  let
    pmOut = (pmOutRequestToPmOut user_id pm_id pmOut_request) { pmOutCreatedAt = Just ts }

  insertEntityDb pmOut



updatePmOutM :: UserId -> PmOutId -> PmOutRequest -> Handler (Entity PmOut)
updatePmOutM user_id pmOut_id pmOut_request = do

  ts <- timestampH'

  let
    PmOut{..} = (pmOutRequestToPmOut user_id dummyId pmOut_request) { pmOutModifiedAt = Just ts }

  updateWhereDb
    [ PmOutUserId ==. user_id, PmOutId ==. pmOut_id ]
    [ PmOutModifiedAt =. pmOutModifiedAt
    , PmOutLabel =. pmOutLabel
    ]

  notFoundMaybe =<< selectFirstDb [ PmOutUserId ==. user_id, PmOutId ==. pmOut_id ] []



deletePmOutM :: UserId -> PmOutId -> Handler ()
deletePmOutM _ _ = do
  return ()
--  deleteWhereDb [ PmOutUserId ==. user_id, PmOutId ==. pmOut_id ]
