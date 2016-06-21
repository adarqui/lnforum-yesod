{-# LANGUAGE RecordWildCards #-}

module All.PmOut (
  getPmOutsR,
  postPmOutsR,
  getPmOutR,
  putPmOutR,
  deletePmOutR,

  -- Model/Function
  pmOutRequestToPmOut,
  pmOutToResponse,
  pmOutsToResponses,

  -- Model/Internal
  getPmOutsM,
  getPmOutM,
  insertPmOutM,
  updatePmOutM,
  deletePmOutM
) where



import           Handler.Prelude
import           Model.PmOut



getPmOutsR :: Handler Value
getPmOutsR = do
  user_id <- requireAuthId
  (toJSON . pmOutsToResponses) <$> getPmOutsM user_id



postPmOutsR :: Handler Value
postPmOutsR = do
  user_id <- requireAuthId
  sp <- lookupStandardParams
  case (spPmId sp) of
    Nothing -> notFound
    Just pm_id -> do
      pmOut_request <- requireJsonBody :: Handler PmOutRequest
      (toJSON . pmOutToResponse) <$> insertPmOutM user_id pm_id pmOut_request



getPmOutR :: PmOutId -> Handler Value
getPmOutR pmOut_id = do
  user_id <- requireAuthId
  (toJSON . pmOutToResponse) <$> getPmOutM user_id pmOut_id



putPmOutR :: PmOutId -> Handler Value
putPmOutR pmOut_id = do
  user_id <- requireAuthId
  pmOut_request <- requireJsonBody
  (toJSON . pmOutToResponse) <$> updatePmOutM user_id pmOut_id pmOut_request



deletePmOutR :: PmOutId -> Handler Value
deletePmOutR pmOut_id = do
  user_id <- requireAuthId
  void $ deletePmOutM user_id pmOut_id
  pure $ toJSON ()





--
-- Model/Function
--

pmOutRequestToPmOut :: UserId -> PmId -> PmOutRequest -> PmOut
pmOutRequestToPmOut user_id pm_id PmOutRequest{..} = PmOut {
  pmOutPmId       = pm_id,
  pmOutUserId     = user_id,
  pmOutLabel      = pmOutRequestLabel,
  pmOutIsSaved    = True,
  pmOutActive     = True,
  pmOutGuard      = pmOutRequestGuard,
  pmOutCreatedAt  = Nothing,
  pmOutModifiedAt = Nothing
}



pmOutToResponse :: Entity PmOut -> PmOutResponse
pmOutToResponse (Entity pm_out_id PmOut{..}) = PmOutResponse {
  pmOutResponseId         = keyToInt64 pm_out_id,
  pmOutResponsePmId       = keyToInt64 pmOutPmId,
  pmOutResponseUserId     = keyToInt64 pmOutUserId,
  pmOutResponseLabel      = pmOutLabel,
  pmOutResponseIsSaved    = pmOutIsSaved,
  pmOutResponseActive     = pmOutActive,
  pmOutResponseGuard      = pmOutGuard,
  pmOutResponseCreatedAt  = pmOutCreatedAt,
  pmOutResponseModifiedAt = pmOutModifiedAt
}



pmOutsToResponses :: [Entity PmOut] -> PmOutResponses
pmOutsToResponses pmOuts = PmOutResponses {
  pmOutResponses = map pmOutToResponse pmOuts
}







--
-- Model/Internal
--


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
