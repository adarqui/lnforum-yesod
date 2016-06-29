{-# LANGUAGE RecordWildCards #-}

module LN.All.PmOut (
  -- LN.Handler
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



import           LN.All.Prelude



--
-- LN.Handler
--

getPmOutsR :: LN.Handler Value
getPmOutsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON pmOutsToResponses $ getPmOutsM (pure sp) user_id



postPmOutsR :: LN.Handler Value
postPmOutsR = run $ do
  user_id        <- _requireAuthId
  pm_out_request <- requireJsonBody
  sp             <- lookupStandardParams
  errorOrJSON pmOutToResponse $ insertPmOutM (pure sp) user_id pm_out_request



getPmOutR :: PmOutId -> LN.Handler Value
getPmOutR pm_out_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON pmOutToResponse $ getPmOutM user_id pm_out_id



putPmOutR :: PmOutId -> LN.Handler Value
putPmOutR pm_out_id = run $ do
  user_id        <- _requireAuthId
  pm_out_request <- requireJsonBody
  errorOrJSON pmOutToResponse $ updatePmOutM user_id pm_out_id pm_out_request



deletePmOutR :: PmOutId -> LN.Handler Value
deletePmOutR pm_out_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deletePmOutM user_id pm_out_id





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


getPmOutsM :: Maybe StandardParams -> UserId -> LN.HandlerErrorEff [Entity PmOut]
getPmOutsM m_sp user_id = do
  selectListDbE m_sp [PmOutUserId ==. user_id, PmOutActive ==. True] [] PmOutId



getPmOutM :: UserId -> PmOutId -> LN.HandlerErrorEff (Entity PmOut)
getPmOutM user_id pm_out_id = do
  selectFirstDbE [PmOutUserId ==. user_id, PmOutId ==. pm_out_id, PmOutActive ==. True] []



insertPmOutM :: Maybe StandardParams -> UserId -> PmOutRequest -> LN.HandlerErrorEff (Entity PmOut)
insertPmOutM m_sp user_id pm_out_request = do

  case (lookupSpMay m_sp spPmId) of

    Just pm_id -> do
      ts <- timestampH'
      let
        pm_out = (pmOutRequestToPmOut user_id pm_id pm_out_request) { pmOutCreatedAt = Just ts }
      insertEntityDbE pm_out

    _          -> left $ Error_InvalidArguments "pm_id"



updatePmOutM :: UserId -> PmOutId -> PmOutRequest -> LN.HandlerErrorEff (Entity PmOut)
updatePmOutM user_id pm_out_id pm_out_request = do

  ts <- timestampH'

  let
    PmOut{..} = (pmOutRequestToPmOut user_id dummyId pm_out_request) { pmOutModifiedAt = Just ts }

  updateWhereDb
    [ PmOutUserId ==. user_id, PmOutId ==. pm_out_id, PmOutActive ==. True ]
    [ PmOutModifiedAt =. pmOutModifiedAt
    , PmOutLabel =. pmOutLabel
    ]

  selectFirstDbE [PmOutUserId ==. user_id, PmOutId ==. pm_out_id, PmOutActive ==. True] []



deletePmOutM :: UserId -> PmOutId -> LN.HandlerErrorEff ()
deletePmOutM _ _ = do
  right ()
