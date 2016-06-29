{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module All.Pm (
  -- Handler
  getPmsR,
  postPmR0,
  getPmR,
  putPmR,
  deletePmR,

  -- Model/Function
  pmRequestToPm,
  pmToResponse,
  pmsToResponses,

  -- Model/Internal
  getPmsM,
  getPmM,
  insertPmM,
  updatePmM,
  deletePmM
) where



import           All.Prelude



--
-- Handler
--

getPmsR :: Handler Value
getPmsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON pmsToResponses $ getPmsM (pure sp) user_id



postPmR0 :: Handler Value
postPmR0 = run $ do
  user_id    <- _requireAuthId
  sp         <- lookupStandardParams
  pm_request <- requireJsonBody
  errorOrJSON pmToResponse $ insertPmM (pure sp) user_id pm_request



getPmR :: PmId -> Handler Value
getPmR pm_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON pmToResponse $ getPmM user_id pm_id



putPmR :: PmId -> Handler Value
putPmR pm_id = run $ do
  user_id    <- _requireAuthId
  pm_request <- requireJsonBody
  errorOrJSON pmToResponse $ updatePmM user_id pm_id pm_request



deletePmR :: PmId -> Handler Value
deletePmR pm_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deletePmM user_id pm_id






--
-- Model/Function
--

pmRequestToPm :: UserId -> UserId -> PmRequest -> Pm
pmRequestToPm user_id to_user_id PmRequest{..} = Pm {
  pmUserId     = user_id,
  pmToUserId   = to_user_id,
  pmSubject    = pmRequestSubject,
  pmBody       = pmRequestBody,
  pmActive     = True,
  pmGuard      = pmRequestGuard,
  pmCreatedAt  = Nothing,
  pmModifiedAt = Nothing,
  pmActivityAt = Nothing
}



pmToResponse :: Entity Pm -> PmResponse
pmToResponse (Entity pm_id Pm{..}) = PmResponse {
  pmResponseId         = keyToInt64 pm_id,
  pmResponseUserId     = keyToInt64 pmUserId,
  pmResponseToUserId   = keyToInt64 pmToUserId,
  pmResponseSubject    = pmSubject,
  pmResponseBody       = pmBody,
  pmResponseActive     = pmActive,
  pmResponseGuard      = pmGuard,
  pmResponseCreatedAt  = pmCreatedAt,
  pmResponseModifiedAt = pmModifiedAt,
  pmResponseActivityAt = pmActivityAt
}



pmsToResponses :: [Entity Pm] -> PmResponses
pmsToResponses pms = PmResponses {
  pmResponses = map pmToResponse pms
}






--
-- Model/Internal
--

getPmsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Pm]
getPmsM m_sp user_id = do
  selectListDbE m_sp [PmUserId ==. user_id, PmActive ==. True] [] PmId



getPmM :: UserId -> PmId -> HandlerErrorEff (Entity Pm)
getPmM user_id pm_id = do
  selectFirstDbE [PmUserId ==. user_id, PmId ==. pm_id, PmActive ==. True] []



insertPmM :: Maybe StandardParams -> UserId -> PmRequest -> HandlerErrorEff (Entity Pm)
insertPmM m_sp user_id pm_request = do

  case (lookupSpMay m_sp spUserId) of

    Just to_user_id ->
      if user_id == to_user_id

        then do
          -- can't send a pm to yourself
          left $ Error_PermissionDenied -- TODO FIXME: PermissionDeniedReason "Can't send a PM to yourself"

        else do
          ts <- timestampH'
          let
            pm = (pmRequestToPm user_id to_user_id pm_request) { pmCreatedAt = Just ts }
          insertEntityDbE pm

    _               -> left $ Error_InvalidArguments "user_id"



updatePmM :: UserId -> PmId -> PmRequest -> HandlerErrorEff (Entity Pm)
updatePmM user_id pm_id pm_request = do

  ts <- timestampH'

  let
    Pm{..} = (pmRequestToPm user_id dummyId pm_request) { pmModifiedAt = Just ts }

  updateWhereDb
    [ PmUserId ==. user_id, PmId ==. pm_id, PmActive ==. True ]
    [ PmModifiedAt =. pmModifiedAt
    , PmSubject =. pmSubject
    , PmBody =. pmBody
    ]

  selectFirstDbE [PmUserId ==. user_id, PmId ==. pm_id, PmActive ==. True] []



deletePmM :: UserId -> PmId -> HandlerErrorEff ()
deletePmM _ _ = left Error_NotImplemented
