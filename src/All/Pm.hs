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
  (toJSON . pmsToResponses) <$> getPmsM user_id



postPmR0 :: Handler Value
postPmR0 = do
  user_id <- _requireAuthId
  sp <- lookupStandardParams
  -- can handle groups, users, user .. if we want
  case (spUserId sp) of
    Nothing -> notFound
    Just to_user_id ->
      if user_id == to_user_id
        then do
          -- can't send a pm to yourself
          permissionDenied "Can't send a PM to yourself"
        else do
          pm_request <- requireJsonBody :: HandlerEff PmRequest
          (toJSON . pmToResponse) <$> insertPmM user_id to_user_id pm_request



getPmR :: PmId -> Handler Value
getPmR pm_id = do
  user_id <- _requireAuthId
  (toJSON . pmToResponse) <$> getPmM user_id pm_id



putPmR :: PmId -> Handler Value
putPmR pm_id = do
  user_id <- _requireAuthId
  pm_request <- requireJsonBody
  (toJSON . pmToResponse) <$> updatePmM user_id pm_id pm_request



deletePmR :: PmId -> Handler Value
deletePmR pm_id = do
  user_id <- _requireAuthId
  void $ deletePmM user_id pm_id
  pure $ toJSON ()






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

getPmsM :: UserId -> HandlerEff [Entity Pm]
getPmsM user_id = do
  selectListDb' [ PmUserId ==. user_id ] [] PmId



getPmM :: UserId -> PmId -> HandlerEff (Entity Pm)
getPmM user_id pm_id = do
  notFoundMaybe =<< selectFirstDb [ PmUserId ==. user_id, PmId ==. pm_id ] []



insertPmM :: UserId -> UserId -> PmRequest -> HandlerEff (Entity Pm)
insertPmM user_id to_user_id pm_request = do

  ts <- timestampH'

  let
    pm = (pmRequestToPm user_id to_user_id pm_request) { pmCreatedAt = Just ts }

  insertEntityDb pm



updatePmM :: UserId -> PmId -> PmRequest -> HandlerEff (Entity Pm)
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



deletePmM :: UserId -> PmId -> HandlerEff ()
deletePmM _ _ = do
  return ()
--  deleteWhereDb [ PmUserId ==. user_id, PmId ==. pm_id ]
