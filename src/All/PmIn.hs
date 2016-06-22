{-# LANGUAGE RecordWildCards #-}

module All.PmIn (
  -- Handler
  getPmInsR,
  postPmInsR,
  getPmInR,
  putPmInR,
  deletePmInR,

  -- Model/Function
  pmInRequestToPmIn,
  pmInToResponse,
  pmInsToResponses,

  -- Model/Internal
  getPmInsM,
  getPmInM,
  insertPmInM,
  updatePmInM,
  deletePmInM
) where



import           All.Prelude



--
-- Handler
--

getPmInsR :: HandlerEff Value
getPmInsR = do
  user_id <- requireAuthId
  (toJSON . pmInsToResponses) <$> getPmInsM user_id



postPmInsR :: HandlerEff Value
postPmInsR = do
  user_id <- requireAuthId
  sp <- lookupStandardParams
  case (spPmId sp) of
    Nothing -> notFound
    Just pm_id -> do
      pmIn_request <- requireJsonBody :: HandlerEff PmInRequest
      (toJSON . pmInToResponse) <$> insertPmInM user_id pm_id pmIn_request



getPmInR :: PmInId -> HandlerEff Value
getPmInR pmIn_id = do
  user_id <- requireAuthId
  (toJSON . pmInToResponse) <$> getPmInM user_id pmIn_id



putPmInR :: PmInId -> HandlerEff Value
putPmInR pmIn_id = do
  user_id <- requireAuthId
  pmIn_request <- requireJsonBody
  (toJSON . pmInToResponse) <$> updatePmInM user_id pmIn_id pmIn_request



deletePmInR :: PmInId -> HandlerEff Value
deletePmInR pmIn_id = do
  user_id <- requireAuthId
  void $ deletePmInM user_id pmIn_id
  pure $ toJSON ()






--
-- Model/Function
--

pmInRequestToPmIn :: UserId -> PmId -> PmInRequest -> PmIn
pmInRequestToPmIn user_id pm_id PmInRequest{..} = PmIn {
  pmInPmId       = pm_id,
  pmInUserId     = user_id,
  pmInLabel      = pmInRequestLabel,
  pmInIsRead     = pmInRequestIsRead,
  pmInIsStarred  = pmInRequestIsStarred,
  pmInIsNew      = True,
  pmInIsSaved    = True,
  pmInActive     = True,
  pmInGuard      = pmInRequestGuard,
  pmInCreatedAt  = Nothing,
  pmInModifiedAt = Nothing
}



pmInToResponse :: Entity PmIn -> PmInResponse
pmInToResponse (Entity pm_in_id PmIn{..}) = PmInResponse {
  pmInResponseId         = keyToInt64 pm_in_id,
  pmInResponsePmId       = keyToInt64 pmInPmId,
  pmInResponseUserId     = keyToInt64 pmInUserId,
  pmInResponseLabel      = pmInLabel,
  pmInResponseIsRead     = pmInIsRead,
  pmInResponseIsStarred  = pmInIsStarred,
  pmInResponseIsNew      = pmInIsNew,
  pmInResponseIsSaved    = pmInIsSaved,
  pmInResponseActive     = pmInActive,
  pmInResponseGuard      = pmInGuard,
  pmInResponseCreatedAt  = pmInCreatedAt,
  pmInResponseModifiedAt = pmInModifiedAt
}



pmInsToResponses :: [Entity PmIn] -> PmInResponses
pmInsToResponses pmIns = PmInResponses {
  pmInResponses = map pmInToResponse pmIns
}






--
-- Model/Internal
--

getPmInsM :: UserId -> HandlerEff [Entity PmIn]
getPmInsM user_id = do
  selectListDb' [ PmInUserId ==. user_id ] [] PmInId



getPmInM :: UserId -> PmInId -> HandlerEff (Entity PmIn)
getPmInM user_id pmIn_id = do
  notFoundMaybe =<< selectFirstDb [ PmInUserId ==. user_id, PmInId ==. pmIn_id ] []



insertPmInM :: UserId -> PmId -> PmInRequest -> HandlerEff (Entity PmIn)
insertPmInM user_id pm_id pmIn_request = do

  ts <- timestampH'

  let
    pmIn = (pmInRequestToPmIn user_id pm_id pmIn_request) { pmInCreatedAt = Just ts }

  insertEntityDb pmIn



updatePmInM :: UserId -> PmInId -> PmInRequest -> HandlerEff (Entity PmIn)
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



deletePmInM :: UserId -> PmInId -> HandlerEff ()
deletePmInM _ _ = do
  return ()
--  deleteWhereDb [ PmInUserId ==. user_id, PmInId ==. pmIn_id ]
