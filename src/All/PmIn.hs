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

getPmInsR :: Handler Value
getPmInsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON pmInsToResponses $ getPmInsM (pure sp) user_id



postPmInsR :: Handler Value
postPmInsR = run $ do
  user_id <- _requireAuthId
  sp <- lookupStandardParams
  pm_in_request <- requireJsonBody
  errorOrJSON pmInToResponse $ insertPmInM (pure sp) user_id pm_in_request



getPmInR :: PmInId -> Handler Value
getPmInR pm_in_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON pmInToResponse $ getPmInM user_id pm_in_id



putPmInR :: PmInId -> Handler Value
putPmInR pm_in_id = run $ do
  user_id       <- _requireAuthId
  pm_in_request <- requireJsonBody
  errorOrJSON pmInToResponse $ updatePmInM user_id pm_in_id pm_in_request



deletePmInR :: PmInId -> Handler Value
deletePmInR pm_in_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deletePmInM user_id pm_in_id






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

getPmInsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity PmIn]
getPmInsM m_sp user_id = do
  selectListDbE m_sp [PmInUserId ==. user_id, PmInActive ==. True] [] PmInId



getPmInM :: UserId -> PmInId -> HandlerErrorEff (Entity PmIn)
getPmInM user_id pm_in_id = do
  selectFirstDbE [PmInUserId ==. user_id, PmInId ==. pm_in_id, PmInActive ==. True] []



insertPmInM :: Maybe StandardParams -> UserId -> PmInRequest -> HandlerErrorEff (Entity PmIn)
insertPmInM m_sp user_id pm_in_request = do

  case (lookupSpMay m_sp spPmId) of

    Just pm_id -> do
      ts <- timestampH'

      let
        pm_in = (pmInRequestToPmIn user_id pm_id pm_in_request) { pmInCreatedAt = Just ts }

      insertEntityDbE pm_in



updatePmInM :: UserId -> PmInId -> PmInRequest -> HandlerErrorEff (Entity PmIn)
updatePmInM user_id pm_in_id pm_in_request = do

  ts <- timestampH'

  let
    PmIn{..} = (pmInRequestToPmIn user_id dummyId pm_in_request) { pmInModifiedAt = Just ts }

  updateWhereDb
    [ PmInUserId ==. user_id, PmInId ==. pm_in_id, PmInActive ==. True]
    [ PmInModifiedAt =. pmInModifiedAt
    , PmInLabel =. pmInLabel
    , PmInIsRead =. pmInIsRead
    , PmInIsStarred =. pmInIsStarred
    ]

  selectFirstDbE [PmInUserId ==. user_id, PmInId ==. pm_in_id, PmInActive ==. True] []



deletePmInM :: UserId -> PmInId -> HandlerErrorEff ()
deletePmInM _ _ = right ()
