{-# LANGUAGE RecordWildCards #-}

module All.Leuron (
  -- Handler
  getLeuronsR,
  postLeuronR0,
  getLeuronR,
  putLeuronR,
  deleteLeuronR,
  getCountLeuronsR,
  getLeuronStatsR,
  getLeuronStatR,
  getLeuronPacksR,
  getLeuronPackR,

  -- Model/Function
  leuronRequestToLeuron,
  leuronToResponse,
  leuronsToResponses,

  -- Model/Internal
  getLeuronsM,
  getLeurons_ByResourceIdM,
  getLeurons_ByResourceId_RandomM,
  getLeurons_ByUserIdM,
  getLeurons_ByEverythingM,
  getLeuronsIdsM,
  getLeuronM,
  insertLeuronM,
  updateLeuronM,
  deleteLeuronM,
  countLeuronsM,
  getLeuronStatsM,
  getLeuronStatM
) where



import           Import
import           Api.Params
import All.Prelude
import           Database.Esqueleto    ((^.))
import qualified Database.Esqueleto    as E
import qualified Database.Redis        as R
import qualified LN.T.Like             as L



--
-- Handler
--

getLeuronsR :: Handler Value
getLeuronsR = do

  user_id <- requireAuthId

  (toJSON . leuronsToResponses) <$> getLeuronsM user_id



postLeuronR0 :: Handler Value
postLeuronR0 = do

  user_id <- requireAuthId

  sp <- lookupStandardParams

  case (spResourceId sp) of

    Nothing -> notFound

    Just resource_id -> do
      leuron_request <- requireJsonBody
      (toJSON . leuronToResponse) <$> insertLeuronM user_id resource_id leuron_request




getLeuronR :: LeuronId -> Handler Value
getLeuronR leuron_id = do
  user_id <- requireAuthId
  (toJSON . leuronToResponse) <$> getLeuronM user_id leuron_id



putLeuronR :: LeuronId -> Handler Value
putLeuronR leuron_id = do
  user_id <- requireAuthId
  leuron_request <- requireJsonBody
  (toJSON . leuronToResponse) <$> updateLeuronM user_id leuron_id leuron_request



deleteLeuronR :: LeuronId -> Handler Value
deleteLeuronR leuron_id = do
  user_id <- requireAuthId
  void $ deleteLeuronM user_id leuron_id
  pure $ toJSON ()



getCountLeuronsR :: Handler Value
getCountLeuronsR = do
  user_id <- requireAuthId
  toJSON <$> countLeuronsM user_id



getLeuronStatsR :: Handler Value
getLeuronStatsR = do
  user_id <- requireAuthId
  toJSON <$> getLeuronStatsM user_id



getLeuronStatR :: LeuronId -> Handler Value
getLeuronStatR leuron_id = do
  user_id <- requireAuthId
  toJSON <$> getLeuronStatM user_id leuron_id



getLeuronPacksR :: Handler Value
getLeuronPacksR = do
  user_id <- requireAuthId
  toJSON <$> getLeuronPacksM user_id



getLeuronPackR :: LeuronId -> Handler Value
getLeuronPackR leuron_id = do
  user_id <- requireAuthId
  toJSON <$> getLeuronPackM user_id leuron_id




--
-- Model/Function
--

leuronRequestToLeuron :: UserId -> ResourceId -> LeuronRequest -> Leuron
leuronRequestToLeuron user_id resource_id LeuronRequest{..} = Leuron {
  leuronUserId        = user_id,
  leuronResourceId    = resource_id,
  leuronData          = encodeText leuronRequestData,
  leuronTitle         = leuronRequestTitle,
  leuronDescription   = leuronRequestDescription,
  leuronSection       = leuronRequestSection,
  leuronPage          = leuronRequestPage,
  leuronExamples      = encodeTextMaybe leuronRequestExamples,
  leuronStrengths     = encodeTextMaybe leuronRequestStrengths,
  leuronCategories    = if null leuronRequestCategories then Nothing else Just $ encodeText leuronRequestCategories,
  leuronSplits        = encodeTextMaybe leuronRequestSplits,
  leuronSubstitutions = encodeTextMaybe leuronRequestSubstitutions,
  leuronTags          = leuronRequestTags,
  leuronStyle         = encodeTextMaybe leuronRequestStyle,
  leuronActive        = True,
  leuronGuard         = leuronRequestGuard,
  leuronCreatedAt     = Nothing,
  leuronModifiedAt    = Nothing,
  leuronActivityAt    = Nothing
}



leuronToResponse :: Entity Leuron -> LeuronResponse
leuronToResponse (Entity leuron_id Leuron{..}) = LeuronResponse {
  leuronResponseId            = keyToInt64 leuron_id,
  leuronResponseUserId        = keyToInt64 leuronUserId,
  leuronResponseResourceId    = keyToInt64 leuronResourceId,
  leuronResponseData          = maybe LnEmpty id (decodeText leuronData),
  leuronResponseTitle         = leuronTitle,
  leuronResponseDescription   = leuronDescription,
  leuronResponseSection       = leuronSection,
  leuronResponsePage          = leuronPage,
  leuronResponseExamples      = maybe Nothing decodeText leuronExamples,
  leuronResponseStrengths     = maybe Nothing decodeText leuronStrengths,
  leuronResponseCategories    = maybe [] (\s -> maybe [] id (decodeText s)) leuronCategories,
  leuronResponseSplits        = maybe Nothing decodeText leuronSplits,
  leuronResponseSubstitutions = maybe Nothing decodeText leuronSubstitutions,
  leuronResponseTags          = leuronTags,
  leuronResponseStyle         = maybe Nothing decodeText leuronStyle,
  leuronResponseActive        = leuronActive,
  leuronResponseGuard         = leuronGuard,
  leuronResponseCreatedAt     = leuronCreatedAt,
  leuronResponseModifiedAt    = leuronModifiedAt,
  leuronResponseActivityAt    = leuronActivityAt
}



leuronsToResponses :: [Entity Leuron] -> LeuronResponses
leuronsToResponses leurons = LeuronResponses {
  leuronResponses = map leuronToResponse leurons
}





--
-- Model/Internal
--

getLeuronsM :: UserId -> Handler [Entity Leuron]
getLeuronsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spSortOrder of

    Nothing    -> normal sp
    Just order -> possibly_rand order sp


  where
  normal sp@StandardParams{..} = do
    case (spResourceId, spUserId) of

      (Just resource_id, _)     -> getLeurons_ByResourceIdM user_id resource_id sp

      (_, Just lookup_user_id)  -> getLeurons_ByUserIdM user_id lookup_user_id sp

      (_, _)                    -> getLeurons_ByEverythingM user_id sp

  possibly_rand order sp = do
    case order of
      SortOrderBy_Rnd -> rand sp
      _               -> normal sp

  rand sp@StandardParams{..} = do
    case spResourceId of
      Just resource_id          -> getLeurons_ByResourceId_RandomM user_id resource_id sp
      _                         -> getLeurons_ByEverythingM user_id sp



getLeurons_ByResourceIdM :: UserId -> ResourceId -> StandardParams -> Handler [Entity Leuron]
getLeurons_ByResourceIdM _ resource_id sp = do

  selectListDb sp [LeuronResourceId ==. resource_id] [] LeuronId



-- ESQUELETO-QUERY
getLeurons_ByResourceId_RandomM :: UserId -> ResourceId -> StandardParams -> Handler [Entity Leuron]
getLeurons_ByResourceId_RandomM _ resource_id _ = do

-- SELECT * FROM leuron OFFSET floor(random() * (select count(*) from leuron)) LIMIT 1;
-- TODO FIXME: the query below is not as efficient as the one above..

  runDB
    $ E.select
    $ E.from $ \leuron -> do
      E.where_ (leuron ^. LeuronResourceId E.==. E.val resource_id)
      E.orderBy [E.rand]
      E.offset 1
      E.limit 1
      return leuron



getLeurons_ByUserIdM :: UserId -> UserId -> StandardParams -> Handler [Entity Leuron]
getLeurons_ByUserIdM _ lookup_user_id sp = do

  selectListDb sp [LeuronUserId ==. lookup_user_id] [] LeuronId



getLeurons_ByEverythingM :: UserId -> StandardParams -> Handler [Entity Leuron]
getLeurons_ByEverythingM _ sp = do

  selectListDb sp [] [] LeuronId



getLeuronsIdsM :: UserId -> ResourceId -> Handler [LeuronId]
getLeuronsIdsM _ resource_id = do

  selectKeysListDb' [ LeuronResourceId ==. resource_id ] [] LeuronId



getLeuronM :: UserId -> LeuronId -> Handler (Entity Leuron)
getLeuronM _ leuron_id = do

  notFoundMaybe =<< selectFirstDb [ LeuronId ==. leuron_id ] []



insertLeuronM :: UserId -> ResourceId -> LeuronRequest -> Handler (Entity Leuron)
insertLeuronM user_id resource_id leuron_request = do

-- DEBUG:  liftIO $ print $ [show user_id, show resource_id]

  ts <- timestampH'

  let
    leuron = (leuronRequestToLeuron user_id resource_id leuron_request) { leuronCreatedAt = Just ts }

  void $ notFoundMaybe =<< selectFirstDb [ ResourceUserId ==. user_id, ResourceId ==. resource_id ] []

  entity@(Entity leuron_id _) <- insertEntityDb leuron

  -- background job
  --
  -- Add leuron categories to our redis keys
  insertLeuronCategoriesM leuron_id resource_id leuron_request

  -- Add leuron to our user's zsets etc
  insertLeuronR user_id resource_id leuron_id

  --
  -- end background job

  return entity



updateLeuronM :: UserId -> LeuronId -> LeuronRequest -> Handler (Entity Leuron)
updateLeuronM user_id leuron_id leuron_request = do

  ts <- timestampH'

  let
    Leuron{..} = (leuronRequestToLeuron user_id dummyId leuron_request) { leuronModifiedAt = Just ts }

  updateWhereDb
    [ LeuronUserId ==. user_id, LeuronId ==. leuron_id ]
    [ LeuronModifiedAt    =. leuronModifiedAt
    , LeuronData          =. leuronData
    , LeuronTitle         =. leuronTitle
    , LeuronDescription   =. leuronDescription
    , LeuronSection       =. leuronSection
    , LeuronPage          =. leuronPage
    , LeuronExamples      =. leuronExamples
    , LeuronStrengths     =. leuronStrengths
--    , LeuronCategories =.
    , LeuronSplits        =. leuronSplits
    , LeuronSubstitutions =. leuronSubstitutions
    , LeuronTags          =. leuronTags
    , LeuronStyle         =. leuronStyle
    ]

  notFoundMaybe =<< selectFirstDb [ LeuronUserId ==. user_id, LeuronId ==. leuron_id ] []



deleteLeuronM :: UserId -> LeuronId -> Handler ()
deleteLeuronM user_id leuron_id = do
  (Entity _ Leuron{..}) <- getLeuronM user_id leuron_id

  -- Remove from database
  deleteWhereDb [ LeuronUserId ==. user_id, LeuronId ==. leuron_id ]

  -- Remove from redis
  void $ deleteLeuronR user_id leuronResourceId leuron_id




insertLeuronCategoriesM :: LeuronId -> ResourceId -> LeuronRequest -> Handler ()
insertLeuronCategoriesM leuron_id resource_id LeuronRequest{..} = do
  red <- getsYesod appRed

  -- add leuron to it's categories
  void $ liftIO $ R.runRedis red $ do
    mapM_
      (\cat -> void $ R.sadd (categoriesKey cat) [encodeStrict leuron_id])
      (depListToLower leuronRequestCategories)

  -- hacks everywhere, for now.. adds categories for all leurons, belonging to a resource, to that resource
  void $ liftIO $ R.runRedis red $ do
    mapM_
      (\cat -> void $ R.sadd (resourceCategoriesKey resource_id cat) [encodeStrict cat])
      (depListToLower leuronRequestCategories)

  return ()






-- Redis
--

-- | When a user inserts a leuron, add it to the proper sets
--
insertLeuronR :: UserId -> ResourceId -> LeuronId -> Handler ()
insertLeuronR user_id resource_id leuron_id = do
  red <- getsYesod appRed
  void $ liftIO $ R.runRedis red $ do

    -- ln:users:<user_id>:z:leurons
    --
    void $ R.zadd (usersZLeuronsKey user_id) [(0, keyToInt64Sbs leuron_id)]

    -- ln:resources:<resource_id>
    --
    void $ R.sadd (resourcesKey resource_id) [keyToInt64Sbs leuron_id]

  return ()



-- | Undo insertLeuronR
--
deleteLeuronR :: UserId -> ResourceId -> LeuronId -> Handler ()
deleteLeuronR user_id resource_id leuron_id = do
  red <- getsYesod appRed
  void $ liftIO $ R.runRedis red $ do

    void $ R.zrem (usersZLeuronsKey user_id) [keyToInt64Sbs leuron_id]
    void $ R.srem (resourcesKey resource_id) [keyToInt64Sbs leuron_id]

  return ()



countLeuronsM :: UserId -> Handler CountResponses
countLeuronsM _ = do

  StandardParams{..} <- lookupStandardParams

  case (spUserId, spUserIds) of

    (_, _) -> do
      n <- countDb [ LeuronActive ==. True ]
      return $ CountResponses [CountResponse 0 (fromIntegral n)]



getLeuronStatsM :: UserId -> Handler LeuronStatResponses
getLeuronStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spBoardId of

    Just _  -> notFound
    Nothing -> notFound




getLeuronStatM :: UserId -> LeuronId -> Handler LeuronStatResponse
getLeuronStatM _ leuron_id = do

  return $ LeuronStatResponse {
    leuronStatResponseLeuronId    = keyToInt64 leuron_id,
    leuronStatResponseLikes       = 0,
    leuronStatResponseNeutral     = 0,
    leuronStatResponseDislikes    = 0,
    leuronStatResponseStars       = 0,
    leuronStatResponseViews       = 0
  }
