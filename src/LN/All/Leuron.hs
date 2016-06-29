{-# LANGUAGE RecordWildCards #-}

module LN.All.Leuron (
  -- LN.Handler
  getLeuronsR,
  postLeuronR0,
  getLeuronR,
  putLeuronR,
  deleteLeuronR,
  getLeuronsCountR,
  getLeuronStatsR,
  getLeuronStatR,

  -- LN.Model/Function
  leuronRequestToLeuron,
  leuronToResponse,
  leuronsToResponses,

  -- LN.Model/Internal
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



import           LN.All.Prelude
import           Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import qualified Database.Redis     as R



--
-- LN.Handler
--

getLeuronsR :: Handler Value
getLeuronsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON leuronsToResponses $ getLeuronsM (pure sp) user_id



postLeuronR0 :: Handler Value
postLeuronR0 = run $ do
  user_id        <- _requireAuthId
  sp             <- lookupStandardParams
  leuron_request <- requireJsonBody
  errorOrJSON leuronToResponse $ insertLeuronM (pure sp) user_id leuron_request




getLeuronR :: LeuronId -> Handler Value
getLeuronR leuron_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON leuronToResponse $ getLeuronM user_id leuron_id



putLeuronR :: LeuronId -> Handler Value
putLeuronR leuron_id = run $ do
  user_id        <- _requireAuthId
  leuron_request <- requireJsonBody
  errorOrJSON leuronToResponse $ updateLeuronM user_id leuron_id leuron_request



deleteLeuronR :: LeuronId -> Handler Value
deleteLeuronR leuron_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteLeuronM user_id leuron_id



getLeuronsCountR :: Handler Value
getLeuronsCountR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ countLeuronsM (pure sp) user_id



getLeuronStatsR :: Handler Value
getLeuronStatsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getLeuronStatsM (pure sp) user_id



getLeuronStatR :: LeuronId -> Handler Value
getLeuronStatR leuron_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getLeuronStatM user_id leuron_id








--
-- LN.Model/Function
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
-- LN.Model/Internal
--

getLeuronsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Leuron]
getLeuronsM m_sp user_id = do

  case (lookupSpMay m_sp spSortOrder) of

    Nothing    -> normal
    Just order -> possibly_rand order


  where
  -- TODO FIXME: CLEANUP
  -- Clean up all of this.
  --
  normal = do
    case (lookupSpMay m_sp spResourceId, lookupSpMay m_sp spUserId) of

      (Just resource_id, _)     -> getLeurons_ByResourceIdM m_sp user_id resource_id

      (_, Just lookup_user_id)  -> getLeurons_ByUserIdM m_sp user_id lookup_user_id

      _                         -> getLeurons_ByEverythingM m_sp user_id

  possibly_rand order = do
    case order of
      SortOrderBy_Rnd -> rand
      _               -> normal

  rand = do
    case (lookupSpMay m_sp spResourceId) of
      Just resource_id          -> getLeurons_ByResourceId_RandomM m_sp user_id resource_id
      _                         -> getLeurons_ByEverythingM m_sp user_id



getLeurons_ByResourceIdM :: Maybe StandardParams -> UserId -> ResourceId -> HandlerErrorEff [Entity Leuron]
getLeurons_ByResourceIdM m_sp _ resource_id = do

  selectListDbE m_sp [LeuronResourceId ==. resource_id, LeuronActive ==. True] [] LeuronId



-- ESQUELETO-QUERY
getLeurons_ByResourceId_RandomM :: Maybe StandardParams -> UserId -> ResourceId -> HandlerErrorEff [Entity Leuron]
getLeurons_ByResourceId_RandomM _ _ resource_id = do

-- SELECT * FROM leuron OFFSET floor(random() * (select count(*) from leuron)) LIMIT 1;
-- TODO FIXME: the query below is not as efficient as the one above..

  Right <$>
    (_runDB
    $ E.select
    $ E.from $ \leuron -> do
      E.where_ (leuron ^. LeuronResourceId E.==. E.val resource_id)
      E.orderBy [E.rand]
      E.offset 1
      E.limit 1
      pure leuron)



getLeurons_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff [Entity Leuron]
getLeurons_ByUserIdM m_sp _ lookup_user_id = do

  selectListDbE m_sp [LeuronUserId ==. lookup_user_id, LeuronActive ==. True] [] LeuronId



getLeurons_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Leuron]
getLeurons_ByEverythingM m_sp _ = do

  selectListDbE m_sp [LeuronActive ==. True] [] LeuronId



getLeuronsIdsM :: Maybe StandardParams -> UserId -> ResourceId -> HandlerErrorEff [LeuronId]
getLeuronsIdsM m_sp _ resource_id = do

  selectKeysListDbE m_sp [LeuronResourceId ==. resource_id, LeuronActive ==. True] [] LeuronId



getLeuronM :: UserId -> LeuronId -> HandlerErrorEff (Entity Leuron)
getLeuronM _ leuron_id = do

  selectFirstDbE [LeuronId ==. leuron_id, LeuronActive ==. True] []



insertLeuronM :: Maybe StandardParams -> UserId -> LeuronRequest -> HandlerErrorEff (Entity Leuron)
insertLeuronM m_sp user_id leuron_request = do

  case (lookupSpMay m_sp spResourceId) of

    Just resource_id -> do
      ts <- timestampH'

      let
        leuron = (leuronRequestToLeuron user_id resource_id leuron_request) { leuronCreatedAt = Just ts }

      m_resource <- selectFirstDb [ResourceUserId ==. user_id, ResourceId ==. resource_id, ResourceActive ==. True] []
      case m_resource of
        Nothing -> left LN.Error_NotFound
        Just _  -> do
          entity@(Entity leuron_id _) <- insertEntityDb leuron
          -- background job
          --
          -- Add leuron categories to our redis keys
          void $ insertLeuronCategoriesM leuron_id resource_id leuron_request
          -- Add leuron to our user's zsets etc
          void $ insertLeuronRedis user_id resource_id leuron_id
          --
          -- end background job
          right entity

    _ -> left $ LN.Error_InvalidArguments "resource_id"



updateLeuronM :: UserId -> LeuronId -> LeuronRequest -> HandlerErrorEff (Entity Leuron)
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

  selectFirstDbE [LeuronUserId ==. user_id, LeuronId ==. leuron_id, LeuronActive ==. True] []



deleteLeuronM :: UserId -> LeuronId -> HandlerErrorEff ()
deleteLeuronM user_id leuron_id = do
  e_leuron <- getLeuronM user_id leuron_id
  rehtie e_leuron left $ \(Entity _ Leuron{..}) -> do

    -- Remove from database
    deleteWhereDb [ LeuronUserId ==. user_id, LeuronId ==. leuron_id ]

    -- Remove from redis
    void $ deleteLeuronRedis user_id leuronResourceId leuron_id

    right ()




insertLeuronCategoriesM :: LeuronId -> ResourceId -> LeuronRequest -> HandlerErrorEff ()
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

  right ()






-- Redis
--

-- | When a user inserts a leuron, add it to the proper sets
--
insertLeuronRedis :: UserId -> ResourceId -> LeuronId -> HandlerErrorEff ()
insertLeuronRedis user_id resource_id leuron_id = do
  red <- getsYesod appRed
  void $ liftIO $ R.runRedis red $ do

    -- ln:users:<user_id>:z:leurons
    --
    void $ R.zadd (usersZLeuronsKey user_id) [(0, keyToInt64Sbs leuron_id)]

    -- ln:resources:<resource_id>
    --
    void $ R.sadd (resourcesKey resource_id) [keyToInt64Sbs leuron_id]

  right ()



-- | Undo insertLeuronR
--
deleteLeuronRedis :: UserId -> ResourceId -> LeuronId -> HandlerErrorEff ()
deleteLeuronRedis user_id resource_id leuron_id = do
  red <- getsYesod appRed
  void $ liftIO $ R.runRedis red $ do

    void $ R.zrem (usersZLeuronsKey user_id) [keyToInt64Sbs leuron_id]
    void $ R.srem (resourcesKey resource_id) [keyToInt64Sbs leuron_id]

  right ()



countLeuronsM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countLeuronsM _ _ = do

  n <- countDb [LeuronActive ==. True]
  right $ CountResponses [CountResponse 0 (fromIntegral n)]



getLeuronStatsM :: Maybe StandardParams -> UserId -> HandlerErrorEff LeuronStatResponses
getLeuronStatsM _ _ = left LN.Error_NotImplemented



getLeuronStatM :: UserId -> LeuronId -> HandlerErrorEff LeuronStatResponse
getLeuronStatM _ leuron_id = do

  right $ LeuronStatResponse {
    leuronStatResponseLeuronId    = keyToInt64 leuron_id,
    leuronStatResponseLikes       = 0,
    leuronStatResponseNeutral     = 0,
    leuronStatResponseDislikes    = 0,
    leuronStatResponseStars       = 0,
    leuronStatResponseViews       = 0
  }
