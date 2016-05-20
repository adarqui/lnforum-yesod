{-# LANGUAGE RecordWildCards #-}

module Model.Leuron.Internal (
  getLeuronsM,
  getLeuronsBy_ResourceIdM,
  getLeuronsBy_UserIdM,
  getLeuronsBy_EverythingM,

  getLeuronsIdsM,
  getLeuronM,
  insertLeuronM,
  updateLeuronM,
  deleteLeuronM,

  countLeuronsM,

  getLeuronStatsM,
  getLeuronStatM
) where



import qualified Database.Redis        as R
import qualified LN.T.Like             as L
import           Model.Leuron.Function
import           Model.Prelude





getLeuronsM :: UserId -> Handler [Entity Leuron]
getLeuronsM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spResourceId, spUserId) of

    (Just resource_id, _)     -> getLeuronsBy_ResourceIdM user_id resource_id sp

    (_, Just lookup_user_id)  -> getLeuronsBy_UserIdM user_id lookup_user_id sp

    (_, _)                    -> getLeuronsBy_EverythingM user_id sp



getLeuronsBy_ResourceIdM :: UserId -> ResourceId -> StandardParams -> Handler [Entity Leuron]
getLeuronsBy_ResourceIdM _ resource_id sp = do

  selectListDb sp [LeuronResourceId ==. resource_id] [] LeuronId



getLeuronsBy_UserIdM :: UserId -> UserId -> StandardParams -> Handler [Entity Leuron]
getLeuronsBy_UserIdM _ lookup_user_id sp = do

  selectListDb sp [LeuronUserId ==. lookup_user_id] [] LeuronId



getLeuronsBy_EverythingM :: UserId -> StandardParams -> Handler [Entity Leuron]
getLeuronsBy_EverythingM _ sp = do

  selectListDb sp [] [] LeuronId



getLeuronsIdsM :: UserId -> ResourceId -> Handler [LeuronId]
getLeuronsIdsM _ resource_id = do
  selectKeysListDb' [ LeuronResourceId ==. resource_id ] [] LeuronId



getLeuronM :: UserId -> LeuronId -> Handler (Entity Leuron)
getLeuronM _ leuron_id = do
  notFoundMaybe =<< selectFirstDb [ LeuronId ==. leuron_id ] []



insertLeuronM :: UserId -> ResourceId -> LeuronRequest -> Handler (Entity Leuron)
insertLeuronM user_id resource_id leuron_request = do

  liftIO $ print $ [show user_id, show resource_id]

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
    [ LeuronModifiedAt =. leuronModifiedAt
    , LeuronData =. leuronData
    , LeuronTitle =. leuronTitle
    , LeuronDescription =. leuronDescription
    , LeuronSection =. leuronSection
    , LeuronPage =. leuronPage
    , LeuronExamples =. leuronExamples
    , LeuronStrengths =. leuronStrengths
--    , LeuronCategories =.
    , LeuronSplits =. leuronSplits
    , LeuronSubstitutions =. leuronSubstitutions
    , LeuronTags =. leuronTags
    , LeuronStyle =. leuronStyle
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

-- likes <- selectListDb defaultStandardParams [
-- stars <-

  return $ LeuronStatResponse {
    leuronStatResponseLeuronId    = keyToInt64 leuron_id,
    leuronStatResponseLikes       = 0,
    leuronStatResponseNeutral     = 0,
    leuronStatResponseDislikes    = 0,
    leuronStatResponseStars       = 0,
    leuronStatResponseViews       = 0
  }
