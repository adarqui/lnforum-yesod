{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module LN.Api.Params (
  StandardParams (..),
  defaultStandardParams,
  lookupStandardParams,
  lookupSpMay,
  lookupSpBool,
  lookupGetParam400,
  lookupGetParam401,
  lookupGetParam403,
  lookupGetParam404,
  lookupEnt,
  lookupLikeEnt,
  lookupLikeEntMay,
  lookupViewEnt,
  lookupViewEntMay,
  spToSelect,
  spToSelectMay,
  spToSelectE,
  timestamp,
  timestampH,
  timestampH'
) where



import           Data.List             (nub)
import           Data.Time             ()
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Database.Esqueleto    as E
import           LN.Control
import           LN.Import
import           LN.Misc.Codec
import           LN.T.Ent              (Ent (..))
import           LN.T.Param



data StandardParams = StandardParams {
  spOffset        :: Maybe Int64,
  spLimit         :: Maybe Int64,
  spSortOrder     :: Maybe SortOrderBy,
  spOrder         :: Maybe OrderBy,
  spTs            :: Maybe UTCTime,
  spUnixTs        :: Maybe Int64,
  spCreatedTs     :: Maybe UTCTime,
  spCreatedUnixTs :: Maybe Int64,
  spRealIp        :: Maybe Text,
  spIp            :: Maybe Text,
  -- id
  spUserId        :: Maybe UserId,
  spParentId      :: Maybe Int64,
  spForumId       :: Maybe ForumId,
  -- ids
  spUserIds       :: Maybe [UserId], -- TODO FIXME: change to spUsersIds, same for the others below
  -- names
  spUserName      :: Maybe Text,
  spParentName    :: Maybe Text,
  spEmail         :: Maybe Text,
  spSelf          :: Bool
} deriving (Eq, Show)



defaultStandardParams :: StandardParams
defaultStandardParams = StandardParams {

  spOffset           = Nothing,
  spLimit            = defLimit,
  spSortOrder        = Nothing,
  spOrder            = Nothing,
  spTs               = Nothing,
  spUnixTs           = Nothing,
  spCreatedTs        = Nothing,
  spCreatedUnixTs    = Nothing,
  spRealIp           = Nothing,
  spIp               = Nothing,
  -- id
  spUserId           = Nothing,
  spParentId         = Nothing,
  -- ids
  spUserIds          = Nothing,
  -- names
  spUserName         = Nothing,
  spParentName       = Nothing,
  spEmail            = Nothing,
  spSelf             = False
}



defLimit :: Maybe Int64
defLimit = Just 10

minLimit :: Int64
minLimit = 1

maxLimit :: Int64
maxLimit = 50


sanitizeLimit :: Int64 -> Int64
sanitizeLimit limit
  | limit < minLimit = minLimit
  | limit > maxLimit = maxLimit
  | otherwise        = limit



minOffset :: Int64
minOffset = 0

sanitizeOffset :: Int64 -> Int64
sanitizeOffset offset
  | offset < minOffset = minOffset
  | otherwise          = offset



lookupStandardParams :: HandlerEff StandardParams
lookupStandardParams = do

  offset             <- lookupGetParam $ tshow ParamTag_Offset
  limit              <- lookupGetParam $ tshow ParamTag_Limit
  sort_order         <- lookupGetParam $ tshow ParamTag_SortOrder
  order              <- lookupGetParam $ tshow ParamTag_Order
  ts                 <- lookupGetParam $ tshow ParamTag_Timestamp
  unix_ts            <- lookupGetParam $ tshow ParamTag_UnixTimestamp
  created_ts         <- lookupGetParam $ tshow ParamTag_CreatedAtTimestamp
  created_unix_ts    <- lookupGetParam $ tshow ParamTag_CreatedAtUnixTimestamp
  real_ip            <- lookupHeader "real_ip"  -- TODO FIXME: case insensitive bytestring $ tshow ParamTag_RealIP
  ip                 <- lookupGetParam $ tshow ParamTag_IP
  -- id
  user_id            <- lookupGetParam $ tshow ParamTag_ByUserId
  parent_id          <- lookupGetParam $ tshow ParamTag_ByParentId
  -- ids
  user_ids           <- lookupGetParam $ tshow ParamTag_ByUsersIds
  -- names
  user_name          <- lookupGetParam $ tshow ParamTag_ByUserName
  email              <- lookupGetParam $ tshow ParamTag_ByEmail
  self               <- (maybe False (const True)) <$> (lookupGetParam $ tshow ParamTag_BySelf)

  -- TODO: FIXME: need to safely tread, because the value may not read properly (incorrect input)
  pure $ StandardParams {
    spOffset           = fmap (sanitizeOffset . tread) offset,
    spLimit            = fmap (sanitizeLimit . tread) limit,
    spSortOrder        = fmap tread sort_order,
    spOrder            = fmap tread order,
    spTs               = fmap tread ts,
    spUnixTs           = fmap tread unix_ts,
    spCreatedTs        = fmap tread created_ts,
    spCreatedUnixTs    = fmap tread created_unix_ts,
    spRealIp           = fmap bread real_ip,
    spIp               = fmap tread ip,
    -- id
    spUserId           = fmap textToKey' user_id,
    spParentId         = fmap tread parent_id,
    -- ids
    spUserIds          = fmap (nub . textToKeys') user_ids,
    --- names
    spUserName         = user_name,
    spEmail            = email,
    spSelf             = self
  }



lookupSpMay :: Maybe StandardParams -> (StandardParams -> Maybe a) -> Maybe a
lookupSpMay Nothing _   = Nothing
lookupSpMay (Just sp) f = f sp



lookupSpBool :: Maybe StandardParams -> (StandardParams -> Bool) -> Bool
lookupSpBool Nothing _   = False
lookupSpBool (Just sp) f = f sp



lookupGetParam400 :: Text -> HandlerEff Text
lookupGetParam400 = lookupGetParamStatus 400

lookupGetParam401 :: Text -> HandlerEff Text
lookupGetParam401 = lookupGetParamStatus 401

lookupGetParam403 :: Text -> HandlerEff Text
lookupGetParam403 = lookupGetParamStatus 403

lookupGetParam404 :: Text -> HandlerEff Text
lookupGetParam404 = lookupGetParamStatus 404



lookupGetParamStatus :: Int -> Text -> HandlerEff Text
lookupGetParamStatus status param = do
  r <- lookupGetParam param
  case r of
    Nothing -> case status of
      400 -> badMethod
      401 -> notAuthenticated
      403 -> permissionDenied "Nope."
      404 -> notFound
      _   -> notFound
    Just r' -> pure r'



-- ** LN.Errors
-- , notFound
-- , badMethod
-- , notAuthenticated
-- , permissionDenied
-- , permissionDeniedI
-- , invalidArgs
-- , invalidArgsI




lookupEnt :: StandardParams -> Maybe (Ent, Int64)
lookupEnt (spUserId -> Just v)         = Just (Ent_User, keyToInt64 v)
lookupEnt _                            = Nothing




lookupLikeEntMay :: Maybe StandardParams -> Maybe (Ent, Int64)
lookupLikeEntMay Nothing   = Nothing
lookupLikeEntMay (Just sp) = lookupLikeEnt sp



lookupLikeEnt :: StandardParams -> Maybe (Ent, Int64)
-- lookupLikeEnt (spComment -> Just v)     = Just (Ent_Comment, keyToInt64 v)
lookupLikeEnt _                            = Nothing




lookupViewEntMay :: Maybe StandardParams -> Maybe (Ent, Int64)
lookupViewEntMay Nothing   = Nothing
lookupViewEntMay (Just sp) = lookupViewEnt sp

lookupViewEnt :: StandardParams -> Maybe (Ent, Int64)
lookupViewEnt (spUserId -> Just v)         = Just (Ent_User, keyToInt64 v)
lookupViewEnt _                            = Nothing



--
-- TODO FIXME: MOVE THESE DB FUNCTIONS OUT INTO THEIR OWN MODULE
--







-- | For filtering, ordering, limiting etc.
--
spToSelect :: forall typ record. StandardParams -> EntityField record typ -> [SelectOpt record]
spToSelect StandardParams{..} field =
  offset <> limit <> order
  where
  offset = case spOffset of
           Nothing      -> []
           Just offset' -> [OffsetBy $ fromIntegral offset']
  limit  = case spLimit of
           Nothing     -> []
           Just limit' -> [LimitTo $ fromIntegral limit']
  order  = case spSortOrder of
           Nothing               -> []
           Just SortOrderBy_Asc  -> [Asc field]
           Just SortOrderBy_Dsc  -> [Desc field]
           -- TODO FIXME: need rand
           _                     -> [Asc field]



spToSelectMay :: forall typ record. Maybe StandardParams -> EntityField record typ -> [SelectOpt record]
spToSelectMay Nothing _       = []
spToSelectMay (Just sp) field = spToSelect sp field



-- spToSelect :: forall typ record. StandardParams -> EntityField record typ -> [SelectOpt record]
-- esqueleto

spToSelectE ::
  forall (expr :: * -> *) backend (m :: * -> *).
  E.Esqueleto m expr backend =>
  StandardParams -> m ()
spToSelectE StandardParams{..} {-field-} = do
  offset
  limit
--  order
  where
  offset = case spOffset of
           Nothing      -> E.offset 0
           Just offset' -> E.offset $ fromIntegral offset'
  limit  = case spLimit of
           Nothing     -> pure ()
           Just limit' -> E.limit $ fromIntegral limit'
           {-
  order  = case spOrder of
           Nothing         -> []
           Just SpOrderAsc  -> E.orderBy [E.asc field]
           Just SpOrderDsc  -> E.orderBy [E.desc field]
           Just SpOrderRand -> E.orderBy [E.rand]
           -}



-- | Used to modify createdAt, modifiedAt, etc timestamps
--
-- DANGEROUS IN PRODUCTION, FIXME
--
-- Should have DEVEL/PRODUCTION mode dictate whether this works or not.
--
timestamp :: StandardParams -> IO UTCTime
timestamp sp = do
  case (spTs sp, spUnixTs sp) of
    (Nothing, Nothing) -> getCurrentTime
    (Just ts, _)       -> pure ts
    (_, Just ts)       -> pure $ posixSecondsToUTCTime $ fromIntegral ts



timestampH :: StandardParams -> HandlerEff UTCTime
timestampH sp = do
  liftIO $ timestamp sp



timestampH' :: HandlerEff UTCTime
timestampH' = lookupStandardParams >>= timestampH
