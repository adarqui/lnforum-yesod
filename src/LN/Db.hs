{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module LN.Db (
  selectListDb,
  selectListDbE,
  selectKeysListDb,
  selectKeysListDbE,
  selectFirstDb,
  selectFirstDbE,
  insertDb,
  insertDbE,
  insertUniqueDb,
  insertUniqueDbE,
  insertEntityDb,
  insertEntityDbE,
  insertEntityByDbE,
  insertEntityDbF,
  updateDb,
  updateDbE,
  updateWhereDb,
  updateWhereDbE,
  deleteWhereDb,
  deleteWhereDbE,
  deleteCascadeDb,
  deleteCascadeDbE,
  deleteCascadeWhereDb,
  deleteCascadeWhereDbE,
  deleteDb,
  deleteDbE,
  countDb,
  countDbE
) where



import           Data.Time     ()
import           LN.Api.Params (StandardParams, spToSelectMay)
import           LN.Control
import           LN.Import
import           LN.Lifted



-- | selectList query helper
--
{-
selectListDb
  :: forall site val typ.
     (PersistEntity val,
     PersistQuery (PersistEntityBackend val),
     YesodPersist site,
     PersistEntityBackend val ~ YesodPersistBackend site)
  => Maybe StandardParams
  -> [Filter val]
  -> [SelectOpt val]
  -> EntityField val typ
  -> ControlMA (HandlerT site IO) [Entity val]
  -}
selectListDb :: (PersistEntityBackend record
                 ~
                 BaseBackend (YesodPersistBackend site),
                 PersistEntity record, PersistQueryRead (YesodPersistBackend site),
                 YesodPersist site) =>
                Maybe StandardParams
                -> [Filter record]
                -> [SelectOpt record]
                -> EntityField record typ
                -> ControlMA (HandlerT site IO) [Entity record]
selectListDb m_sp query filt field = do
  _runDB $ selectList query ((spToSelectMay m_sp field) <> filt)



{-
selectListDbE
  :: forall site val typ.
     (PersistEntity val,
     PersistQuery (PersistEntityBackend val),
     YesodPersist site,
     PersistEntityBackend val ~ YesodPersistBackend site)
  => Maybe StandardParams
  -> [Filter val]
  -> [SelectOpt val]
  -> EntityField val typ
  -> ControlMA (HandlerT site IO) (ErrorEff [Entity val])
-}
{-
selectListDbE :: (PersistEntityBackend record
                  ~
                  BaseBackend (YesodPersistBackend site),
                  YesodPersist site, PersistQueryRead (YesodPersistBackend site),
                  PersistEntity record) =>
                 Maybe StandardParams
                 -> [Filter record]
                 -> [SelectOpt record]
                 -> EntityField record typ
                 -> ControlMA (HandlerT site IO) (ErrorEff [Entity val])
                 -}
selectListDbE m_sp query filt field = Right <$> selectListDb m_sp query filt field



-- | selectKeysList query helper
--
{-
selectKeysListDb
  :: forall site val typ.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (PersistEntityBackend val),
     PersistEntityBackend val ~ YesodPersistBackend site)
  => Maybe StandardParams
  -> [Filter val]
  -> [SelectOpt val]
  -> EntityField val typ
  -> ControlMA (HandlerT site IO) [Key val]
  -}
  {-
selectKeysListDb :: (PersistEntityBackend record
                     ~
                     BaseBackend (YesodPersistBackend site),
                     PersistEntity record, PersistQueryRead (YesodPersistBackend site),
                     YesodPersist site) =>
                    Maybe StandardParams
                    -> [Filter record]
                    -> [SelectOpt record]
                    -> EntityField record typ
                    -> ControlMA (HandlerT site IO) [Key val]
                    -}
selectKeysListDb m_sp query filt field = do
  _runDB $ selectKeysList query ((spToSelectMay m_sp field) <> filt)



{-
selectKeysListDbE
  :: forall site val typ.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (PersistEntityBackend val),
     PersistEntityBackend val ~ YesodPersistBackend site)
  => Maybe StandardParams
  -> [Filter val]
  -> [SelectOpt val]
  -> EntityField val typ
  -> ControlMA (HandlerT site IO) (ErrorEff [Key val])
  -}
  {-
selectKeysListDbE :: (PersistEntityBackend record
                      ~
                      BaseBackend (YesodPersistBackend site),
                      YesodPersist site, PersistQueryRead (YesodPersistBackend site),
                      PersistEntity record) =>
                     Maybe StandardParams
                     -> [Filter record]
                     -> [SelectOpt record]
                     -> EntityField record typ
                     -> ControlMA (HandlerT site IO) (ErrorEff [Key val])
                     -}
selectKeysListDbE m_sp query filt field = Right <$> selectKeysListDb m_sp query filt field




-- | selectFirst query helper
--
{-
selectFirstDb
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> [SelectOpt val]
  -> ControlMA (HandlerT site IO) (Maybe (Entity val))
  -}
selectFirstDb :: (PersistEntityBackend record
                  ~
                  BaseBackend (YesodPersistBackend site),
                  PersistEntity record, PersistQueryRead (YesodPersistBackend site),
                  YesodPersist site) =>
                 [Filter record]
                 -> [SelectOpt record]
                 -> ControlMA (HandlerT site IO) (Maybe (Entity record))
selectFirstDb query filt = _runDB $ selectFirst query filt



{-
selectFirstDbE
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> [SelectOpt val]
  -> ControlMA (HandlerT site IO) (ErrorEff (Entity val))
  -}
  {-
selectFirstDbE :: (PersistEntityBackend record
                   ~
                   BaseBackend (YesodPersistBackend site),
                   YesodPersist site, PersistQueryRead (YesodPersistBackend site),
                   PersistEntity record) =>
                  [Filter record]
                  -> [SelectOpt record]
                  -> ControlMA (HandlerT site IO) (ErrorEff (Entity val))
                  -}
selectFirstDbE query filt = do
  m <- selectFirstDb query filt
  case m of
    Nothing -> pure $ Left Error_NotFound
    Just v  -> pure $ Right v



-- | insert helper
--
{-
insertDb
  :: forall a site.
     (PersistEntity a,
     YesodPersist site,
     PersistStore (PersistEntityBackend a),
     PersistEntityBackend a ~ YesodPersistBackend site)
  => a
  -> ControlMA (HandlerT site IO) (Key a)
  -}
insertDb :: (PersistEntityBackend a
             ~
             BaseBackend (YesodPersistBackend site),
             PersistEntity a, PersistStoreWrite (YesodPersistBackend site),
             YesodPersist site) =>
            a -> ControlMA (HandlerT site IO) (Key a)
insertDb = _runDB . insert



{-
insertDbE
  :: forall a site.
     (PersistEntity a,
     YesodPersist site,
     PersistStore (PersistEntityBackend a),
     PersistEntityBackend a ~ YesodPersistBackend site)
     => a
     -> ControlMA (HandlerT site IO) (ErrorEff (Key a))
     -}
     {-
insertDbE :: (PersistEntityBackend a1
              ~
              BaseBackend (YesodPersistBackend site),
              YesodPersist site, PersistStoreWrite (YesodPersistBackend site),
              PersistEntity a1) =>
             a1
             -> ControlMA (HandlerT site IO) (ErrorEff (ey a))
             -}
insertDbE ent = Right <$> insertDb ent



{-
insertUniqueDb :: (PersistEntityBackend a
             ~
             BaseBackend (YesodPersistBackend site),
             PersistEntity a, PersistStoreWrite (YesodPersistBackend site),
             YesodPersist site) =>
             a -> ControlMA (HandlerT site IO) (Maybe (Key a))
             -}
insertUniqueDb = _runDB . insertUnique



insertUniqueDbE ent = Right <$> insertUniqueDb ent




-- | insertEntity helper
--
{-
insertEntityDb
  :: forall site e.
     (PersistEntity e,
     YesodPersist site,
     PersistStore (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend e)
  => e
  -> ControlMA (HandlerT site IO) (Entity e)
  -}
{-
insertEntityDb :: (PersistEntityBackend e
                   ~
                   BaseBackend (YesodPersistBackend site),
                   PersistEntity e, PersistStoreWrite (YesodPersistBackend site),
                   YesodPersist site) =>
                   ControlMA (HandlerT site IO) (Entity a)
-}
insertEntityDb entity = _runDB $ insertEntity entity



{-
insertEntityDbE
  :: forall site e.
     (PersistEntity e,
     YesodPersist site,
     PersistStore (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend e)
  => e
  -> ControlMA (HandlerT site IO) (ErrorEff (Entity e))
  -}
insertEntityDbE :: (PersistEntityBackend e
                    ~
                    BaseBackend (YesodPersistBackend site),
                    YesodPersist site, PersistStoreWrite (YesodPersistBackend site),
                    PersistEntity e) =>
                   e
                   -> ControlMA (HandlerT site IO) (ErrorEff (Entity e))
insertEntityDbE entity = Right <$> insertEntityDb entity



{-
insertEntityByDbE
  :: forall site b.
     (PersistEntity b,
     PersistUnique (PersistEntityBackend b),
     YesodPersist site,
     PersistEntityBackend b ~ YesodPersistBackend site)
  => b
  -> ControlMA (HandlerT site IO) (ErrorEff (Entity b))
  -}
{-
insertEntityByDbE :: (PersistEntityBackend a
                      ~
                      BaseBackend (YesodPersistBackend site),
                      PersistEntity a, PersistUniqueWrite (YesodPersistBackend site),
                      YesodPersist site) =>
                     a
                     -> ControlMA (HandlerT site IO) (ErrorEff (Entity b))
-}
insertEntityByDbE entity = do
  r <- _runDB $ insertBy entity
  case r of
                 -- TODO FIXME: Change to Error_AlreadyExists
    Left _    -> pure $ Left Error_Unknown
    Right key -> do
      m_entity <- _runDB $ get key
      pure $ maybe (Left Error_NotFound) (Right . Entity key) m_entity




-- F = Fondation
insertEntityDbF ent = insertDb ent




-- | update helper
--
{-
updateDb
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistStore (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => Key val
  -> [Update val]
  -> ControlMA (HandlerT site IO) ()
  -}
updateDb :: (PersistEntityBackend record
             ~
             BaseBackend (YesodPersistBackend site),
             PersistEntity record, PersistStoreWrite (YesodPersistBackend site),
             YesodPersist site) =>
            Key record -> [Update record] -> ControlMA (HandlerT site IO) ()
updateDb key update_values = _runDB $ update key update_values



{-
updateDbE
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistStore (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => Key val
  -> [Update val]
  -> ControlMA (HandlerT site IO) (ErrorEff ())
  -}
updateDbE :: (PersistEntityBackend record
              ~
              BaseBackend (YesodPersistBackend site),
              YesodPersist site, PersistStoreWrite (YesodPersistBackend site),
              PersistEntity record) =>
             Key record
             -> [Update record]
             -> ControlMA (HandlerT site IO) (ErrorEff ())
updateDbE key update_values = Right <$> updateDb key update_values



-- | updateWhere helper
--
{- updateWhereDb
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> [Update val]
  -> ControlMA (HandlerT site IO) ()
  -}
updateWhereDb :: (PersistEntityBackend record
                  ~
                  BaseBackend (YesodPersistBackend site),
                  PersistEntity record, PersistQueryWrite (YesodPersistBackend site),
                  YesodPersist site) =>
                 [Filter record]
                 -> [Update record] -> ControlMA (HandlerT site IO) ()
updateWhereDb filt query = _runDB $ updateWhere filt query



{-
updateWhereDbE
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> [Update val]
  -> ControlMA (HandlerT site IO) (ErrorEff ())
  -}
updateWhereDbE :: (PersistEntityBackend record
                   ~
                   BaseBackend (YesodPersistBackend site),
                   YesodPersist site, PersistQueryWrite (YesodPersistBackend site),
                   PersistEntity record) =>
                  [Filter record]
                  -> [Update record]
                  -> ControlMA (HandlerT site IO) (ErrorEff ())
updateWhereDbE filt query = Right <$> updateWhereDb filt query



-- | deleteWhere helper
--
{-
deleteWhereDb
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> ControlMA (HandlerT site IO) ()
  -}
deleteWhereDb :: (PersistEntityBackend record
                  ~
                  BaseBackend (YesodPersistBackend site),
                  PersistEntity record, PersistQueryWrite (YesodPersistBackend site),
                  YesodPersist site) =>
                  [Filter record] -> ControlMA (HandlerT site IO) ()
deleteWhereDb filt = _runDB $ deleteWhere filt



{-
deleteWhereDbE
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> ControlMA (HandlerT site IO) (ErrorEff ())
  -}
deleteWhereDbE :: (PersistEntityBackend record
                   ~
                   BaseBackend (YesodPersistBackend site),
                   YesodPersist site, PersistQueryWrite (YesodPersistBackend site),
                   PersistEntity record) =>
                  [Filter record]
                  -> ControlMA (HandlerT site IO) (ErrorEff ())
deleteWhereDbE filt = Right <$> deleteWhereDb filt



-- | deleteCascade helper
--
{- deleteCascadeDb
  :: forall site record.
     (YesodPersist site,
     DeleteCascade record (PersistEntityBackend record),
     PersistEntityBackend record ~ YesodPersistBackend site)
  => Key record
  -> ControlMA (HandlerT site IO) ()
  -}
deleteCascadeDb :: (YesodPersist site,
                    DeleteCascade record (YesodPersistBackend site)) =>
                   Key record -> ControlMA (HandlerT site IO) ()
deleteCascadeDb entity = _runDB $ deleteCascade entity



{-
deleteCascadeDbE
  :: forall site record.
     (YesodPersist site,
     DeleteCascade record (PersistEntityBackend record),
     PersistEntityBackend record ~ YesodPersistBackend site)
  => Key record
  -> ControlMA (HandlerT site IO) (ErrorEff ())
  -}
deleteCascadeDbE :: (YesodPersist site,
                     DeleteCascade record (YesodPersistBackend site)) =>
                    Key record
                    -> ControlMA (HandlerT site IO) (ErrorEff ())
deleteCascadeDbE entity = Right <$> deleteCascadeDb entity



-- | deleteCascadeWhere helper
--
{- deleteCascadeWhereDb
  :: forall site record.
     (PersistQuery (PersistEntityBackend record),
     DeleteCascade record (PersistEntityBackend record),
     YesodPersist site,
     PersistEntityBackend record ~ YesodPersistBackend site)
  => [Filter record]
  -> ControlMA (HandlerT site IO) ()
  -}
deleteCascadeWhereDb :: (PersistQueryWrite
                           (YesodPersistBackend site),
                         YesodPersist site,
                         DeleteCascade record (YesodPersistBackend site)) =>
                        [Filter record] -> ControlMA (HandlerT site IO) ()
deleteCascadeWhereDb = _runDB . deleteCascadeWhere



{-
deleteCascadeWhereDbE
  :: forall site record.
     (PersistQuery (PersistEntityBackend record),
     DeleteCascade record (PersistEntityBackend record),
     YesodPersist site,
     PersistEntityBackend record ~ YesodPersistBackend site)
  => [Filter record]
  -> ControlMA (HandlerT site IO) (ErrorEff ())
  -}
deleteCascadeWhereDbE :: (DeleteCascade
                            record (YesodPersistBackend site),
                          YesodPersist site, PersistQueryWrite (YesodPersistBackend site)) =>
                         [Filter record]
                         -> ControlMA (HandlerT site IO) (ErrorEff ())
deleteCascadeWhereDbE filt = Right <$> deleteCascadeWhereDb filt



-- | delete helper
--
{-
deleteDb
  :: forall site val. (PersistEntity val, PersistStore (PersistEntityBackend val), YesodPersist site, PersistEntityBackend val ~ YesodPersistBackend site)
  => Key val
  -> ControlMA (HandlerT site IO) ()
  -}
deleteDb :: (PersistEntityBackend record
             ~
             BaseBackend (YesodPersistBackend site),
             PersistEntity record, PersistStoreWrite (YesodPersistBackend site),
             YesodPersist site) =>
            Key record -> ControlMA (HandlerT site IO) ()
deleteDb = _runDB . delete



{-
deleteDbE
  :: forall site val.
     (PersistEntity val,
     PersistStore (PersistEntityBackend val),
     YesodPersist site,
     PersistEntityBackend val ~ YesodPersistBackend site)
  => Key val
  -> ControlMA (HandlerT site IO) (ErrorEff ())
  -}
deleteDbE :: (PersistEntityBackend record
              ~
              BaseBackend (YesodPersistBackend site),
              YesodPersist site, PersistStoreWrite (YesodPersistBackend site),
              PersistEntity record) =>
             Key record
             -> ControlMA (HandlerT site IO) (ErrorEff ())
deleteDbE k = Right <$> deleteDb k



-- | count helper
--
{-
countDb
  :: forall site val.
     (PersistEntity val,
     PersistQuery (YesodPersistBackend site),
     YesodPersist site,
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> ControlMA (HandlerT site IO) Int
  -}
countDb :: (PersistEntityBackend record
            ~
            BaseBackend (YesodPersistBackend site),
            PersistEntity record, PersistQueryRead (YesodPersistBackend site),
            YesodPersist site) =>
           [Filter record] -> ControlMA (HandlerT site IO) Int
countDb query = _runDB $ count query



{-
countDbE
  :: forall site val.
     (PersistEntity val,
     PersistQuery (YesodPersistBackend site),
     YesodPersist site,
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> ControlMA (HandlerT site IO) (ErrorEff Int)
  -}
countDbE :: (PersistEntityBackend record
             ~
             BaseBackend (YesodPersistBackend site),
             YesodPersist site, PersistQueryRead (YesodPersistBackend site),
             PersistEntity record) =>
            [Filter record]
            -> ControlMA (HandlerT site IO) (ErrorEff Int)
countDbE query = Right <$> countDb query
