{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Db (
  selectListDb',
  selectListDbEither,
  selectKeysListDbEither,
  selectFirstDbEither,
  selectFirstDb',
  insertDbEither,
  insertEntityDbEither,
  updateDb,
  updateWhereDb,
  deleteWhereDbEither,
  deleteCascadeDbEither,
  deleteCascadeWhereDbEither,
  deleteDbEither,
  countDb
) where



import           Control
import           Api.Params            (StandardParams, spToSelect, spToSelectMay, lookupStandardParams)
import           Data.List             (nub)
import           Data.Time             ()
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Database.Esqueleto    as E
import           Import
import           Lifted
import           Misc.Codec
import           LN.T.Ent              (Ent (..))
import           LN.T.Error            (ApplicationError (..))
import           LN.T.Param



-- | selectList query helper
--
selectListDb' :: forall site val typ.
  (PersistEntity val, PersistQuery (PersistEntityBackend val),
  YesodPersist site,
  PersistEntityBackend val ~ YesodPersistBackend site) =>
  Maybe StandardParams
  -> [Filter val]
  -> [SelectOpt val]
  -> EntityField val typ
  -> ControlMA (HandlerT site IO) [Entity val]
selectListDb' m_sp query filt field = do
  _runDB $ selectList query ((spToSelectMay m_sp field) <> filt)



selectListDbEither :: forall site val typ.
  (PersistEntity val, PersistQuery (PersistEntityBackend val),
  YesodPersist site,
  PersistEntityBackend val ~ YesodPersistBackend site) =>
  Maybe StandardParams
  -> [Filter val]
  -> [SelectOpt val]
  -> EntityField val typ
  -> ControlMA (HandlerT site IO) (ErrorEff [Entity val])
selectListDbEither m_sp query filt field = do
  Right <$> selectListDb' m_sp query filt field



-- | selectKeysList query helper
--
selectKeysListDb :: forall site val typ.
                    (PersistEntity val, YesodPersist site,
                     PersistQuery (PersistEntityBackend val),
                     PersistEntityBackend val ~ YesodPersistBackend site) =>
                    StandardParams
                    -> [Filter val]
                    -> [SelectOpt val]
                    -> EntityField val typ
                    -> ControlMA (HandlerT site IO) [Key val]
selectKeysListDb sp query filt field = do
  _runDB $ selectKeysList query ((spToSelect sp field) <> filt)



selectKeysListDbMay :: forall site val typ.
                    (PersistEntity val, YesodPersist site,
                     PersistQuery (PersistEntityBackend val),
                     PersistEntityBackend val ~ YesodPersistBackend site) =>
                    Maybe StandardParams
                    -> [Filter val]
                    -> [SelectOpt val]
                    -> EntityField val typ
                    -> ControlMA (HandlerT site IO) [Key val]
selectKeysListDbMay m_sp query filt field = do
  _runDB $ selectKeysList query ((spToSelectMay m_sp field) <> filt)



selectKeysListDbEither :: forall site val typ.
                    (PersistEntity val, YesodPersist site,
                     PersistQuery (PersistEntityBackend val),
                     PersistEntityBackend val ~ YesodPersistBackend site) =>
                    Maybe StandardParams
                    -> [Filter val]
                    -> [SelectOpt val]
                    -> EntityField val typ
                    -> ControlMA (HandlerT site IO) (ErrorEff [Key val])
selectKeysListDbEither m_sp query filt field = do
  Right <$> (_runDB $ selectKeysList query ((spToSelectMay m_sp field) <> filt))



-- | selectKeysListDb' helper
--
selectKeysListDb' :: forall val typ.
                     (PersistEntity val, PersistQuery (PersistEntityBackend val),
                      PersistEntityBackend val ~ SqlBackend) =>
                     [Filter val]
                     -> [SelectOpt val]
                     -> EntityField val typ
                     -> ControlMA (HandlerT App IO) [Key val]
selectKeysListDb' query filt field = do
  sp <- lookupStandardParams
  selectKeysListDb sp query filt field



-- | selectFirst query helper
--
selectFirstDb' :: forall site val.
                 (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val]
                 -> [SelectOpt val] -> ControlMA (HandlerT site IO) (Maybe (Entity val))
selectFirstDb' query filt = _runDB $ selectFirst query filt



selectFirstDbEither :: forall site val.
                 (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val]
                 -> [SelectOpt val] -> ControlMA (HandlerT site IO) (ErrorEff (Entity val))
selectFirstDbEither query filt = do
  m <- selectFirstDb' query filt
  case m of
    Nothing -> pure $ Left Error_NotFound
    Just v  -> pure $ Right v



-- | insert helper
--
insertDb :: forall a site.
            (PersistEntity a, YesodPersist site,
             PersistStore (PersistEntityBackend a),
             PersistEntityBackend a ~ YesodPersistBackend site) =>
            a -> ControlMA (HandlerT site IO) (Key a)
insertDb = _runDB . insert



insertDbEither :: forall a site.
            (PersistEntity a, YesodPersist site,
             PersistStore (PersistEntityBackend a),
             PersistEntityBackend a ~ YesodPersistBackend site) =>
            a -> ControlMA (HandlerT site IO) (ErrorEff (Key a))
insertDbEither ent = Right <$> (_runDB $ insert ent)



-- | insertEntity helper
--
insertEntityDb' :: forall site e.
                  (PersistEntity e, YesodPersist site,
                   PersistStore (YesodPersistBackend site),
                   YesodPersistBackend site ~ PersistEntityBackend e) =>
                  e -> ControlMA (HandlerT site IO) (Entity e)
insertEntityDb' entity = _runDB $ insertEntity entity



insertEntityDbEither :: forall site e.
                  (PersistEntity e, YesodPersist site,
                   PersistStore (YesodPersistBackend site),
                   YesodPersistBackend site ~ PersistEntityBackend e) =>
                  e -> ControlMA (HandlerT site IO) (ErrorEff (Entity e))
insertEntityDbEither entity = Right <$> insertEntityDb' entity



-- | update helper
--
updateDb :: forall site val.
            (PersistEntity val, YesodPersist site,
             PersistStore (YesodPersistBackend site),
             YesodPersistBackend site ~ PersistEntityBackend val) =>
            Key val -> [Update val] -> ControlMA (HandlerT site IO) ()
updateDb key update_values = _runDB $ update key update_values



-- | updateWhere helper
--
updateWhereDb :: forall site val.
                 (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val] -> [Update val] -> ControlMA (HandlerT site IO) ()
updateWhereDb filt query = _runDB $ updateWhere filt query



-- | deleteWhere helper
--
deleteWhereDb :: forall site val.
                 (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val] -> ControlMA (HandlerT site IO) ()
deleteWhereDb filt = _runDB $ deleteWhere filt



deleteWhereDbEither :: forall site val.
                 (PersistEntity val, YesodPersist site,
                  PersistQuery (YesodPersistBackend site),
                  YesodPersistBackend site ~ PersistEntityBackend val) =>
                 [Filter val] -> ControlMA (HandlerT site IO) (ErrorEff ())
deleteWhereDbEither filt = Right <$> (_runDB $ deleteWhere filt)



-- | deleteCascade helper
--
deleteCascadeDb :: forall site record.
                  (YesodPersist site,
                   DeleteCascade record (PersistEntityBackend record),
                   PersistEntityBackend record ~ YesodPersistBackend site) =>
                  Key record -> ControlMA (HandlerT site IO) ()
deleteCascadeDb entity = _runDB $ deleteCascade entity



deleteCascadeDbEither :: forall site record.
                  (YesodPersist site,
                   DeleteCascade record (PersistEntityBackend record),
                   PersistEntityBackend record ~ YesodPersistBackend site) =>
                  Key record -> ControlMA (HandlerT site IO) (ErrorEff ())
deleteCascadeDbEither entity = Right <$> (_runDB $ deleteCascade entity)



-- | deleteCascadeWhere helper
--
deleteCascadeWhereDb :: forall site record.
  (PersistQuery (PersistEntityBackend record),
  DeleteCascade record (PersistEntityBackend record),
  YesodPersist site,
  PersistEntityBackend record ~ YesodPersistBackend site) =>
  [Filter record]
  -> ControlMA (HandlerT site IO) ()
deleteCascadeWhereDb = _runDB . deleteCascadeWhere



deleteCascadeWhereDbEither :: forall site record.
  (PersistQuery (PersistEntityBackend record),
  DeleteCascade record (PersistEntityBackend record),
  YesodPersist site,
  PersistEntityBackend record ~ YesodPersistBackend site) =>
  [Filter record]
  -> ControlMA (HandlerT site IO) (ErrorEff ())
deleteCascadeWhereDbEither filt = Right <$> (_runDB $ deleteCascadeWhere filt)



deleteDb = _runDB . delete

deleteDbEither k = Right <$> deleteDb k



-- | count helper
countDb :: forall site val.
  (PersistEntity val, PersistQuery (YesodPersistBackend site),
  YesodPersist site,
  YesodPersistBackend site ~ PersistEntityBackend val) =>
  [Filter val]
  -> ControlMA (HandlerT site IO) Int
countDb query = _runDB $ count query
