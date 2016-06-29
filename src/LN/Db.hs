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
  insertEntityDb,
  insertEntityDbE,
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



import           Data.List             (nub)
import           Data.Time             ()
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Database.Esqueleto    as E
import           LN.Api.Params         (StandardParams, lookupStandardParams,
                                        spToSelect, spToSelectMay)
import           LN.Control
import           LN.Import
import           LN.Lifted
import           LN.Misc.Codec
import           LN.T.Ent              (Ent (..))
import           LN.T.Error            (ApplicationError (..))
import           LN.T.Param



-- | selectList query helper
--
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
selectListDb m_sp query filt field = do
  _runDB $ selectList query ((spToSelectMay m_sp field) <> filt)



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
selectListDbE m_sp query filt field = Right <$> selectListDb m_sp query filt field



-- | selectKeysList query helper
--
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
selectKeysListDb m_sp query filt field = do
  _runDB $ selectKeysList query ((spToSelectMay m_sp field) <> filt)



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
selectKeysListDbE m_sp query filt field = Right <$> selectKeysListDb m_sp query filt field




-- | selectFirst query helper
--
selectFirstDb
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> [SelectOpt val]
  -> ControlMA (HandlerT site IO) (Maybe (Entity val))
selectFirstDb query filt = _runDB $ selectFirst query filt



selectFirstDbE
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> [SelectOpt val]
  -> ControlMA (HandlerT site IO) (ErrorEff (Entity val))
selectFirstDbE query filt = do
  m <- selectFirstDb query filt
  case m of
    Nothing -> pure $ Left Error_NotFound
    Just v  -> pure $ Right v



-- | insert helper
--
insertDb
  :: forall a site.
     (PersistEntity a,
     YesodPersist site,
     PersistStore (PersistEntityBackend a),
     PersistEntityBackend a ~ YesodPersistBackend site)
  => a
  -> ControlMA (HandlerT site IO) (Key a)
insertDb = _runDB . insert



insertDbE
  :: forall a site.
     (PersistEntity a,
     YesodPersist site,
     PersistStore (PersistEntityBackend a),
     PersistEntityBackend a ~ YesodPersistBackend site)
     => a
     -> ControlMA (HandlerT site IO) (ErrorEff (Key a))
insertDbE ent = Right <$> insertDb ent



-- | insertEntity helper
--
insertEntityDb
  :: forall site e.
     (PersistEntity e,
     YesodPersist site,
     PersistStore (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend e)
  => e
  -> ControlMA (HandlerT site IO) (Entity e)
insertEntityDb entity = _runDB $ insertEntity entity



insertEntityDbE
  :: forall site e.
     (PersistEntity e,
     YesodPersist site,
     PersistStore (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend e)
  => e
  -> ControlMA (HandlerT site IO) (ErrorEff (Entity e))
insertEntityDbE entity = Right <$> insertEntityDb entity



-- | update helper
--
updateDb
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistStore (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => Key val
  -> [Update val]
  -> ControlMA (HandlerT site IO) ()
updateDb key update_values = _runDB $ update key update_values



updateDbE
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistStore (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => Key val
  -> [Update val]
  -> ControlMA (HandlerT site IO) (ErrorEff ())
updateDbE key update_values = Right <$> updateDb key update_values



-- | updateWhere helper
--
updateWhereDb
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> [Update val]
  -> ControlMA (HandlerT site IO) ()
updateWhereDb filt query = _runDB $ updateWhere filt query



updateWhereDbE
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> [Update val]
  -> ControlMA (HandlerT site IO) (ErrorEff ())
updateWhereDbE filt query = Right <$> updateWhereDb filt query



-- | deleteWhere helper
--
deleteWhereDb
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> ControlMA (HandlerT site IO) ()
deleteWhereDb filt = _runDB $ deleteWhere filt



deleteWhereDbE
  :: forall site val.
     (PersistEntity val,
     YesodPersist site,
     PersistQuery (YesodPersistBackend site),
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> ControlMA (HandlerT site IO) (ErrorEff ())
deleteWhereDbE filt = Right <$> deleteWhereDb filt



-- | deleteCascade helper
--
deleteCascadeDb
  :: forall site record.
     (YesodPersist site,
     DeleteCascade record (PersistEntityBackend record),
     PersistEntityBackend record ~ YesodPersistBackend site)
  => Key record
  -> ControlMA (HandlerT site IO) ()
deleteCascadeDb entity = _runDB $ deleteCascade entity



deleteCascadeDbE
  :: forall site record.
     (YesodPersist site,
     DeleteCascade record (PersistEntityBackend record),
     PersistEntityBackend record ~ YesodPersistBackend site)
  => Key record
  -> ControlMA (HandlerT site IO) (ErrorEff ())
deleteCascadeDbE entity = Right <$> deleteCascadeDb entity



-- | deleteCascadeWhere helper
--
deleteCascadeWhereDb
  :: forall site record.
     (PersistQuery (PersistEntityBackend record),
     DeleteCascade record (PersistEntityBackend record),
     YesodPersist site,
     PersistEntityBackend record ~ YesodPersistBackend site)
  => [Filter record]
  -> ControlMA (HandlerT site IO) ()
deleteCascadeWhereDb = _runDB . deleteCascadeWhere



deleteCascadeWhereDbE
  :: forall site record.
     (PersistQuery (PersistEntityBackend record),
     DeleteCascade record (PersistEntityBackend record),
     YesodPersist site,
     PersistEntityBackend record ~ YesodPersistBackend site)
  => [Filter record]
  -> ControlMA (HandlerT site IO) (ErrorEff ())
deleteCascadeWhereDbE filt = Right <$> deleteCascadeWhereDb filt



-- | delete helper
--
deleteDb = _runDB . delete



deleteDbE k = Right <$> deleteDb k



-- | count helper
--
countDb
  :: forall site val.
     (PersistEntity val,
     PersistQuery (YesodPersistBackend site),
     YesodPersist site,
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> ControlMA (HandlerT site IO) Int
countDb query = _runDB $ count query



countDbE
  :: forall site val.
     (PersistEntity val,
     PersistQuery (YesodPersistBackend site),
     YesodPersist site,
     YesodPersistBackend site ~ PersistEntityBackend val)
  => [Filter val]
  -> ControlMA (HandlerT site IO) (ErrorEff Int)
countDbE query = Right <$> countDb query
