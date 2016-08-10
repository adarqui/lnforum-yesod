{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}

module LN.Misc.Codec (
  module A,
  textToKey,
  textToKey',
  textToKeys,
  textToKeys',
  bscToKey,
  bscToKey',
  bscToKeys,
  bscToKeys',
  int64ToKey,
  int64ToKey',
  int64ToKeyMaybe,
  int64ToKeys,
  int64ToKeyBSC,
  int64ToKeyBSC',
  int64ToKeyBSCMaybe,
  int64ToKeysBSC,
  keyToInt64,
  keyToInt64Sbs,
  entityKeyToInt64,
  entityKeyToId
) where



import Data.String.Conversions
import Data.Either.Combinators (mapLeft)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.Either           (rights)
import           Data.Int              (Int64)
import           Data.Text             (Text)
import           Database.Persist
import           Database.Persist.Sql  (SqlBackend, unSqlBackendKey)
import           LN.Lib.Codec          as A
import           Prelude



textToKey :: PersistEntity record => Text -> Either Text (Key record)
textToKey = int64ToKey . textToInt64



textToKey' :: PersistEntity record => Text -> Key record
textToKey' = int64ToKey' . textToInt64



textToKeys :: PersistEntity record => Text -> Either Text [Key record]
textToKeys = undefined



textToKeys' :: PersistEntity record => Text -> [Key record]
textToKeys' = map int64ToKey' . tread



bscToKey :: PersistEntity record => ByteString -> Either ByteString (Key record)
bscToKey = int64ToKeyBSC . bscToInt64



bscToKey' :: PersistEntity record => ByteString -> Key record
bscToKey' = int64ToKeyBSC' . bscToInt64



bscToKeys :: PersistEntity record => ByteString -> Either ByteString [Key record]
bscToKeys = undefined



bscToKeys' :: PersistEntity record => ByteString -> [Key record]
bscToKeys' = map int64ToKeyBSC' . bread



keyFromValuesBSC :: PersistEntity record => [PersistValue] -> Either ByteString (Key record)
keyFromValuesBSC = mapLeft cs . keyFromValues



int64ToKey :: PersistEntity record => Int64 -> Either Text (Key record)
int64ToKey i64 = keyFromValues [PersistInt64 i64]



int64ToKeyMaybe :: PersistEntity record => Int64 -> Maybe (Key record)
int64ToKeyMaybe i64 = case (int64ToKey i64) of
  Left _   -> Nothing
  Right v  -> Just v



int64ToKey' :: PersistEntity record => Int64 -> Key record
int64ToKey' i64 = key
  where
  (Right key) = keyFromValues [PersistInt64 i64]



int64ToKeys :: PersistEntity record => [Int64] -> [Key record]
int64ToKeys = rights . map (\i64 -> keyFromValues [PersistInt64 i64])



int64ToKeyBSC :: PersistEntity record => Int64 -> Either ByteString (Key record)
int64ToKeyBSC i64 = keyFromValuesBSC [PersistInt64 i64]



int64ToKeyBSCMaybe :: PersistEntity record => Int64 -> Maybe (Key record)
int64ToKeyBSCMaybe i64 = case (int64ToKeyBSC i64) of
  Left _   -> Nothing
  Right v  -> Just v



int64ToKeyBSC' :: PersistEntity record => Int64 -> Key record
int64ToKeyBSC' i64 = key
  where
  (Right key) = keyFromValues [PersistInt64 i64]



int64ToKeysBSC :: PersistEntity record => [Int64] -> [Key record]
int64ToKeysBSC = rights . map (\i64 -> keyFromValues [PersistInt64 i64])



keyToInt64 :: forall record. ToBackendKey SqlBackend record => Key record -> Int64
keyToInt64 = unSqlBackendKey . toBackendKey



keyToInt64Sbs :: forall record. ToBackendKey SqlBackend record => Key record -> BSC.ByteString
keyToInt64Sbs = BSC.pack . show . keyToInt64



entityKeyToInt64 :: forall record. ToBackendKey SqlBackend record => Entity record -> Int64
entityKeyToInt64 = unSqlBackendKey . toBackendKey . entityKey



entityKeyToId :: forall record. ToBackendKey (PersistEntityBackend record) record => Key record -> BackendKey (PersistEntityBackend record)
entityKeyToId = toBackendKey
