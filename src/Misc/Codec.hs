module Misc.Codec (
  module A,
  textToKey,
  textToKey',
  textToKeys,
  textToKeys',
  int64ToKey,
  int64ToKey',
  int64ToKeyMaybe,
  int64ToKeys,
  keyToInt64,
  keyToInt64Sbs,
  entityKeyToInt64,
  entityKeyToId
) where



import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import           Data.Either                (rights)
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import           Database.Persist
import           Database.Persist.Sql       (unSqlBackendKey)
import           LN.Lib.Codec               as A
import           Prelude



textToKey :: PersistEntity record => Text -> Either Text (Key record)
textToKey = int64ToKey . textToInt64



textToKey' :: PersistEntity record => Text -> Key record
textToKey' = int64ToKey' . textToInt64



textToKeys :: PersistEntity record => Text -> Either Text [Key record]
textToKeys = undefined



textToKeys' :: PersistEntity record => Text -> [Key record]
textToKeys' = map int64ToKey' . tread



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



-- keyToInt64 :: forall record.
--              ToBackendKey Database.Persist.Sql.Types.SqlBackend record =>
--              Key record -> Int64
keyToInt64 = unSqlBackendKey . toBackendKey



-- keyToInt64Sbs :: forall record.
--                 ToBackendKey Database.Persist.Sql.Types.SqlBackend record =>
--                 Key record -> ByteString
keyToInt64Sbs = BSC.pack . show . keyToInt64



entityKeyToInt64 = unSqlBackendKey . toBackendKey . entityKey



entityKeyToId = toBackendKey
