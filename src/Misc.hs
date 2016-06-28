module Misc (
  lookupGetParamId
) where



import           Import
import           Misc.Codec



-- | Lookup a key and convert it
--
lookupGetParamId :: forall (m :: * -> *) record.
                    (PersistEntity record, MonadHandler m) =>
                    Text -> m (Maybe (Key record))
lookupGetParamId t = do
  v <- lookupGetParam t
  case v of
    Nothing -> pure $ Nothing
    Just v' -> pure $ int64ToKeyMaybe (textToInt64 v')
