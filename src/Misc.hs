module Misc (
  lookupGetParamId
) where

import           Import

import           Misc.Codec

-- | Lookup a key and convert it
--
lookupGetParamId t = do
  v <- lookupGetParam t
  case v of
    Nothing -> return $ Nothing
    Just v' -> return $ int64ToKeyMaybe (textToInt64 v')
