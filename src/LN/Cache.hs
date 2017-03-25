{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE ExplicitForAll #-}

module LN.Cache (
  Cache (..),
  CacheEntry (..),
  defaultCache,
  emptyCache
--  cacheRun
) where



import qualified Data.Map                as M
import           LN.Import



data CacheEntry a
  = CacheEntry a
  | CacheMissing



type CacheMap a b = M.Map a (CacheEntry b)



data Cache = Cache {
  cacheMe                :: !(Maybe User),
  cacheUsers             :: !(CacheMap UserId (Entity User))
}



emptyCache :: forall a b. CacheMap a b
emptyCache = M.empty



defaultCache :: Cache
defaultCache = Cache {
  cacheMe                = Nothing,
  cacheUsers             = emptyCache
}
