{-# LANGUAGE ExplicitForAll #-}

module LN.Cache (
  LN.Cache (..),
  LN.CacheEntry (..),
  defaultCache,
  emptyCache
) where



import           Control.Monad.Trans.RWS
import qualified Data.Map                as M
import           LN.Import
import           LN.Model.Misc
import           LN.T.Membership
import           LN.T.Visibility



data CacheEntry a
  = CacheEntry a
  | CacheMissing



type CacheMap a b = M.Map a (CacheEntry b)



data Cache = Cache {
  cacheMe            :: Maybe User,
  cacheOrganizations :: CacheMap OrganizationId Organization,
  cacheUsers         :: CacheMap UserId User,
  cacheForums        :: CacheMap ForumId Forum,
  cacheBoards        :: CacheMap BoardId Board,
  cacheThreads       :: CacheMap ThreadId Thread,
  cacheThreadPosts   :: CacheMap ThreadPostId ThreadPost
}



emptyCache :: forall a b. CacheMap a b
emptyCache = M.empty



defaultCache :: Cache
defaultCache = Cache {
  cacheMe            = Nothing,
  cacheOrganizations = emptyCache,
  cacheUsers         = emptyCache,
  cacheForums        = emptyCache,
  cacheBoards        = emptyCache,
  cacheThreads       = emptyCache,
  cacheThreadPosts   = emptyCache
}
