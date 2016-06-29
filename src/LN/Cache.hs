{-# LANGUAGE ExplicitForAll #-}

module LN.Cache (
  LN.Cache (..),
  LN.CacheEntry (..),
  defaultCache,
  emptyCache
) where



import           Import
import Control.Monad.Trans.RWS
import           LN.Model.Misc
import           LN.T.Membership
import           LN.T.Visibility
import qualified Data.Map as M



data LN.CacheEntry a
  = LN.CacheEntry a
  | LN.CacheMissing



type LN.CacheMap a b = M.Map a (CacheEntry b)


data LN.Cache = LN.Cache {
  cacheMe            :: Maybe User,
  cacheOrganizations  :: LN.CacheMap OrganizationId Organization,
  cacheUsers          :: LN.CacheMap UserId User,
  cacheForums         :: LN.CacheMap ForumId Forum,
  cacheBoards         :: LN.CacheMap BoardId Board,
  cacheThreads        :: LN.CacheMap ThreadId Thread,
  cacheThreadPosts    :: LN.CacheMap ThreadPostId ThreadPost
}



emptyCache :: forall a b. LN.CacheMap a b
emptyCache = M.empty



defaultCache :: LN.Cache
defaultCache = LN.Cache {
  cacheMe = Nothing,
  cacheOrganizations = emptyCache,
  cacheUsers = emptyCache,
  cacheForums = emptyCache,
  cacheBoards = emptyCache,
  cacheThreads = emptyCache,
  cacheThreadPosts = emptyCache
}
