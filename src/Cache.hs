module Cache (
  Cache (..),
  CacheEntry (..)
) where



import           Import
import           Api.Params
import Control.Monad.Trans.RWS
import           Model.Misc
import           LN.T.Membership
import           LN.T.Visibility
import qualified Data.Map as M



data CacheEntry a
  = CacheEntry a
  | CacheMissing



data Cache = Cache {
  cacheMe            :: Maybe User,
  cacheOrganization  :: Maybe Organization,
  cacheUser          :: M.Map UserId (CacheEntry User),
  cacheForum         :: Maybe Forum,
  cacheBoard         :: Maybe Board,
  cacheThread        :: Maybe Thread,
  cacheThreadPost    :: Maybe ThreadPost
}
