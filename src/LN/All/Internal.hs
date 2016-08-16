{-# LANGUAGE RecordWildCards #-}

module LN.All.Internal (
  getOrganizationM,
  getTeamM,
  getForumM
) where



import           Control.Monad.Trans.Either as A (EitherT, runEitherT)
import           Data.Ebyam                 as A (ebyam)
import           Data.Maybe                 as A (fromJust)
import           Data.Rehtie                as A (rehtie)
import qualified Data.Text                  as T

import           LN.Api.Params              as A
import           LN.Cache                   as A
import           LN.Cache.Internal
import           LN.Control                 as A
import           LN.Db                      as A
import           LN.Error                   as A
import           LN.Import                  as A
import           LN.Lib                     as A
import           LN.Lifted                  as A




getOrganizationM :: UserId -> OrganizationId -> HandlerErrorEff (Entity Organization)
getOrganizationM _ org_id = do
  m_c_organization <- getOrganizationC org_id
  cacheRun' m_c_organization $ do
    lr <- selectFirstDbE [OrganizationId ==. org_id, OrganizationActive ==. True] []
    rehtie
      lr
      (\err          -> putOrganizationC org_id CacheMissing *> leftA err)
      (\organization -> putOrganizationC org_id (CacheEntry organization) *> rightA organization)



getTeamM :: UserId -> TeamId -> HandlerErrorEff (Entity Team)
getTeamM _ team_id = do
  m_c_team <- getTeamC team_id
  cacheRun' m_c_team $ do
    lr <- selectFirstDbE [TeamId ==. team_id, TeamActive ==. True] []
    rehtie
      lr
      (\err  -> putTeamC team_id CacheMissing *> leftA err)
      (\team -> putTeamC team_id (CacheEntry team) *> rightA team)



getForumM :: UserId -> ForumId -> HandlerErrorEff (Entity Forum)
getForumM _ forum_id = do
  m_c_forum <- getForumC forum_id
  cacheRun' m_c_forum $ do
    lr <- selectFirstDbE [ForumId ==. forum_id, ForumActive ==. True] []
    rehtie
      lr
      (\err   -> putForumC forum_id CacheMissing *> leftA err)
      (\forum -> putForumC forum_id (CacheEntry forum) *> rightA forum)
