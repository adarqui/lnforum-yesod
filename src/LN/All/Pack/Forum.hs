{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.Forum (
  -- Handler
  getForumPackR,

  -- Model
  getForumPackM,
) where



import           LN.All.Prelude
import           LN.All.Forum
import           LN.All.Internal



--
-- Handler
--

getForumPackR :: ForumId -> Handler Value
getForumPackR forum_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getForumPackM user_id forum_id



--
-- Model
--

getForumPackM :: UserId -> ForumId -> HandlerErrorEff ForumPackResponse
getForumPackM user_id forum_id = do

  e_forum <- getForumM user_id forum_id
  rehtie e_forum leftA $ \forum -> getForumPack_ByForumM user_id forum



getForumPack_ByForumM :: UserId -> Entity Forum -> HandlerErrorEff ForumPackResponse
getForumPack_ByForumM user_id forum@(Entity _ Forum{..}) = do

  e_forum_stats       <- getForumStatM user_id (entityKey forum)
  rehtie e_forum_stats leftA $ \forum_stats -> do

    user_perms_by_forum <- userPermissions_ByForumIdM user_id (entityKey forum)

    rightA $ ForumPackResponse {
      forumPackResponseForum            = forumToResponse forum,
      forumPackResponseStat             = forum_stats,
      forumPackResponsePermissions      = user_perms_by_forum
    }
    where
    forum_id = entityKeyToInt64 forum
