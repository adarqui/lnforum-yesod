module LN.All.Pack.Forum (
  -- Handler
  getForumPacksR,
  getForumPackR,
  getForumPackH,

  -- Model
  getForumPacksM,
  getForumPackM,
  getForumPackMH,
) where



import           LN.All.Prelude
import           LN.All.Forum



--
-- Handler
--

getForumPacksR :: Handler Value
getForumPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getForumPacksM (pure sp) user_id



getForumPackR :: ForumId -> Handler Value
getForumPackR forum_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getForumPackM user_id forum_id



getForumPackH :: Text -> Handler Value
getForumPackH forum_name = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getForumPackMH (pure sp) user_id forum_name






--
-- Model
--

getForumPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff ForumPackResponses
getForumPacksM m_sp user_id = do

  case (lookupSpMay m_sp spOrganizationId) of

    Just org_id   -> getForumPacks_ByOrganizationIdM m_sp user_id org_id
    _             -> left $ Error_InvalidArguments "organization_id"



getForumPackM :: UserId -> ForumId -> HandlerErrorEff ForumPackResponse
getForumPackM user_id forum_id = do

  e_forum <- getForumM user_id forum_id
  rehtie e_forum left $ \forum -> getForumPack_ByForumM user_id forum



getForumPackMH :: Maybe StandardParams -> UserId -> Text -> HandlerErrorEff ForumPackResponse
getForumPackMH m_sp user_id forum_name = do

  e_forum <- getForumMH m_sp user_id forum_name
  rehtie e_forum left $ \forum -> getForumPack_ByForumM user_id forum



getForumPacks_ByOrganizationIdM :: Maybe StandardParams -> UserId -> OrganizationId -> HandlerErrorEff ForumPackResponses
getForumPacks_ByOrganizationIdM m_sp user_id org_id = do

  e_forums       <- getForums_ByOrganizationIdM m_sp user_id org_id
  rehtie e_forums left $ \forums -> do
    forums_packs <- fmap rights (mapM (\forum -> getForumPack_ByForumM user_id forum) forums)
    right $ ForumPackResponses {
      forumPackResponses = forums_packs
    }



getForumPack_ByForumM :: UserId -> Entity Forum -> HandlerErrorEff ForumPackResponse
getForumPack_ByForumM user_id forum@(Entity _ Forum{..}) = do

  e_forum_stats       <- getForumStatM user_id (entityKey forum)
  rehtie e_forum_stats left $ \forum_stats -> do

    user_perms_by_forum <- userPermissions_ByForumIdM user_id (entityKey forum)

    right $ ForumPackResponse {
      forumPackResponseForum            = forumToResponse forum,
      forumPackResponseForumId          = forum_id,
      forumPackResponseStat             = forum_stats,
      forumPackResponseLike             = Nothing,
      forumPackResponseStar             = Nothing,
      forumPackResponseWithOrganization = Nothing,
      forumPackResponsePermissions      = user_perms_by_forum
    }
    where
    forum_id = entityKeyToInt64 forum
