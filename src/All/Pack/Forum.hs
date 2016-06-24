module All.Pack.Forum (
  -- Handler
  getForumPacksR,
  getForumPackR,
  getForumPackH,

  -- Model
  getForumPacksM,
  getForumPackM,
  getForumPackMH,
) where



import           All.Prelude
import           All.Forum



--
-- Handler
--

getForumPacksR :: Handler Value
getForumPacksR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getForumPacksM user_id



getForumPackR :: ForumId -> Handler Value
getForumPackR forum_id = run $ do
  user_id <- _requireAuthId
  toJSON <$> getForumPackM user_id forum_id



getForumPackH :: Text -> Handler Value
getForumPackH forum_name = run $ do
  user_id <- _requireAuthId
  toJSON <$> getForumPackMH user_id forum_name






--
-- Model
--

getForumPacksM :: UserId -> HandlerEff ForumPackResponses
getForumPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case spOrganizationId of

    Just org_id   -> getForumPacks_ByOrganizationIdM user_id org_id sp
    _             -> notFound



getForumPackM :: UserId -> ForumId -> HandlerEff ForumPackResponse
getForumPackM user_id forum_id = do

  forum         <- getForumM user_id forum_id
  getForumPack_ByForumM user_id forum



getForumPackMH :: UserId -> Text -> HandlerEff ForumPackResponse
getForumPackMH user_id forum_name = do

  forum         <- getForumMH user_id forum_name
  getForumPack_ByForumM user_id forum



getForumPacks_ByOrganizationIdM :: UserId -> OrganizationId -> StandardParams -> HandlerEff ForumPackResponses
getForumPacks_ByOrganizationIdM user_id org_id sp = do

  forums       <- getForums_ByOrganizationIdM user_id org_id sp
  forums_packs <- mapM (\forum -> getForumPack_ByForumM user_id forum) forums
  return $ ForumPackResponses {
    forumPackResponses = forums_packs
  }



getForumPack_ByForumM :: UserId -> Entity Forum -> HandlerEff ForumPackResponse
getForumPack_ByForumM user_id forum@(Entity forum_id Forum{..}) = do

  forum_stats         <- getForumStatM user_id (entityKey forum)
  user_perms_by_forum <- userPermissions_ByForumIdM user_id (entityKey forum)

  return $ ForumPackResponse {
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
