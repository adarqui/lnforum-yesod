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
getForumPacksR = do
  user_id <- requireAuthId
  toJSON <$> getForumPacksM user_id



getForumPackR :: ForumId -> Handler Value
getForumPackR forum_id = do
  user_id <- requireAuthId
  toJSON <$> getForumPackM user_id forum_id



getForumPackH :: Text -> Handler Value
getForumPackH forum_name = do
  user_id <- requireAuthId
  toJSON <$> getForumPackMH user_id forum_name






--
-- Model
--

getForumPacksM :: UserId -> Handler ForumPackResponses
getForumPacksM user_id = do

  sp@StandardParams{..} <- lookupStandardParams

  case (spOrganizationId, spOrganizationName) of

    (Just org_id, _)             -> getForumPacks_ByOrganizationIdM user_id org_id sp
    (_          , Just org_name) -> getForumPacks_ByOrganizationNameM user_id org_name sp
    (_          , _)             -> notFound



getForumPackM :: UserId -> ForumId -> Handler ForumPackResponse
getForumPackM user_id forum_id = do

  forum         <- getForumM user_id forum_id
  getForumPack_ByForumM user_id forum



getForumPackMH :: UserId -> Text -> Handler ForumPackResponse
getForumPackMH user_id forum_name = do

  forum         <- getForumMH user_id forum_name
  getForumPack_ByForumM user_id forum



getForumPacks_ByOrganizationIdM :: UserId -> OrganizationId -> StandardParams -> Handler ForumPackResponses
getForumPacks_ByOrganizationIdM user_id org_id sp = do

  forums       <- getForums_ByOrganizationIdM user_id org_id sp
  forums_packs <- mapM (\forum -> getForumPack_ByForumM user_id forum) forums
  return $ ForumPackResponses {
    forumPackResponses = forums_packs
  }



getForumPacks_ByOrganizationNameM :: UserId -> Text -> StandardParams -> Handler ForumPackResponses
getForumPacks_ByOrganizationNameM user_id org_name sp = do

  forums       <- getForums_ByOrganizationNameM user_id org_name sp
  forums_packs <- mapM (\forum -> getForumPack_ByForumM user_id forum) forums
  return $ ForumPackResponses {
    forumPackResponses = forums_packs
  }



getForumPack_ByForumM :: UserId -> Entity Forum -> Handler ForumPackResponse
getForumPack_ByForumM user_id forum = do

  -- let sp = defaultStandardParams {
  --     spSortOrder = Just SortOrderBy_Dsc,
  --     spOrder     = Just OrderBy_ActivityAt,
  --     spLimit     = Just 1
  --   }

  forum_stats   <- getForumStatM user_id (entityKey forum)

  return $ ForumPackResponse {
    forumPackResponseForum            = forumToResponse forum,
    forumPackResponseForumId          = forum_id,
    forumPackResponseStat             = forum_stats,
    forumPackResponseLike             = Nothing,
    forumPackResponseStar             = Nothing,
    forumPackResponseWithOrganization = Nothing,
    forumPackResponsePermissions      = emptyPermissions
  }
  where
  forum_id = entityKeyToInt64 forum
