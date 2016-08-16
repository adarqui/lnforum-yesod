{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.GlobalGroup (
  -- LN.Handler
  getGlobalGroupPacksR,
  getGlobalGroupPackR,
  getGlobalGroupPackH,

  -- LN.Model

) where



import           LN.All.Internal
import           LN.All.GlobalGroup
import           LN.All.Prelude
import           LN.All.User



--
-- LN.Handler
--

getGlobalGroupPacksR :: Handler Value
getGlobalGroupPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getGlobalGroupPacksM (pure sp) user_id



getGlobalGroupPackR :: GlobalGroupId -> Handler Value
getGlobalGroupPackR global_group_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getGlobalGroupPackM user_id global_group_id



getGlobalGroupPackH :: Text -> Handler Value
getGlobalGroupPackH global_group_name = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getGlobalGroupPackMH user_id global_group_name







-- LN.Model

getGlobalGroupPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff GlobalGroupPackResponses
getGlobalGroupPacksM m_sp user_id = do

  case (lookupSpMay m_sp spUserId) of

    Just lookup_user_id -> getGlobalGroupPacks_ByUserIdM m_sp user_id lookup_user_id
    _                   -> getGlobalGroupPacks_ByEverythingM m_sp user_id



getGlobalGroupPackM :: UserId -> GlobalGroupId -> HandlerErrorEff GlobalGroupPackResponse
getGlobalGroupPackM user_id global_group_id = do

  e_global_group <- getGlobalGroupM user_id global_group_id
  rehtie e_global_group leftA $ getGlobalGroupPack_ByGlobalGroupM user_id



getGlobalGroupPackMH :: UserId -> Text -> HandlerErrorEff GlobalGroupPackResponse
getGlobalGroupPackMH user_id global_group_name = do

  e_global_group <- getGlobalGroupMH user_id global_group_name
  rehtie e_global_group leftA $ getGlobalGroupPack_ByGlobalGroupM user_id



getGlobalGroupPacks_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff GlobalGroupPackResponses
getGlobalGroupPacks_ByEverythingM m_sp user_id = do
  e_global_groups <- getGlobalGroups_ByEverythingM m_sp user_id
  rehtie e_global_groups leftA $ \global_groups -> do
    global_groups_packs <- rights <$> mapM (\global_group -> getGlobalGroupPack_ByGlobalGroupM user_id global_group) global_groups
    rightA $ GlobalGroupPackResponses {
      globalGroupPackResponses = global_groups_packs
    }



getGlobalGroupPacks_ByUserIdM :: Maybe StandardParams -> UserId -> UserId -> HandlerErrorEff GlobalGroupPackResponses
getGlobalGroupPacks_ByUserIdM m_sp user_id lookup_user_id = do

  e_global_groups <- getGlobalGroups_ByUserIdM m_sp user_id lookup_user_id
  rehtie e_global_groups leftA $ \global_groups -> do
    global_groups_packs <- rights <$> mapM (\global_group -> getGlobalGroupPack_ByGlobalGroupM user_id global_group) global_groups
    rightA $ GlobalGroupPackResponses {
      globalGroupPackResponses = global_groups_packs
    }



getGlobalGroupPack_ByGlobalGroupM :: UserId -> Entity GlobalGroup -> HandlerErrorEff GlobalGroupPackResponse
getGlobalGroupPack_ByGlobalGroupM user_id global_group@(Entity global_group_id GlobalGroup{..}) = do

  lr <- runEitherT $ do
    global_group_user    <- mustT $ getUserM user_id globalGroupUserId
    global_group_stats   <- mustT $ getGlobalGroupStatM user_id (entityKey global_group)
    pure (global_group_user, global_group_stats)

  rehtie lr leftA $ \(global_group_user, global_group_stats) -> do
    rightA $ GlobalGroupPackResponse {
      globalGroupPackResponseUser          = userToSanitizedResponse global_group_user,
      globalGroupPackResponseUserId        = entityKeyToInt64 global_group_user,
      globalGroupPackResponseGlobalGroup   = globalGroupToResponse global_group,
      globalGroupPackResponseGlobalGroupId = keyToInt64 global_group_id,
      globalGroupPackResponseStat          = global_group_stats,
      globalGroupPackResponsePermissions   = emptyPermissions
    }
