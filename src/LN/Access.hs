{-# LANGUAGE RecordWildCards #-}

module LN.Access (
  isSuperM,
  mustBe_SuperM,
  mustBe_SameUserM,
  mustBe_OwnerOf_ForumIdM,
  userPermissions_ByForumIdM
) where



import           Control.Monad.Trans.Either (runEitherT)
import           Data.Ebyam                 (ebyam)
import           Data.List                  (nub)

import           LN.All.Internal
import           LN.Control
import           LN.Generate.Permission     (allPermissions)
import           LN.Import
import           LN.T.Permission
import           LN.T.Visibility



-- | Queries appSuperUsers (from Foundation)
-- If our user_id is found within this list, consider us a super user.
--
isSuperM :: UserId -> HandlerEff Bool
isSuperM user_id = do
  super_users <- getsYesod appSuperUsers
  pure $ any (\(Entity _ Super{..}) -> user_id == superUserId) super_users



mustBe_SuperM :: UserId -> HandlerErrorEff ()
mustBe_SuperM user_id = do
  super_users <- getsYesod appSuperUsers
  if (not $ any (\(Entity _ Super{..}) -> user_id == superUserId) super_users)
     then leftA Error_PermissionDenied
     else rightA ()



mustBe_SameUserM :: UserId -> UserId -> HandlerErrorEff ()
mustBe_SameUserM user_id lookup_user_id = do
  if user_id == lookup_user_id
    then rightA ()
    else leftA Error_PermissionDenied



mustBe_OwnerOf_ForumIdM :: UserId -> ForumId -> HandlerErrorEff ()
mustBe_OwnerOf_ForumIdM user_id forum_id = do
  leftA Error_PermissionDenied
  -- runEitherT $ do
  --   (Entity _ Forum{..}) <- mustT $ getForumM user_id forum_id
  --   -- TODO FIXME: need access control
  --   leftA Error_PermissionDenied
  --   -- mustT $ mustBe_OwnerOf_OrganizationIdM user_id forumOrgId



userPermissions_ByForumIdM :: UserId -> ForumId -> HandlerEff Permissions
userPermissions_ByForumIdM user_id forum_id = do
  m_forum <- getForumMaybeM user_id forum_id
  ebyam m_forum (pure []) $ \(Entity _ Forum{..}) -> do
   -- TODO FIXME
   -- userPermissions_ByOrganizationIdM user_id forumOrgId
   pure allPermissions
