{-# LANGUAGE RecordWildCards #-}

module LN.Access (
  isSuperM,
  mustBe_SuperM,
  mustBe_SameUserM
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
