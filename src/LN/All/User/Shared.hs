{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

--
-- INFO: Because of OAuth2.hs and cyclic dependencies,
-- these routines cannot be centralized in All.User.hs
--

module LN.All.User.Shared (
  insertUsers_TasksM
) where



import           LN.Job.Enqueue       (mkJob_CreateUserProfile)
import           LN.T.Api.Request     (ApiRequest (..))
import           LN.T.Profile.Request (ProfileRequest (..))



insertUsers_TasksM :: UserId -> Entity User -> HandlerEff ()
insertUsers_TasksM _ (Entity new_user_id _) = do

  liftIO $ mkJob_CreateUserProfile new_user_id defaultProfileRequest
  liftIO $ mkJob_CreateUserApi new_user_id defaultApieRequest

  right ()
