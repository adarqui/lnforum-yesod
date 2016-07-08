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



import           Control.Monad          (void)
import           Database.Persist.Types (Entity (..))
import           LN.Generate.Default    (defaultApiRequest,
                                         defaultProfileRequest)
import           LN.Job.Enqueue         (mkJob_CreateUserApi,
                                         mkJob_CreateUserProfile)
import           LN.Model
import           Prelude                (IO, pure, ($))



insertUsers_TasksM :: Entity User -> IO ()
insertUsers_TasksM (Entity new_user_id _) = do

  void $ mkJob_CreateUserProfile new_user_id defaultProfileRequest
  void $ mkJob_CreateUserApi new_user_id defaultApiRequest

  pure ()
