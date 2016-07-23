{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.PmOut (
  -- LN.Handler
  getPmOutPacksR,
  getPmOutPackR,

  -- LN.Model
  getPmOutPacksM,
  getPmOutPackM
) where



import           LN.All.PmOut
import           LN.All.Prelude
import           LN.All.User



--
-- LN.Handler
--

getPmOutPacksR :: Handler Value
getPmOutPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getPmOutPacksM (pure sp) user_id



getPmOutPackR :: PmOutId -> Handler Value
getPmOutPackR thread_post_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getPmOutPackM user_id thread_post_id






--
-- LN.Model
--

getPmOutPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff PmOutPackResponses
getPmOutPacksM m_sp user_id = do
  e_pm_outs <- getPmOutsM m_sp user_id
  rehtie e_pm_outs leftA $ \pm_outs -> do
    pm_out_packs <- rights <$> mapM (\pm_out -> getPmOutPack_ByPmOutM user_id pm_out) pm_outs
    rightA $ PmOutPackResponses {
      pmOutPackResponses = pm_out_packs
    }



getPmOutPackM :: UserId -> PmOutId -> HandlerErrorEff PmOutPackResponse
getPmOutPackM user_id pm_out_id = do
  e_pm_out <- getPmOutM user_id pm_out_id
  rehtie e_pm_out leftA $ getPmOutPack_ByPmOutM user_id



getPmOutPack_ByPmOutM :: UserId -> Entity PmOut -> HandlerErrorEff PmOutPackResponse
getPmOutPack_ByPmOutM user_id pmOut@(Entity pm_out_id PmOut{..}) = do

  e_pm_out_user <- getUserM user_id pmOutUserId

  rehtie e_pm_out_user leftA $ \pm_out_user -> do
    rightA $ PmOutPackResponse {
      pmOutPackResponsePmOut   = pmOutToResponse pmOut,
      pmOutPackResponsePmOutId = keyToInt64 pm_out_id,
      pmOutPackResponseUser    = userToSanitizedResponse pm_out_user,
      pmOutPackResponseUserId  = entityKeyToInt64 pm_out_user
    }
