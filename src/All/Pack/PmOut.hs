{-# LANGUAGE RecordWildCards #-}

module All.Pack.PmOut (
  -- Handler
  getPmOutPacksR,
  getPmOutPackR,

  -- Model
  getPmOutPacksM,
  getPmOutPackM
) where



import           All.PmOut
import           All.Prelude
import           All.User



--
-- Handler
--

getPmOutPacksR :: Handler Value
getPmOutPacksR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getPmOutPacksM user_id



getPmOutPackR :: PmOutId -> Handler Value
getPmOutPackR thread_post_id = run $ do
  user_id <- _requireAuthId
  toJSON <$> getPmOutPackM user_id thread_post_id






--
-- Model
--

getPmOutPacksM :: UserId -> HandlerEff PmOutPackResponses
getPmOutPacksM user_id = do

  sp <- lookupStandardParams

  pmOuts <- getPmOutsM user_id

  pm_out_packs <- mapM (\pmOut -> getPmOutPack_ByPmOutM user_id pmOut sp) pmOuts

  return $ PmOutPackResponses {
    pmOutPackResponses = pm_out_packs
  }



getPmOutPackM :: UserId -> PmOutId -> HandlerEff PmOutPackResponse
getPmOutPackM user_id pm_out_id = do

  pmOut <- getPmOutM user_id pm_out_id

  getPmOutPack_ByPmOutM user_id pmOut defaultStandardParams



getPmOutPack_ByPmOutM :: UserId -> Entity PmOut -> StandardParams -> HandlerEff PmOutPackResponse
getPmOutPack_ByPmOutM user_id pmOut@(Entity pm_out_id PmOut{..}) _ = do

  pm_out_user <- getUserM user_id pmOutUserId

  return $ PmOutPackResponse {
    pmOutPackResponsePmOut    = pmOutToResponse pmOut,
    pmOutPackResponsePmOutId  = keyToInt64 pm_out_id,
    pmOutPackResponseUser    = userToSanitizedResponse pm_out_user,
    pmOutPackResponseUserId  = entityKeyToInt64 pm_out_user
  }
