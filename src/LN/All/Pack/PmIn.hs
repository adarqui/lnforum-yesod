{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.PmIn (
  -- LN.Handler
  getPmInPacksR,
  getPmInPackR,

  -- LN.Model
  getPmInPacksM,
  getPmInPackM
) where



import           LN.All.PmIn
import           LN.All.Prelude
import           LN.All.User



--
-- LN.Handler
--

getPmInPacksR :: Handler Value
getPmInPacksR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getPmInPacksM (pure sp) user_id



getPmInPackR :: PmInId -> Handler Value
getPmInPackR thread_post_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getPmInPackM user_id thread_post_id






--
-- LN.Model
--

getPmInPacksM :: Maybe StandardParams -> UserId -> HandlerErrorEff PmInPackResponses
getPmInPacksM m_sp user_id = do

  e_pm_ins <- getPmInsM m_sp user_id

  rehtie e_pm_ins leftA $ \pm_ins -> do
    pm_in_packs <- rights <$> mapM (\pm_in -> getPmInPack_ByPmInM user_id pm_in) pm_ins
    rightA $ PmInPackResponses {
      pmInPackResponses = pm_in_packs
    }



getPmInPackM :: UserId -> PmInId -> HandlerErrorEff PmInPackResponse
getPmInPackM user_id pm_in_id = do

  e_pm_in <- getPmInM user_id pm_in_id
  rehtie e_pm_in leftA $ getPmInPack_ByPmInM user_id



getPmInPack_ByPmInM :: UserId -> Entity PmIn -> HandlerErrorEff PmInPackResponse
getPmInPack_ByPmInM user_id pmIn@(Entity pm_in_id PmIn{..}) = do

  e_pm_in_user <- getUserM user_id pmInUserId

  rehtie e_pm_in_user leftA $ \pm_in_user -> do
    rightA $ PmInPackResponse {
      pmInPackResponsePmIn    = pmInToResponse pmIn,
      pmInPackResponsePmInId  = keyToInt64 pm_in_id,
      pmInPackResponseUser    = userToSanitizedResponse pm_in_user,
      pmInPackResponseUserId  = entityKeyToInt64 pm_in_user
    }
