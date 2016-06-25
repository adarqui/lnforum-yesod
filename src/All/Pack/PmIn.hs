{-# LANGUAGE RecordWildCards #-}

module All.Pack.PmIn (
  -- Handler
  getPmInPacksR,
  getPmInPackR,

  -- Model
  getPmInPacksM,
  getPmInPackM
) where



import           All.PmIn
import           All.Prelude
import           All.User



--
-- Handler
--

getPmInPacksR :: Handler Value
getPmInPacksR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getPmInPacksM user_id



getPmInPackR :: PmInId -> Handler Value
getPmInPackR thread_post_id = run $ do
  user_id <- _requireAuthId
  toJSON <$> getPmInPackM user_id thread_post_id






--
-- Model
--

getPmInPacksM :: UserId -> HandlerEff PmInPackResponses
getPmInPacksM user_id = do

  sp <- lookupStandardParams

  pmIns <- getPmInsM user_id

  pm_in_packs <- mapM (\pmIn -> getPmInPack_ByPmInM user_id pmIn sp) pmIns

  return $ PmInPackResponses {
    pmInPackResponses = pm_in_packs
  }



getPmInPackM :: UserId -> PmInId -> HandlerEff PmInPackResponse
getPmInPackM user_id pm_in_id = do

  pmIn <- getPmInM user_id pm_in_id

  getPmInPack_ByPmInM user_id pmIn defaultStandardParams



getPmInPack_ByPmInM :: UserId -> Entity PmIn -> StandardParams -> HandlerEff PmInPackResponse
getPmInPack_ByPmInM user_id pmIn@(Entity pm_in_id PmIn{..}) _ = do

  pm_in_user <- getUserM user_id pmInUserId

  return $ PmInPackResponse {
    pmInPackResponsePmIn    = pmInToResponse pmIn,
    pmInPackResponsePmInId  = keyToInt64 pm_in_id,
    pmInPackResponseUser    = userToSanitizedResponse pm_in_user,
    pmInPackResponseUserId  = entityKeyToInt64 pm_in_user
  }
