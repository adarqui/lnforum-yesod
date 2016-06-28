{-# LANGUAGE RecordWildCards #-}

module All.Pack.Me (
  -- Handler
  getMePackR
) where



import           All.Pack.User
import           All.Prelude



getMePackR :: Handler Value
getMePackR = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getUserPack_ByUserIdM user_id user_id
