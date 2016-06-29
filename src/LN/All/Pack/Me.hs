{-# LANGUAGE RecordWildCards #-}

module LN.All.Pack.Me (
  -- LN.Handler
  getMePackR
) where



import           LN.All.Pack.User
import           LN.All.Prelude



getMePackR :: Handler Value
getMePackR = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getUserPack_ByUserIdM user_id user_id
