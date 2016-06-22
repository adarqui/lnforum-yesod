{-# LANGUAGE RecordWildCards #-}

module All.Pack.Me (
  -- Handler
  getMePackR
) where



import           All.Pack.User
import           All.Prelude
import           All.User



getMePackR :: Handler Value
getMePackR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getUserPack_ByUserIdM user_id user_id defaultStandardParams
