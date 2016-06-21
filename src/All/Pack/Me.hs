{-# LANGUAGE RecordWildCards #-}

module All.Pack.Me (
  -- Handler
  getMePackR
) where



import           All.Prelude
import           All.Pack.User
import           Model.User.Function
import           Model.User.Internal2



getMePackR :: Handler Value
getMePackR = do
  user_id <- requireAuthId
  toJSON <$> getUserPack_ByUserIdM user_id user_id defaultStandardParams
