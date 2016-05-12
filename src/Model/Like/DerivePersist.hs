{-# LANGUAGE TemplateHaskell #-}

module Model.Like.DerivePersist (
  module A
) where



import           Database.Persist.TH
import           LN.T.Like           as A



derivePersistField "LikeOpt"
