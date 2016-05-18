{-# LANGUAGE TemplateHaskell #-}

module Model.Like.DerivePersist (
  LikeOpt
) where



import           Database.Persist.TH
import           LN.T.Like



derivePersistField "LikeOpt"
