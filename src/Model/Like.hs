{-# LANGUAGE TemplateHaskell #-}

module Model.Like (
  module A
) where



import           Database.Persist.TH
import           LN.T.Like           as A



derivePersistField "LikeOpt"
