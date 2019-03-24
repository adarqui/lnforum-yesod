{-# LANGUAGE TemplateHaskell #-}

module LN.Model.DerivePersist (
  module A,
  Visibility,
  LikeOpt
) where



import           Database.Persist.TH
import           LN.T.Ent            as A
import           LN.T.Like
import           LN.T.Profile        as A
import           LN.T.Visibility




derivePersistField "Visibility"
derivePersistField "Ent"
derivePersistField "LikeOpt"
derivePersistField "ProfileGender"
