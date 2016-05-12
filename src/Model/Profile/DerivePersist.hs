{-# LANGUAGE TemplateHaskell #-}

module Model.Profile.DerivePersist (
  module A
) where



import           Database.Persist.TH
import           LN.T.Profile        as A



derivePersistField "ProfileGender"
