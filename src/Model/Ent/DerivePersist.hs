{-# LANGUAGE TemplateHaskell #-}

module Model.Ent.DerivePersist (
  module A
) where



import           Database.Persist.TH
import           LN.T.Entity         as A



derivePersistField "Ent"
