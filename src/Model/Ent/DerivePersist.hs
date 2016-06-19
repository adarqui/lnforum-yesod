{-# LANGUAGE TemplateHaskell #-}

module Model.Ent.DerivePersist (
  module A
) where



import           Database.Persist.TH
import           LN.T.Ent            as A



derivePersistField "Ent"
