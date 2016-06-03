{-# LANGUAGE TemplateHaskell #-}

module Model.DerivePersist (
  Membership,
  Visibility
) where



import           Database.Persist.TH
import           LN.T.Membership
import           LN.T.Visibility



derivePersistField "Membership"
derivePersistField "Visibility"
