{-# LANGUAGE TemplateHaskell #-}

module LN.Model.DerivePersist (
  module A,
  Membership,
  Visibility,
  LikeOpt
) where



import           Database.Persist.TH
import           LN.T.Ent            as A
import           LN.T.LeuronTraining as A
import           LN.T.Like
import           LN.T.Membership
import           LN.T.Profile        as A
import           LN.T.Visibility




derivePersistField "Membership"
derivePersistField "Visibility"
derivePersistField "Ent"
derivePersistField "LeuronTrainingSummary"
derivePersistField "LikeOpt"
derivePersistField "ProfileGender"
