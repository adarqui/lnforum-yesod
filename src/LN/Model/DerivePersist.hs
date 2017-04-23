{-# LANGUAGE TemplateHaskell #-}

module LN.Model.DerivePersist (
  module A,
  Membership,
  Visibility,
  LikeOpt,
  TrainingStyle
) where



import           Database.Persist.TH
import           LN.T.Ent            as A
import           LN.T.LeuronTraining as A
import           LN.T.Like
import           LN.T.Membership
import           LN.T.Profile        as A
import           LN.T.Visibility
import           LN.T.Training




derivePersistField "Membership"
derivePersistField "Visibility"
derivePersistField "Ent"
derivePersistField "LeuronTrainingSummary"
derivePersistField "LikeOpt"
derivePersistField "ProfileGender"
derivePersistField "TrainingStyle"
