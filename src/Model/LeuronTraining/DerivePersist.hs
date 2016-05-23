{-# LANGUAGE TemplateHaskell #-}

module Model.LeuronTraining.DerivePersist (
  module A
) where



import           Database.Persist.TH
import           LN.T.LeuronTraining         as A



derivePersistField "LeuronTrainingSummary"
