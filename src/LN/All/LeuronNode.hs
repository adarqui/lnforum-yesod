{-# LANGUAGE RecordWildCards #-}

module LN.All.LeuronNode (
  defaultLeuronNode
) where



import           LN.All.Prelude




--
-- Model/Function
--

defaultLeuronNode :: UserId -> ResourceId -> LeuronId -> LeuronNode
defaultLeuronNode user_id resource_id leuron_id = LeuronNode {
  leuronNodeUserId          = user_id,
  leuronNodeResourceId      = resource_id,
  leuronNodeLeuronId        = leuron_id,

  leuronNodeNumTotal          = 0,
  leuronNodeNumKnow           = 0,
  leuronNodeNumDontKnow       = 0,
  leuronNodeNumDontCare       = 0,
  leuronNodeNumProtest        = 0,
  leuronNodeHonorKnow         = 0,
  leuronNodeHonorKnowAt       = Nothing,
  leuronNodeHonorDontKnow     = 0,
  leuronNodeHonorDontKnowAt   = Nothing,
  leuronNodeHonorDontCare     = 0,
  leuronNodeHonorDontCareAt   = Nothing,
  leuronNodeHonorProtest      = 0,
  leuronNodeHonorProtestAt    = Nothing,
  leuronNodeBooleanKnow       = 0,
  leuronNodeBooleanKnowAt     = Nothing,
  leuronNodeBooleanDontKnow   = 0,
  leuronNodeBooleanDontKnowAt = Nothing,
  leuronNodeBooleanDontCare   = 0,
  leuronNodeBooleanDontCareAt = Nothing,
  leuronNodeBooleanProtest    = 0,
  leuronNodeBooleanProtestAt  = Nothing,
  leuronNodeMatchKnow         = 0,
  leuronNodeMatchKnowAt       = Nothing,
  leuronNodeMatchDontKnow     = 0,
  leuronNodeMatchDontKnowAt   = Nothing,
  leuronNodeMatchDontCare     = 0,
  leuronNodeMatchDontCareAt   = Nothing,
  leuronNodeMatchProtest      = 0,
  leuronNodeMatchProtestAt    = Nothing,
  leuronNodeSubsKnow          = 0,
  leuronNodeSubsKnowAt        = Nothing,
  leuronNodeSubsDontKnow      = 0,
  leuronNodeSubsDontKnowAt    = Nothing,
  leuronNodeSubsDontCare      = 0,
  leuronNodeSubsDontCareAt    = Nothing,
  leuronNodeSubsProtest       = 0,
  leuronNodeSubsProtestAt     = Nothing,
  leuronNodeSplitsKnow        = 0,
  leuronNodeSplitsKnowAt      = Nothing,
  leuronNodeSplitsDontKnow    = 0,
  leuronNodeSplitsDontKnowAt  = Nothing,
  leuronNodeSplitsDontCare    = 0,
  leuronNodeSplitsDontCareAt  = Nothing,
  leuronNodeSplitsProtest     = 0,
  leuronNodeSplitsProtestAt   = Nothing,

  leuronNodeActive          = True,
  leuronNodeGuard           = 0,
  leuronNodeCreatedAt       = Nothing,
  leuronNodeModifiedAt      = Nothing
}
