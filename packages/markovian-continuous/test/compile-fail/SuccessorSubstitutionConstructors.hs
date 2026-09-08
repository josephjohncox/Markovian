module SuccessorSubstitutionConstructors where

import Markovian.Continuous.Kernel.JointAffine.Exact

data Label

badSubstitutionConstructor :: ExactSuccessorSubstitution Label Label Label
badSubstitutionConstructor = SealedSuccessorSubstitution undefined [] undefined

-- No signature mentions the private witness type: if its constructor were
-- accidentally exported, this binding would compile rather than fail by type.
badSharedWitnessConstructor = VerifiedSharedSuccessorOwners undefined

badFreshWitnessConstructor = VerifiedFreshSuccessorOwners undefined

badOwnerRequestConstructor :: ExactSuccessorOwnerRequest Label Label
badOwnerRequestConstructor = SharedSuccessorOwners []

badSubstitutionLimitsConstructor :: ExactSuccessorSubstitutionLimits
badSubstitutionLimitsConstructor = SuccessorSubstitutionLimits undefined

badSubstitutionReportConstructor :: ExactSuccessorSubstitutionReport
badSubstitutionReportConstructor = SuccessorSubstitutionReport SharedRightOwners undefined 0 0 0 1
