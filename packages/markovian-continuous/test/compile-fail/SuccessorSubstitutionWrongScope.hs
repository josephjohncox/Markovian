module SuccessorSubstitutionWrongScope where

import Markovian.Continuous.Kernel.JointAffine.Exact
import Markovian.Continuous.Measure.Exact

data OwnerA
data OwnerB

badSuccessorSharedScope :: NoiseOwner OwnerA -> ExactSuccessorOwnerRequest OwnerA OwnerB
badSuccessorSharedScope source = sharedSuccessorOwners [(source, noiseOwner 1)]

badSuccessorFreshScope :: NoiseOwner OwnerA -> ExactSuccessorOwnerRequest OwnerA OwnerB
badSuccessorFreshScope source = freshSuccessorOwners [(source, 3)]
