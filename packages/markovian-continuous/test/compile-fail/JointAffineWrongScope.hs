module JointAffineWrongScope where

import Markovian.Continuous.Kernel.JointAffine.Exact
import Markovian.Continuous.Measure.Exact

data OwnerA
data OwnerB
data InputA
data RewardA
data SuccessorA

badJointWrongScopeMapping :: ExactJointAffineKernel OwnerA InputA RewardA SuccessorA -> Either ExactJointAffineError (ExactJointAffineKernel OwnerA InputA RewardA SuccessorA)
badJointWrongScopeMapping =
    alphaRenameJointAffineKernel
        (exactJointAffineLimits 1 1 2 6 100 10)
        [(noiseOwner 1 :: NoiseOwner OwnerB, noiseOwner 2 :: NoiseOwner OwnerB)]
