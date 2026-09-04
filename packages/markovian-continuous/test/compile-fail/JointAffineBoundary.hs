module JointAffineBoundary where

import Markovian.Continuous.Kernel.JointAffine.Exact

data OwnerA
data InputA
data RewardA
data SuccessorA

badJointAffineConstructor :: ExactJointAffineKernel OwnerA InputA RewardA SuccessorA
badJointAffineConstructor = UnsafeExactJointAffineKernel undefined undefined [] 0 0 0 undefined

badAffineInputConstructor :: ExactAffineInputCoordinate InputA RewardA
badAffineInputConstructor = UnsafeExactAffineInputCoordinate 1 0
