module JointAffineRoles where

import Data.Coerce (coerce)
import Markovian.Continuous.Kernel.JointAffine.Exact
import Markovian.Continuous.Measure.Exact

data OwnerA
data OwnerB
data InputA
data InputB
data RewardA
data RewardB
data SuccessorA
data SuccessorB

badJointOwnerCoerce :: ExactJointAffineKernel OwnerA InputA RewardA SuccessorA -> ExactJointAffineKernel OwnerB InputA RewardA SuccessorA
-- pi-lens-ignore: typecheck:GHC-18872
badJointOwnerCoerce = coerce

badJointInputCoordinateRole :: ExactAffineInputCoordinate InputA RewardA -> ExactAffineInputCoordinate InputB RewardA
-- pi-lens-ignore: typecheck:GHC-18872
badJointInputCoordinateRole = coerce

badJointRewardCoordinateRole :: ExactJointAffineKernel OwnerA InputA RewardA SuccessorA -> ExactJointAffineKernel OwnerA InputA RewardB SuccessorA
-- pi-lens-ignore: typecheck:GHC-18872
badJointRewardCoordinateRole = coerce

badJointSuccessorCoordinateRole :: ExactJointAffineKernel OwnerA InputA RewardA SuccessorA -> ExactJointAffineKernel OwnerA InputA RewardA SuccessorB
-- pi-lens-ignore: typecheck:GHC-18872
badJointSuccessorCoordinateRole = coerce

badJointOwnerLocality :: ExactJointAffineKernel OwnerA InputA RewardA SuccessorA -> Either ExactJointAffineError (ExactJointAffineKernel OwnerB InputA RewardA SuccessorA)
-- pi-lens-ignore: typecheck:GHC-83865
badJointOwnerLocality = alphaRenameJointAffineKernel (exactJointAffineLimits 1 1 2 6 10 10) []

badArbitrarySpaceMaterialization :: ExactJointAffineKernel OwnerA InputA RewardA SuccessorA -> Either ExactJointAffineError (ExactJointLaw RewardA SuccessorA, ExactJointAffineReport)
-- pi-lens-ignore: typecheck:GHC-83865
badArbitrarySpaceMaterialization kernel = materializeJointAffineKernel (exactJointAffineLimits 1 1 2 6 100 10) kernel 0
