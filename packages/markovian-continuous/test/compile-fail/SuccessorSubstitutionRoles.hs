module SuccessorSubstitutionRoles where

import Data.Coerce (coerce)
import Markovian.Continuous.Kernel.JointAffine.Exact

newtype A = A Int
newtype B = B Int

badSubstitutionSourceRole :: ExactSuccessorSubstitution A A A -> ExactSuccessorSubstitution B A A
badSubstitutionSourceRole = coerce

badSubstitutionRewardRole :: ExactSuccessorSubstitution A A A -> ExactSuccessorSubstitution A B A
badSubstitutionRewardRole = coerce

badSubstitutionSuccessorRole :: ExactSuccessorSubstitution A A A -> ExactSuccessorSubstitution A A B
badSubstitutionSuccessorRole = coerce

badRequestLeftRole :: ExactSuccessorOwnerRequest A A -> ExactSuccessorOwnerRequest B A
badRequestLeftRole = coerce

badRequestRightRole :: ExactSuccessorOwnerRequest A A -> ExactSuccessorOwnerRequest A B
badRequestRightRole = coerce
