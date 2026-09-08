module SuccessorSubstitutionSpace where

import Markovian.Continuous.Kernel.JointAffine.Exact
import Markovian.Continuous.Measure.Exact

data RewardLabel
data SuccessorLabel

badSubstitutionSpace :: ExactSuccessorSubstitution () RewardLabel SuccessorLabel -> Either ExactJointAffineError (ExactJointLaw RewardLabel SuccessorLabel, ExactJointAffineReport)
badSubstitutionSpace result = materializeSuccessorSubstitution (exactJointAffineLimits 1 1 2 6 100 100) result 0
