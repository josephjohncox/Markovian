module SuccessorSubstitutionIntermediate where

import Markovian.Continuous.Kernel.JointAffine.Exact

data A
data B

badIntermediateLabel :: ExactJointAffineKernel A A A A -> ExactJointAffineKernel A B A A -> Either ExactSuccessorSubstitutionError (ExactSuccessorSubstitution A A A)
badIntermediateLabel left right = substituteLeftSuccessor (exactSuccessorSubstitutionLimits 1 1 2 12 14 1) left right (sharedSuccessorOwners [])
