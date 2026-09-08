module SuccessorSubstitutionSealing where

import Markovian.Continuous.Kernel.JointAffine.Exact

badSealedReScope :: ExactSuccessorSubstitution () () () -> Either ExactJointAffineError (ExactJointAffineKernel () () () ())
badSealedReScope result = reScopeJointAffineKernel (exactJointAffineLimits 1 1 2 6 100 100) [] result

badSealedChaining :: ExactSuccessorSubstitution () () () -> ExactJointAffineKernel () () () () -> Either ExactSuccessorSubstitutionError (ExactSuccessorSubstitution () () ())
badSealedChaining result right = substituteLeftSuccessor (exactSuccessorSubstitutionLimits 3 3 2 20 77 2) result right (sharedSuccessorOwners [])
