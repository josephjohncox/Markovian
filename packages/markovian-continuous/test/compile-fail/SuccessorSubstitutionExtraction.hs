module SuccessorSubstitutionExtraction where

import Markovian.Continuous.Kernel.JointAffine.Exact

badUnderlyingKernel :: ExactSuccessorSubstitution () () () -> ExactJointAffineKernel () () () ()
badUnderlyingKernel result = case result of SealedSuccessorSubstitution kernel _ _ -> kernel
