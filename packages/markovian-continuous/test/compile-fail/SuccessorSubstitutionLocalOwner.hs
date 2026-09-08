module SuccessorSubstitutionLocalOwner where

import Markovian.Continuous.Kernel.JointAffine.Exact
import Markovian.Continuous.Measure.Exact

badLocalOwner :: ExactSuccessorSubstitution () () () -> NoiseOwner ()
badLocalOwner result = withSuccessorSubstitutionKernel result (\_ -> noiseOwner 1)
