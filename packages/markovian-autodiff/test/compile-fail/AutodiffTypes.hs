{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module AutodiffTypes where

import Data.Coerce (coerce)
import Markovian.Autodiff

badIntermediateTypes = compose (identity SScalar) (sumVector (SVector @2))

badExactSmooth =
    compileExactPolynomial
        (compilerLimits 10 10 10 10 10 10 10 10 100 64)
        StorePullbacks
        tanhScalar

badOwnerSharing =
    shareParameters
        (parameter @"left" SScalar SScalar)
        (parameter @"right" SScalar SScalar)

badScalarVectorCoercion :: Value Rational 'Scalar -> Value Rational ('Vector 1)
badScalarVectorCoercion = coerce
