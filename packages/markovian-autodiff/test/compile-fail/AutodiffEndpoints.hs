{-# LANGUAGE DataKinds #-}

module AutodiffEndpoints where

import Markovian.Autodiff

type OneOwner = Owner "weight" 'Scalar

badWrongSeed :: ExactTape 'NoParameters 'Scalar 'Scalar -> Value Rational ('Vector 1) -> Either CompileError (ParameterValue Rational 'NoParameters, Value Rational 'Scalar)
badWrongSeed = applyExactTape

badWrongFragment :: DoubleTape 'Smooth 'NoParameters 'Scalar 'Scalar -> DoubleTape 'Polynomial 'NoParameters 'Scalar 'Scalar
badWrongFragment = id

badWrongTree :: ExactExecutable OneOwner 'Scalar 'Scalar -> Either CompileError (ExactRun OneOwner 'Scalar 'Scalar)
badWrongTree executable = runExact executable noParameters (scalarValue 1)

badCrossEndpoint :: ExactRun 'NoParameters 'Scalar ('Vector 1) -> ExactTape 'NoParameters 'Scalar 'Scalar
badCrossEndpoint = exactRunTape
