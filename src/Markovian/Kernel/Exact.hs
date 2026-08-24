-- | Exact one-layer finite stochastic kernels and Kleisli composition.
module Markovian.Kernel.Exact (
    ExactKernel,
    exactKernel,
    runExactKernel,
    exactDeterministic,
    composeExactKernel,
) where

import Markovian.Probability.Exact (
    ExactFiniteDist,
    bindExactFiniteDist,
    exactDirac,
 )

{- | An exact stochastic kernel from an input to a rational finite
distribution. Running a kernel returns one layer.
-}
newtype ExactKernel input output = ExactKernel (input -> ExactFiniteDist output)

-- | Construct an exact kernel from a total one-layer function.
exactKernel :: (input -> ExactFiniteDist output) -> ExactKernel input output
exactKernel = ExactKernel

-- | Run an exact kernel for one input.
runExactKernel :: ExactKernel input output -> input -> ExactFiniteDist output
runExactKernel (ExactKernel step) = step

-- | Lift a deterministic function into an exact finite kernel.
exactDeterministic :: (input -> output) -> ExactKernel input output
exactDeterministic f = ExactKernel (exactDirac . f)

{- | Compose exact kernels from left to right.

@composeExactKernel first second@ runs @first@ and binds each outcome through
@second@. Rational arithmetic makes the Kleisli laws literal equalities.
-}
composeExactKernel :: ExactKernel a b -> ExactKernel b c -> ExactKernel a c
composeExactKernel first second =
    ExactKernel
        (\input -> bindExactFiniteDist (runExactKernel first input) (runExactKernel second))
