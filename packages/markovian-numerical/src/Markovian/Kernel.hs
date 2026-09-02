-- | One-layer finite stochastic kernels.
module Markovian.Kernel (
    Kernel,
    kernel,
    runKernel,
    deterministic,
)
where

import Markovian.Probability (FiniteDist, dirac)

{- | A stochastic kernel from an input to a validated finite distribution.
Running a kernel returns one layer. It does not unfold successor states.
-}
newtype Kernel input output = Kernel (input -> FiniteDist output)

-- | Construct a kernel from a total one-layer function.
kernel :: (input -> FiniteDist output) -> Kernel input output
kernel = Kernel

-- | Run a kernel for one input.
runKernel :: Kernel input output -> input -> FiniteDist output
runKernel (Kernel step) = step

-- | Lift a deterministic function into a finite kernel.
deterministic :: (input -> output) -> Kernel input output
deterministic f = Kernel (dirac . f)
