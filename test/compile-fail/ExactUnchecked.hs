module ExactUnchecked where

import Markovian.Kernel.Exact
import Markovian.Probability.Exact

badMonad :: ExactFiniteDist Int -> ExactFiniteDist Int
badMonad distribution = do
    value <- distribution
    exactDirac (value + 1)

badComposition :: ExactKernel Int Int -> ExactKernel Int Int
badComposition kernel = composeExactKernel kernel kernel
