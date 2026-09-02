module ExactKernelComposition where

import Markovian.Kernel.Exact

badComposition :: ExactKernel Int Int -> ExactKernel Int Int
badComposition kernel = composeExactKernel kernel kernel
