module ExactKernelArrow where

import Control.Arrow (arr)
import Markovian.Kernel.Exact (ExactKernel)

badArrow :: ExactKernel Int Int
badArrow = arr id
