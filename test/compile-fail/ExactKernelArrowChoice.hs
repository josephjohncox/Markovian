module ExactKernelArrowChoice where

import Control.Arrow (left)
import Markovian.Kernel.Exact (ExactKernel)

badArrowChoice :: ExactKernel (Either Int Bool) (Either Int Bool)
badArrowChoice = left (error "unrestricted ArrowChoice")
