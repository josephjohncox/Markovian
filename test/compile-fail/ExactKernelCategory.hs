module ExactKernelCategory where

import qualified Control.Category as Category
import Markovian.Kernel.Exact (ExactKernel)

badCategory :: ExactKernel Int Int
badCategory = Category.id
