module ExactUncheckedBind where

import Markovian.Probability.Exact

badUncheckedBind :: ExactFiniteDist Int -> (Int -> ExactFiniteDist Int) -> ExactFiniteDist Int
badUncheckedBind = bindExactFiniteDist
