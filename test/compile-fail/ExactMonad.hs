module ExactMonad where

import Markovian.Probability.Exact

badMonad :: ExactFiniteDist Int -> ExactFiniteDist Int
badMonad distribution = do
    value <- distribution
    exactDirac (value + 1)
