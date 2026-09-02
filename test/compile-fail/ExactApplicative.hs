module ExactApplicative where

import Markovian.Probability.Exact

badApplicative :: ExactFiniteDist (Int -> Int) -> ExactFiniteDist Int -> ExactFiniteDist Int
badApplicative functions values = functions <*> values
