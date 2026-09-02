module ExactDistributionOpacity where

import qualified Data.List.NonEmpty as NonEmpty
import Markovian.Probability.Exact

badDistribution :: ExactProb -> ExactFiniteDist Int
badDistribution mass = UnsafeExactFiniteDist ((1, mass) NonEmpty.:| [])
