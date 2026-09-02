module Main (main) where

import Data.Foldable (toList)
import Markovian.Probability.Exact (
    exactFiniteDist,
    exactOutcomes,
    exactProbability,
 )

main :: IO ()
main =
    case exactFiniteDist [('a', 1 / 4), ('b', 3 / 4)] of
        Left err -> fail (show err)
        Right distribution ->
            print
                ( sum
                    [ value outcome * exactProbability probability
                    | (outcome, probability) <- toList (exactOutcomes distribution)
                    ]
                )
  where
    value 'a' = 2
    value _ = 8
