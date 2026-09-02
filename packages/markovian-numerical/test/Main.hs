module Main (main) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Probability (finiteDist, outcomes, probability)

main :: IO ()
main =
    case finiteDist [('a', 1), ('b', 3)] of
        Left problem -> ioError (userError (show problem))
        Right distribution ->
            if fmap (probability . snd) (NonEmpty.toList (outcomes distribution)) == [0.25, 0.75]
                then putStrLn "PASS: floating probability remains in markovian-numerical"
                else ioError (userError "floating normalization changed")
