module Main (main) where

import Markovian.Probability (dirac)
import Markovian.Sampling (generatorFromSeed, sampleFiniteDist)

main :: IO ()
main =
    case sampleFiniteDist (generatorFromSeed 7) (dirac 'x') of
        Right ('x', _) -> putStrLn "PASS: explicit sampled interpreter package"
        result -> ioError (userError ("sampling failed: " ++ show result))
