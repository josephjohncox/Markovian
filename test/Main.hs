module Main (main) where

import AcyclicOpenSystems (runAcyclicOpenSystemTests)
import AlgebraicFoundation (runAlgebraicFoundationTests)
import BayesianExact (runBayesianExactTests)
import ExactControl (runExactControlTests)
import Legacy (runLegacyTests)
import OpenSystems (runOpenSystemTests)
import StochasticCircuit (runStochasticCircuitTests)
import TabularLearning (runTabularLearningTests)

main :: IO ()
main = do
    runAlgebraicFoundationTests run
    runBayesianExactTests run
    runExactControlTests run
    runTabularLearningTests run
    runStochasticCircuitTests run
    runOpenSystemTests run
    runAcyclicOpenSystemTests run
    runLegacyTests run

run :: String -> IO () -> IO ()
run name test = do
    test
    putStrLn ("PASS: " ++ name)
