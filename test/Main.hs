module Main (main) where

import AcyclicOpenSystems (runAcyclicOpenSystemTests)
import AlgebraicFoundation (runAlgebraicFoundationTests)
import BayesianExact (runBayesianExactTests)
import CircuitCostRewrite (runCircuitCostRewriteTests)
import ClarkScarf1960 (runClarkScarf1960Tests)
import DogruInventoryBenchmark (runDogruInventoryTests)
import ExactControl (runExactControlTests)
import FiniteOpenGames (runFiniteOpenGameTests)
import FixedBatchRnQ (runFixedBatchRnQTests)
import GameCore (runGameCoreTests)
import InventoryBenchmark (runInventoryBenchmarkTests)
import Legacy (runLegacyTests)
import OpenSystems (runOpenSystemTests)
import PushPullExact (runPushPullExactTests)
import StochasticCircuit (runStochasticCircuitTests)
import TabularLearning (runTabularLearningTests)

main :: IO ()
main = do
    runAlgebraicFoundationTests run
    runBayesianExactTests run
    runPushPullExactTests run
    runExactControlTests run
    runInventoryBenchmarkTests run
    runClarkScarf1960Tests run
    runDogruInventoryTests run
    runFixedBatchRnQTests run
    runTabularLearningTests run
    runStochasticCircuitTests run
    runCircuitCostRewriteTests run
    runGameCoreTests run
    runFiniteOpenGameTests run
    runOpenSystemTests run
    runAcyclicOpenSystemTests run
    runLegacyTests run

run :: String -> IO () -> IO ()
run name test = do
    test
    putStrLn ("PASS: " ++ name)
