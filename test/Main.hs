module Main (main) where

import AcyclicOpenSystems (runAcyclicOpenSystemTests)
import AggregationExact (runAggregationLesson, runAggregationTests)
import AlgebraicFoundation (runAlgebraicFoundationTests)
import BayesianExact (runBayesianExactTests)
import CircuitCostRewrite (runCircuitCostRewriteTests)
import ExactBind (runExactBindTests)
import ExactControl (runExactControlTests)
import FeedbackExact (runFeedbackExactTests)
import FeedbackRewardJVP (runFeedbackRewardJVPTests, runRewardJVPLesson)
import FeedbackValueExact (runFeedbackValueExactTests)
import FiniteOpenGames (runFiniteOpenGameTests)
import GameCore (runGameCoreTests)
import LawLaboratory (runLawLaboratory)
import MixedBayesianGames (runMixedBayesianGameTests)
import OpenSystems (runOpenSystemTests)
import PushPullExact (runPushPullExactTests)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--learning"] -> runLawLaboratory
        ["--reward-jvp"] -> runRewardJVPLesson
        ["--aggregation"] -> runAggregationLesson
        _ -> allTests

allTests :: IO ()
allTests = do
    runLawLaboratory
    runAlgebraicFoundationTests run
    runBayesianExactTests run
    runPushPullExactTests run
    runExactBindTests run
    runExactControlTests run
    runFeedbackExactTests run
    runFeedbackValueExactTests run
    runFeedbackRewardJVPTests run
    runAggregationTests run
    runCircuitCostRewriteTests run
    runGameCoreTests run
    runFiniteOpenGameTests run
    runMixedBayesianGameTests run
    runOpenSystemTests run
    runAcyclicOpenSystemTests run

run :: String -> IO () -> IO ()
run name test = do
    test
    putStrLn ("PASS: " ++ name)
