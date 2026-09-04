module Main (main) where

import AcyclicOpenSystems (runAcyclicOpenSystemTests)
import AlgebraicFoundation (runAlgebraicFoundationTests)
import BayesianExact (runBayesianExactTests)
import CircuitCostRewrite (runCircuitCostRewriteTests)
import ExactBind (runExactBindTests)
import ExactControl (runExactControlTests)
import FeedbackExact (runFeedbackExactTests)
import FeedbackValueExact (runFeedbackValueExactTests)
import FiniteOpenGames (runFiniteOpenGameTests)
import GameCore (runGameCoreTests)
import MixedBayesianGames (runMixedBayesianGameTests)
import OpenSystems (runOpenSystemTests)
import PushPullExact (runPushPullExactTests)

main :: IO ()
main = do
    runAlgebraicFoundationTests run
    runBayesianExactTests run
    runPushPullExactTests run
    runExactBindTests run
    runExactControlTests run
    runFeedbackExactTests run
    runFeedbackValueExactTests run
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
