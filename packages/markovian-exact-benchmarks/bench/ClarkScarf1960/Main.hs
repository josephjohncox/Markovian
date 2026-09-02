module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Ratio ((%))
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTimeNSec)
import Markovian.Benchmark.Inventory.ClarkScarf1960.Finite.Exact
import Markovian.Benchmark.Inventory.ClarkScarf1960.Oracle.Exact
import Markovian.Benchmark.Inventory.ClarkScarf1960.Report
import Markovian.Objective.Exact (mkExactDiscount)
import Numeric.Natural (Natural)
import System.IO (hSetEncoding, stdout, utf8)
import System.Info (arch, compilerName, compilerVersion, os)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    warmups <- replicateM (fromIntegral clarkScarfBenchmarkWarmupCount) semanticRun
    mapM_ forceSemantic warmups
    measured <- replicateM (fromIntegral clarkScarfBenchmarkSampleCount) measure
    case measured of
        [] -> fail "Clark-Scarf benchmark requires measured samples"
        first : remaining -> do
            mapM_ (requireSame (measuredSemantic first) . measuredSemantic) remaining
            putStr (semanticReport (measuredSemantic first))
            putStrLn "benchmark metadata (local reproducibility only)"
            putStrLn ("compiler: " ++ compilerName ++ "-" ++ showVersion compilerVersion)
            putStrLn ("os: " ++ os ++ ", architecture: " ++ arch)
            putStrLn ("warm-up samples excluded: " ++ show clarkScarfBenchmarkWarmupCount)
            putStrLn ("measured complete samples: " ++ show clarkScarfBenchmarkSampleCount)
            mapM_ renderSample (zip [1 :: Natural ..] measured)

data Semantic = Semantic
    { semanticReport :: !String
    , semanticPrimaryCost :: !Rational
    , semanticWidenedCost :: !Rational
    }
    deriving (Eq, Show)

data Measured = Measured
    { measuredNanoseconds :: !Natural
    , measuredSemantic :: !Semantic
    }

semanticRun :: IO Semantic
semanticRun = do
    discount <- requireRight "discount" (mkExactDiscount 1)
    demand <- requireRight "demand" (conditionedClarkScarfDemand (3 % 4) [(0, 1 % 2), (1, 1 % 2)])
    primary <- solve discount demand 2 4
    widened <- solve discount demand 3 5
    report <- requireRight "report" (clarkScarfReport primary widened)
    pure
        Semantic
            { semanticReport = renderClarkScarfReport report
            , semanticPrimaryCost = clarkScarfInitialCost primary
            , semanticWidenedCost = clarkScarfInitialCost widened
            }
  where
    solve discount demand orderCap targetCap = do
        parameters <-
            requireRight
                "parameters"
                ( validateClarkScarfParameters
                    ClarkScarfParameterInput
                        { inputClarkScarfHorizon = 3
                        , inputClarkScarfDiscount = discount
                        , inputClarkScarfOrderCap = orderCap
                        , inputClarkScarfIsolatedTargetCap = targetCap
                        , inputClarkScarfExternalSetupCost = 0
                        , inputClarkScarfExternalUnitCost = 0
                        , inputClarkScarfTransportUnitCost = 0
                        , inputClarkScarfDownstreamHoldingCost = 1
                        , inputClarkScarfDownstreamShortageCost = 4
                        , inputClarkScarfUpstreamHoldingCost = 1
                        , inputClarkScarfUpstreamShortageCost = 0
                        , inputClarkScarfStateBudget = 100000
                        , inputClarkScarfStateActionBudget = 3000000
                        , inputClarkScarfSolverWorkBudget = 100000000000
                        }
                )
        fixture <- requireRight "fixture" (clarkScarfFixture parameters demand (ClarkScarfState 3 0 0 1))
        requireRight "solve" (solveClarkScarf fixture)

forceSemantic :: Semantic -> IO ()
forceSemantic result = do
    _ <- evaluate (length (semanticReport result))
    _ <- evaluate (semanticPrimaryCost result)
    _ <- evaluate (semanticWidenedCost result)
    pure ()

measure :: IO Measured
measure = do
    start <- getMonotonicTimeNSec
    result <- semanticRun
    forceSemantic result
    finish <- getMonotonicTimeNSec
    pure Measured{measuredNanoseconds = fromIntegral (finish - start), measuredSemantic = result}

requireSame :: Semantic -> Semantic -> IO ()
requireSame expected actual =
    if expected == actual then pure () else fail "timing sample changed exact semantic report"

renderSample :: (Natural, Measured) -> IO ()
renderSample (index, result) =
    putStrLn ("sample " ++ show index ++ " elapsed-ns: " ++ show (measuredNanoseconds result))

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left err) = fail (label ++ ": " ++ show err)
