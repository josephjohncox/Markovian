module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTimeNSec)
import Markovian.Benchmark.Inventory.Serial.FixedBatch.Exact
import Markovian.Benchmark.Inventory.Serial.FixedBatch.Newsvendor.Exact
import Markovian.Benchmark.Inventory.Serial.FixedBatch.Report
import Numeric.Natural (Natural)
import System.IO (hSetEncoding, stdout, utf8)
import System.Info (arch, compilerName, compilerVersion, os)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    warmups <- replicateM (fromIntegral fixedBatchBenchmarkWarmupCount) semanticRun
    mapM_ forceSemantic warmups
    measured <- replicateM (fromIntegral fixedBatchBenchmarkSampleCount) measure
    case measured of
        [] -> fail "fixed-batch benchmark requires measured samples"
        first : remaining -> do
            mapM_ (requireSame (measuredSemantic first) . measuredSemantic) remaining
            putStr (semanticReport (measuredSemantic first))
            putStrLn "benchmark metadata (local reproducibility only; no speedup or convergence claim)"
            putStrLn ("compiler: " ++ compilerName ++ "-" ++ showVersion compilerVersion)
            putStrLn ("os: " ++ os ++ ", architecture: " ++ arch)
            putStrLn ("warm-up samples excluded: " ++ show fixedBatchBenchmarkWarmupCount)
            putStrLn ("measured complete samples: " ++ show fixedBatchBenchmarkSampleCount)
            mapM_ renderSample (zip [1 :: Natural ..] measured)

data Semantic = Semantic
    { semanticReport :: !String
    , semanticOracle :: !Rational
    , semanticPolicy :: !Rational
    , semanticStationary :: !Rational
    }
    deriving (Eq, Show)

data Measured = Measured
    { measuredNanoseconds :: !Natural
    , measuredSemantic :: !Semantic
    }

semanticRun :: IO Semantic
semanticRun = do
    (_, primary, primaryStationary) <- solve 2 3 (-2) 4
    (_, widened, _) <- solve 3 3 (-3) 5
    (_, demandWidened, demandStationary) <- solve 2 4 (-2) 4
    (_, separationFinite, separationStationary) <- solveAt 2 2 2 (-4) 7
    report <- requireRight "report" (fixedBatchReport primary widened demandWidened primaryStationary demandStationary separationFinite separationStationary)
    pure
        Semantic
            { semanticReport = renderFixedBatchReport report
            , semanticOracle = fixedBatchOracleReturn primary
            , semanticPolicy = fixedBatchPolicyReturn primary
            , semanticStationary = newsvendorSubsystem2Cost primaryStationary
            }
  where
    solve = solveAt 3

solveAt :: Integer -> Integer -> Integer -> Integer -> Integer -> IO (FixedBatchParameters, FixedBatchSolution, NewsvendorSolution)
solveAt horizon externalCap demandCap lower upper = do
    parameters <- requireRight "parameters" (validateFixedBatchParameters (parameterInput horizon externalCap demandCap))
    initial <- requireRight "state" (fixedBatchState parameters (fromInteger horizon) 0 2 [0, 0])
    fixture <- requireRight "fixture" (fixedBatchFixture parameters initial)
    grid <- requireRight "grid" (reorderGrid parameters (gridValues lower upper))
    domain <- requireRight "stationary domain" (newsvendorDomain parameters (lower :| [lower + 1 .. upper]) (lower :| [lower + 1 .. upper]))
    solution <- requireRight "finite solve" (solveFixedBatch grid fixture)
    stationary <- requireRight "stationary solve" (solveNewsvendorGrid parameters domain)
    pure (parameters, solution, stationary)

parameterInput :: Integer -> Integer -> Integer -> FixedBatchParameterInput
parameterInput horizon externalCap demandCap =
    FixedBatchParameterInput
        { inputFixedBatchHorizon = horizon
        , inputFixedBatchSupplierLeadTime = 2
        , inputFixedBatchQ1 = 1
        , inputFixedBatchQ2 = 2
        , inputFixedBatchExternalBatchCap = externalCap
        , inputFixedBatchDemandCap = demandCap
        , inputFixedBatchStage1HoldingCost = 1
        , inputFixedBatchStage2HoldingCost = 1
        , inputFixedBatchBacklogCost = 4
        , inputFixedBatchStateBudget = 500000
        , inputFixedBatchStateActionBudget = 20000000
        , inputFixedBatchSolverWorkBudget = 2000000000
        , inputFixedBatchGridBudget = 1000
        , inputFixedBatchConvolutionBudget = 2000000
        }

gridValues :: Integer -> Integer -> NonEmpty ReorderLevels
gridValues lower upper =
    ReorderLevels lower lower
        :| [ReorderLevels r1 r2 | r1 <- [lower .. upper], r2 <- [lower .. upper], (r1, r2) /= (lower, lower)]

forceSemantic :: Semantic -> IO ()
forceSemantic result = do
    _ <- evaluate (length (semanticReport result))
    _ <- evaluate (semanticOracle result)
    _ <- evaluate (semanticPolicy result)
    _ <- evaluate (semanticStationary result)
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
