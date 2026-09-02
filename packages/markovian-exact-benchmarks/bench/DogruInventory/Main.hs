module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTimeNSec)
import Markovian.Benchmark.Inventory.Distribution.Dogru.Exact
import Markovian.Benchmark.Inventory.Distribution.Dogru.Report
import Numeric.Natural (Natural)
import System.IO (hSetEncoding, stdout, utf8)
import System.Info (arch, compilerName, compilerVersion, os)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    warmups <- replicateM (fromIntegral dogruBenchmarkWarmupCount) semanticRun
    mapM_ forceSemantic warmups
    measured <- replicateM (fromIntegral dogruBenchmarkSampleCount) measure
    case measured of
        [] -> fail "Dogru benchmark requires measured samples"
        first : remaining -> do
            mapM_ (requireSame (measuredSemantic first) . measuredSemantic) remaining
            putStr (semanticReport (measuredSemantic first))
            putStrLn "benchmark metadata (local reproducibility only)"
            putStrLn ("compiler: " ++ compilerName ++ "-" ++ showVersion compilerVersion)
            putStrLn ("os: " ++ os ++ ", architecture: " ++ arch)
            putStrLn ("warm-up samples excluded: " ++ show dogruBenchmarkWarmupCount)
            putStrLn ("measured complete samples: " ++ show dogruBenchmarkSampleCount)
            mapM_ renderSample (zip [1 :: Natural ..] measured)

data Semantic = Semantic
    { semanticReport :: !String
    , semanticRelaxedCost :: !Rational
    , semanticPhysicalCost :: !Rational
    , semanticHeuristicCost :: !Rational
    }
    deriving (Eq, Show)

data Measured = Measured
    { measuredNanoseconds :: !Natural
    , measuredSemantic :: !Semantic
    }

semanticRun :: IO Semantic
semanticRun = do
    primary <- solve 4 1
    widened <- solve 5 2
    report <- requireRight "report" (dogruReport primary widened)
    pure
        Semantic
            { semanticReport = renderDogruReport report
            , semanticRelaxedCost = dogruRelaxedOracleCost primary
            , semanticPhysicalCost = dogruPhysicalOracleCost primary
            , semanticHeuristicCost = dogruPhysicalizedLBHeuristicCost primary
            }
  where
    solve orderCap returnCap = do
        parameters <- requireRight "parameters" (validateDogruParameters (dogruScenario2Input 2 orderCap returnCap))
        fixture <- requireRight "fixture" (dogruFixture parameters (DogruState 2 0 0 (-1) 1))
        requireRight "solve" (solveDogru fixture)

forceSemantic :: Semantic -> IO ()
forceSemantic result = do
    _ <- evaluate (length (semanticReport result))
    _ <- evaluate (semanticRelaxedCost result)
    _ <- evaluate (semanticPhysicalCost result)
    _ <- evaluate (semanticHeuristicCost result)
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
