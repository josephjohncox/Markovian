module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio ((%))
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTimeNSec)
import Markovian.Benchmark.Inventory.Report (
    inventoryBenchmarkReport,
    inventoryBenchmarkSampleCount,
    inventoryBenchmarkWarmupCount,
    renderInventoryBenchmarkReport,
 )
import Markovian.Benchmark.Inventory.Serial.Exact (
    BaseStockTargetGrid,
    SerialInventoryError,
    SerialInventoryParameterInput (..),
    SerialInventoryState (..),
    baseStockTargetGrid,
    serialInventoryFixture,
    serialInventoryStateActionCount,
    serialInventoryStateCount,
    solveSerialInventory,
    validateSerialInventoryParameters,
 )
import Markovian.Objective.Exact (mkExactDiscount)
import Numeric.Natural (Natural)
import System.Info (arch, compilerName, compilerVersion, os)

main :: IO ()
main = do
    warmups <- replicateM (fromIntegral inventoryBenchmarkWarmupCount) semanticRun
    mapM_ forceSemantic warmups
    measured <- replicateM (fromIntegral inventoryBenchmarkSampleCount) measureSemantic
    case measured of
        [] -> fail "inventory benchmark requires at least one measured sample"
        first : remaining -> do
            let reference = measuredSemantic first
            mapM_ (requireSameSemantics reference . measuredSemantic) remaining
            putStr (semanticText reference)
            putStrLn "benchmark metadata"
            putStrLn ("compiler: " ++ compilerName ++ "-" ++ showVersion compilerVersion)
            putStrLn ("os: " ++ os)
            putStrLn ("architecture: " ++ arch)
            putStrLn ("warm-up samples excluded: " ++ show inventoryBenchmarkWarmupCount)
            putStrLn ("measured samples: " ++ show inventoryBenchmarkSampleCount)
            putStrLn ("model states: " ++ show (semanticStateCount reference))
            putStrLn ("model state-actions: " ++ show (semanticStateActionCount reference))
            mapM_ renderRawSample (zip [1 :: Natural ..] measured)
            renderSummary (fmap measuredNanoseconds measured)

data SemanticResult = SemanticResult
    { semanticText :: !String
    , semanticStateCount :: !Natural
    , semanticStateActionCount :: !Natural
    }
    deriving (Eq, Show)

data MeasuredResult = MeasuredResult
    { measuredNanoseconds :: !Natural
    , measuredSemantic :: !SemanticResult
    }

semanticRun :: IO SemanticResult
semanticRun = do
    discount <- requireRight "discount" (mkExactDiscount (1 % 2))
    primaryParameters <-
        requireRight
            "primary parameters"
            ( validateSerialInventoryParameters
                SerialInventoryParameterInput
                    { inputSerialInventoryHorizon = 2
                    , inputSerialInventoryDiscount = discount
                    , inputSerialInventoryOrderCap = 1
                    , inputSerialInventoryDemandCap = 1
                    , inputSerialInventoryUpstreamHoldingCost = 1
                    , inputSerialInventoryDownstreamHoldingCost = 1
                    , inputSerialInventoryBacklogCost = 4
                    }
            )
    widenedParameters <-
        requireRight
            "widened parameters"
            ( validateSerialInventoryParameters
                SerialInventoryParameterInput
                    { inputSerialInventoryHorizon = 2
                    , inputSerialInventoryDiscount = discount
                    , inputSerialInventoryOrderCap = 2
                    , inputSerialInventoryDemandCap = 1
                    , inputSerialInventoryUpstreamHoldingCost = 1
                    , inputSerialInventoryDownstreamHoldingCost = 1
                    , inputSerialInventoryBacklogCost = 4
                    }
            )
    let initial = SerialInventoryState 2 1 0 0
    primaryFixture <- requireRight "primary fixture" (serialInventoryFixture primaryParameters initial)
    widenedFixture <- requireRight "widened fixture" (serialInventoryFixture widenedParameters initial)
    primaryGrid <- requireRight "primary target grid" (targetGrid 2 2)
    widenedGrid <- requireRight "widened target grid" (targetGrid 2 3)
    primary <- requireRight "primary solve" (solveSerialInventory primaryGrid primaryFixture)
    widened <- requireRight "widened solve" (solveSerialInventory widenedGrid widenedFixture)
    reportValue <- requireRight "validated bound report" (inventoryBenchmarkReport primary widened)
    let report = renderInventoryBenchmarkReport reportValue
    pure
        SemanticResult
            { semanticText = report
            , semanticStateCount = serialInventoryStateCount primary
            , semanticStateActionCount = serialInventoryStateActionCount primary
            }

forceSemantic :: SemanticResult -> IO ()
forceSemantic result = do
    _ <- evaluate (length (semanticText result))
    _ <- evaluate (semanticStateCount result)
    _ <- evaluate (semanticStateActionCount result)
    pure ()

measureSemantic :: IO MeasuredResult
measureSemantic = do
    start <- getMonotonicTimeNSec
    semantic <- semanticRun
    forceSemantic semantic
    finish <- getMonotonicTimeNSec
    pure
        MeasuredResult
            { measuredNanoseconds = fromIntegral (finish - start)
            , measuredSemantic = semantic
            }

requireSameSemantics :: SemanticResult -> SemanticResult -> IO ()
requireSameSemantics expected actual =
    if actual == expected
        then pure ()
        else fail "timing sample changed the exact inventory semantic result"

renderRawSample :: (Natural, MeasuredResult) -> IO ()
renderRawSample (index, result) =
    putStrLn
        ( "sample "
            ++ show index
            ++ " elapsed-ns: "
            ++ show (measuredNanoseconds result)
        )

renderSummary :: [Natural] -> IO ()
renderSummary samples = do
    let values = fmap fromIntegral samples :: [Double]
        count = fromIntegral (length values)
        mean = sum values / count
        variance
            | length values < 2 = 0
            | otherwise = sum (fmap (\value -> (value - mean) ^ (2 :: Natural)) values) / (count - 1)
    putStrLn ("mean elapsed-ns: " ++ show mean)
    putStrLn ("sample standard deviation elapsed-ns: " ++ show (sqrt variance))
    putStrLn ("minimum elapsed-ns: " ++ show (minimum samples))
    putStrLn ("maximum elapsed-ns: " ++ show (maximum samples))

targetGrid :: Natural -> Natural -> Either SerialInventoryError BaseStockTargetGrid
targetGrid horizon maximumTarget =
    baseStockTargetGrid (candidatePairs :| replicate (fromIntegral horizon - 1) candidatePairs)
  where
    candidatePairs =
        case NonEmpty.nonEmpty [(upstream, downstream) | upstream <- [0 .. maximumTarget], downstream <- [0 .. maximumTarget]] of
            Nothing -> (0, 0) :| []
            Just pairs -> pairs

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left err) = fail (label ++ ": " ++ show err)
