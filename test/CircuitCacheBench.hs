{-# LANGUAGE GADTs #-}

-- Run with RTS statistics enabled. Each sample obtains its immutable starting
-- owner from IO, so optimization cannot reuse a prior pure result thunk.
module Main (main) where

import CircuitCacheFixtures
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM_)
import Data.IORef
import Data.Ratio ((%))
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (allocated_bytes, getRTSStats, getRTSStatsEnabled)
import Markovian.Open.Acyclic.Circuit.Exact
import System.CPUTime (getCPUTime)
import System.Info (arch, compilerName, compilerVersion, os)
import System.Mem (performGC)

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    assertCache "RTS allocation statistics required; use +RTS -T" enabled
    putStrLn ("compiler=" ++ compilerName ++ "-" ++ showVersion compilerVersion ++ " platform=" ++ arch ++ "-" ++ os)
    putStrLn "scope=complete-public-call; local-table-ledgers-only; setup-and-warmup-excluded; samples=3"
    putStrLn "case,path,sample,edges,local_depth,cpu_ps,wall_ns,allocated_bytes,source_work,executor_operations,executor_reserved,lookup_comparisons,constructions,hits,resident_entries,resident_cells,resident_trace_slots"
    forM_ [("small", 8, 4), ("large", 16, 16)] $ \(name, edges, depth) -> do
        SomeAcyclicOpenCircuit _ circuit <- chainFixture edges depth (1 % 2) False
        let emptyOwner = retainAcyclicOpenCircuit ExactTableInterpreterV1 circuit
        (_, warmOwner, _) <- rightCache (retainedAcyclicOpenCircuitDenotation RetainCircuitTables semanticLimits infrastructureLimits emptyOwner)
        forM_ [("uncached", UncachedCircuitTables, emptyOwner), ("cold", RetainCircuitTables, emptyOwner), ("warm", RetainCircuitTables, warmOwner)] $ \(path, mode, startingOwner) -> do
            starting <- newIORef startingOwner
            sink <- newIORef startingOwner
            let sample = do
                    owner <- readIORef starting
                    result <- evaluate (retainedAcyclicOpenCircuitDenotation mode semanticLimits infrastructureLimits owner)
                    (matrix, next, report) <- rightCache result
                    _ <- evaluate (foldl' (foldl' (\() value -> value `seq` ())) () (rowsCache matrix))
                    _ <- evaluate report
                    -- The production boundary completes cache/table/trace spines
                    -- before returning; retain the resulting owner inside timing.
                    writeIORef sink next
                    pure report
            replicateM_ 1 sample
            forM_ [1 :: Int, 2, 3] $ \sampleIndex -> do
                performGC
                before <- getRTSStats
                cpuStart <- getCPUTime
                wallStart <- getMonotonicTimeNSec
                report <- sample
                wallEnd <- getMonotonicTimeNSec
                cpuEnd <- getCPUTime
                performGC
                after <- getRTSStats
                let allocated = allocated_bytes after - allocated_bytes before
                assertCache "sample allocated no memory; repeated-result sharing suspected" (allocated > 0)
                let fields =
                        [ show sampleIndex
                        , show edges
                        , show depth
                        , show (cpuEnd - cpuStart)
                        , show (wallEnd - wallStart)
                        , show allocated
                        , show (cacheSourceWork report)
                        , show (cacheExecutorOperations report)
                        , show (cacheExecutorReserved report)
                        , show (cacheLookupComparisons report)
                        , show (cacheConstructions report)
                        , show (cacheHits report)
                        , show (cacheResidentEntries report)
                        , show (cacheResidentCells report)
                        , show (cacheResidentTraceSlots report)
                        ]
                putStrLn (concatWithComma (name : path : fields))

concatWithComma :: [String] -> String
concatWithComma [] = ""
concatWithComma (value : remaining) = value ++ concatMap (',' :) remaining
