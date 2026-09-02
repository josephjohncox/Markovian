{-# LANGUAGE DataKinds #-}

module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (replicateM, unless)
import Data.Version (showVersion)
import Markovian.Autodiff
import System.CPUTime (getCPUTime)
import System.Info (arch, compilerName, compilerVersion, os)
import Text.Printf (printf)

type SquareParameters =
    'ParameterProduct
        ('ParameterProduct 'NoParameters 'NoParameters)
        'NoParameters

program :: Program Double 'Polynomial SquareParameters 'Scalar 'Scalar
program = compose (fanout (identity SScalar) (identity SScalar)) multiplyScalar

parameters :: ParameterValue Double SquareParameters
parameters = parameterProduct (parameterProduct noParameters noParameters) noParameters

limits :: CompilerLimits
limits = compilerLimits 100 100 30 4 32 32 200 30 10000 256

main :: IO ()
main = case compileDoublePolynomial limits StorePullbacks program of
    Left problem -> error (show problem)
    Right executable -> do
        _ <- sample executable -- excluded warmup
        samples <- replicateM 20 (sample executable)
        let observations = map snd samples
            durations = map fst samples
            pinnedObservation = 4.0625
            checksum = sum observations
        unless (all (== pinnedObservation) observations && checksum == 81.25) $
            fail "benchmark changed its committed semantic observation"
        putStrLn "markovian-autodiff-benchmark"
        putStrLn ("toolchain: " ++ compilerName ++ "-" ++ showVersion compilerVersion)
        putStrLn ("host: " ++ os ++ "/" ++ arch)
        putStrLn "warmups-excluded: 1"
        putStrLn "samples: 20"
        putStrLn "unit: cpu-picoseconds"
        mapM_ (\(index, duration) -> printf "sample-%02d: %d\n" (index :: Int) duration) (zip [1 ..] durations)
        putStrLn ("semantic-observation: " ++ show pinnedObservation)
        putStrLn ("semantic-checksum: " ++ show checksum)

sample :: DoubleExecutable 'Polynomial SquareParameters 'Scalar 'Scalar -> IO (Integer, Double)
sample executable = do
    start <- getCPUTime
    observation <- case runDouble executable parameters (scalarValue 1.25) of
        Left problem -> error (show problem)
        Right run -> case applyDoubleVJP run (scalarValue 1) of
            Left problem -> error (show problem)
            Right (_, gradient) -> evaluate (scalarFromValue (doubleRunOutput run) + scalarFromValue gradient)
    end <- getCPUTime
    pure (end - start, observation)
