module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Markovian.Backend.Neural (
    DQNTargetSelection (StandardDQN),
    mkActionMask,
    mkDQNConfig,
    mkDQNState,
    mkDenseNetwork,
    mkReplayBuffer,
    mkSGD,
    periodicHardTargetUpdates,
 )
import Markovian.Backend.Neural.Bridge.DQN.Trainer (
    DQNEnvironmentStep (DQNContinuing),
    DQNTrainerFuel,
    DQNTrainerLimits,
    DQNTrainerReport,
    DQNTrainerState,
    DQNTrainerStop (DQNTrainerFuelExhausted),
    dqnTrainerReportFinalAccounting,
    dqnTrainerReportFuelUsed,
    dqnTrainerReportSteps,
    dqnTrainerReportStop,
    dqnTrainerRunReport,
    mkDQNTrainerConfig,
    mkDQNTrainerFuel,
    mkDQNTrainerLimits,
    mkDQNTrainerObservation,
    mkDQNTrainerState,
    runDQNTrainer,
 )
import Markovian.Sampling (generatorFromSeed)
import System.Info (arch, compilerName, compilerVersion, os)

main :: IO ()
main = do
    (initial, environment) <- benchmarkFixture
    initialRef <- newIORef initial
    fuel <- mustRight "benchmark fuel" (mkDQNTrainerFuel 32)
    let limits = mkDQNTrainerLimits 32 32 116 29 29 238
        action = benchmarkRun fuel limits environment initialRef
    warmup <- action
    samples <- replicateM 20 (timed action)
    final <- action
    putStrLn "D084 bounded DQN trainer benchmark"
    putStrLn "scope: one fixed pure continuing callback, behavior and replay sampling, FIFO append, DQN batch updates, and target checkpoints"
    putStrLn "excluded: Cabal startup, compilation, initial fixture construction, and report printing"
    putStrLn "claim boundary: this is neither accelerator, throughput, convergence, nor production evidence"
    putStrLn "warm-up samples excluded: 1"
    putStrLn "measured samples: 20"
    putStrLn "clock: GHC.Clock.getMonotonicTimeNSec"
    putStrLn ("compiler: " ++ compilerName ++ " " ++ show compilerVersion)
    putStrLn ("platform: " ++ arch ++ "-" ++ os)
    putStrLn "configuration: input=2, output=2, epsilon=0.25, batch=4, warm-up=4, replay-capacity=16, fuel=32, target-period=4, seed=20260914"
    if warmup == final
        then pure ()
        else fail "benchmark identical fixture produced different semantic receipts"
    putStrLn (semanticReceipt final)
    putStrLn ("raw elapsed-ns: " ++ show samples)
    putStrLn ("mean elapsed-ns: " ++ show (mean samples))
    putStrLn ("sample standard deviation elapsed-ns: " ++ show (sampleStandardDeviation samples))
    putStrLn ("minimum elapsed-ns: " ++ show (minimum samples))
    putStrLn ("maximum elapsed-ns: " ++ show (maximum samples))

benchmarkFixture :: IO (DQNTrainerState Int, Int -> Int -> Either String (DQNEnvironmentStep Int))
benchmarkFixture = do
    mask <- mustRight "benchmark action mask" (mkActionMask 2 [0, 1])
    observation <- mustRight "benchmark observation" (mkDQNTrainerObservation [1, 0] mask)
    successorEven <- mustRight "benchmark even successor" (mkDQNTrainerObservation [0, 1] mask)
    successorOdd <- mustRight "benchmark odd successor" (mkDQNTrainerObservation [1, 0] mask)
    online <- mustRight "benchmark online" (mkDenseNetwork 2 [] 2 [0.2, -0.1, -0.3, 0.4, 0, 0])
    target <- mustRight "benchmark target" (mkDenseNetwork 2 [] 2 [0, 0, 0, 0, 0, 0])
    optimizer <- mustRight "benchmark optimizer" (mkSGD 0.01)
    schedule <- mustRight "benchmark target schedule" (periodicHardTargetUpdates 4)
    dqnConfig <- mustRight "benchmark DQN config" (mkDQNConfig 0.9 optimizer StandardDQN schedule)
    dqnState <- mustRight "benchmark DQN state" (mkDQNState online target)
    trainerConfig <- mustRight "benchmark trainer config" (mkDQNTrainerConfig dqnConfig 0.25 4 4)
    replay <- mustRight "benchmark replay" (mkReplayBuffer 16)
    initial <- mustRight "benchmark trainer state" (mkDQNTrainerState trainerConfig (0 :: Int) observation dqnState replay (generatorFromSeed 20260914))
    pure
        ( initial
        , \state action ->
            Right
                ( DQNContinuing
                    (state + 1)
                    (if even state then successorEven else successorOdd)
                    (if action == 0 then 0.25 else 0.5)
                )
        )

benchmarkRun ::
    DQNTrainerFuel ->
    DQNTrainerLimits ->
    (Int -> Int -> Either String (DQNEnvironmentStep Int)) ->
    IORef (DQNTrainerState Int) ->
    IO (DQNTrainerReport String)
benchmarkRun fuel limits environment initialRef = do
    initial <- freshInitial initialRef
    result <- evaluate (runDQNTrainer fuel limits environment initial)
    case dqnTrainerReportStop (dqnTrainerRunReport result) of
        DQNTrainerFuelExhausted -> pure (dqnTrainerRunReport result)
        stop -> fail ("benchmark trainer did not exhaust fuel: " ++ show stop)

timed :: IO result -> IO Word64
timed action = do
    start <- getMonotonicTimeNSec
    _ <- action
    finish <- getMonotonicTimeNSec
    pure (finish - start)

mean :: [Word64] -> Double
mean values = fromIntegral (sum values) / fromIntegral (length values)

sampleStandardDeviation :: [Word64] -> Double
sampleStandardDeviation values
    | length values < 2 = 0
    | otherwise =
        sqrt
            ( sum [difference * difference | value <- values, let difference = fromIntegral value - average]
                / fromIntegral (length values - 1)
            )
  where
    average = mean values

semanticReceipt :: (Show callbackError) => DQNTrainerReport callbackError -> String
semanticReceipt report =
    "semantic receipt: stop="
        ++ show (dqnTrainerReportStop report)
        ++ ", fuel-used="
        ++ show (dqnTrainerReportFuelUsed report)
        ++ ", final-accounting="
        ++ show (dqnTrainerReportFinalAccounting report)
        ++ ", step-count="
        ++ show (length (dqnTrainerReportSteps report))

freshInitial :: IORef state -> IO state
freshInitial = readIORef
{-# NOINLINE freshInitial #-}

mustRight :: (Show error) => String -> Either error value -> IO value
mustRight _ (Right value) = pure value
mustRight label (Left problem) = fail (label ++ ": " ++ show problem)
