module DQNTrainer (tests) where

import Control.Exception (SomeException, evaluate, try)
import Data.Maybe (isJust, isNothing)
import Data.Word (Word64)
import Markovian.Backend.Neural (
    DQNConfig,
    DQNState,
    DQNTargetSelection (..),
    ReplayEntryId,
    TargetUpdateSchedule,
    appendReplay,
    denseParameters,
    dqnBatchTargets,
    dqnOnlineNetwork,
    dqnTargetNetwork,
    mkActionMask,
    mkDQNConfig,
    mkDQNState,
    mkDenseNetwork,
    mkReplayBuffer,
    mkSGD,
    mkTerminalTransition,
    noAutomaticTargetUpdates,
    periodicHardTargetUpdates,
    polyakTargetUpdates,
    replayEntries,
    replayEntryId,
    replayEntryIdOrdinal,
    targetNetworkSnapshot,
    targetSuccessfulUpdateCount,
 )
import Markovian.Backend.Neural.Bridge.DQN.Trainer qualified as Trainer
import Markovian.Probability (finiteDist)
import Markovian.Sampling (Generator, generatorFromSeed, generatorState, sampleFiniteDist)
import Numeric.Natural (Natural)
import System.Exit (exitFailure)

data CallbackError = CallbackFailure
    deriving (Eq, Show)

tests :: IO ()
tests = do
    constructionChecks
    fuelAndLimitChecks
    splitAndRngChecks
    replayAndTargetChecks
    targetRuleChecks
    terminalAndFailureChecks
    putStrLn "PASS: DQN trainer"

constructionChecks :: IO ()
constructionChecks = do
    dqn <- baselineDQN StandardDQN noAutomaticTargetUpdates 0.5
    mask <- requireRight "trainer construction mask" (mkActionMask 2 [0, 1])
    observation <- requireRight "trainer construction observation" (Trainer.mkDQNTrainerObservation [1] mask)
    replay <- requireRight "trainer construction replay" (mkReplayBuffer 1)
    case Trainer.mkDQNTrainerConfig (fst dqn) (-0.1) 1 1 of
        Left (Trainer.InvalidDQNTrainerExplorationRate (-0.1)) -> pure ()
        result -> assert ("negative exploration accepted: " ++ show result) False
    case Trainer.mkDQNTrainerConfig (fst dqn) 0 0 1 of
        Left (Trainer.InvalidDQNTrainerBatchSize 0) -> pure ()
        result -> assert ("zero batch accepted: " ++ show result) False
    case Trainer.mkDQNTrainerConfig (fst dqn) 0 2 1 of
        Left (Trainer.DQNTrainerWarmupBelowBatch 1 2) -> pure ()
        result -> assert ("warm-up below batch accepted: " ++ show result) False
    case Trainer.mkDQNTrainerFuel 0 of
        Left Trainer.DQNTrainerFuelMustBePositive -> pure ()
        result -> assert ("zero fuel accepted: " ++ show result) False
    config <- requireRight "trainer construction config" (Trainer.mkDQNTrainerConfig (fst dqn) 0 1 2)
    case Trainer.mkDQNTrainerState config (0 :: Int) observation (snd dqn) replay (generatorFromSeed 1) of
        Left (Trainer.DQNTrainerReplayCapacityBelowWarmup 1 2) -> pure ()
        result -> assert ("undersized replay accepted: " ++ show result) False
    wrongObservation <- requireRight "trainer wrong observation" (Trainer.mkDQNTrainerObservation [1, 2] mask)
    configOne <- requireRight "trainer valid config" (Trainer.mkDQNTrainerConfig (fst dqn) 0 1 1)
    case Trainer.mkDQNTrainerState configOne (0 :: Int) wrongObservation (snd dqn) replay (generatorFromSeed 1) of
        Left (Trainer.DQNTrainerFeatureWidthMismatch 1 2) -> pure ()
        result -> assert ("wrong feature width accepted: " ++ show result) False
    transition <- requireRight "incompatible replay transition" (mkTerminalTransition [1, 2] mask 0 0 0)
    let (_, incompatibleReplay) = appendReplay transition replay
    case Trainer.mkDQNTrainerState configOne (0 :: Int) observation (snd dqn) incompatibleReplay (generatorFromSeed 1) of
        Left (Trainer.DQNTrainerReplayTransitionIncompatible _ (Trainer.DQNTrainerFeatureWidthMismatch 1 2)) -> pure ()
        result -> assert ("incompatible replay accepted: " ++ show result) False
    let strictConstructor =
            Trainer.mkDQNTrainerState
                configOne
                (error "poison trainer environment" :: Int)
                observation
                (snd dqn)
                replay
                (generatorFromSeed 1)
    construction <-
        try (evaluate strictConstructor) :: IO (Either SomeException (Either Trainer.DQNTrainerStateError (Trainer.DQNTrainerState Int)))
    case construction of
        Left _ -> pure ()
        Right _ -> assert "trainer construction deferred a bottom environment" False

fuelAndLimitChecks :: IO ()
fuelAndLimitChecks = do
    initial <- baselineState 0 1 1 noAutomaticTargetUpdates 0.5 1
    fuel <- requireRight "one trainer fuel" (Trainer.mkDQNTrainerFuel 1)
    let exact = limits 1 1 1 1 1 5
        complete = Trainer.runDQNTrainer fuel exact continuing initial
        report = Trainer.dqnTrainerRunReport complete
        accounting = Trainer.dqnTrainerRunState complete
    assert "exact trainer limit completed" (Trainer.dqnTrainerReportStop report == Trainer.DQNTrainerFuelExhausted)
    assert
        "exact cumulative accounting"
        ( Trainer.dqnTrainerAttemptedTransitions (Trainer.dqnTrainerStateAccounting accounting) == 1
            && Trainer.dqnTrainerCommittedReplayAppends (Trainer.dqnTrainerStateAccounting accounting) == 1
            && Trainer.dqnTrainerSelectedBatchEntries (Trainer.dqnTrainerStateAccounting accounting) == 1
            && Trainer.dqnTrainerAttemptedOnlineUpdates (Trainer.dqnTrainerStateAccounting accounting) == 1
            && Trainer.dqnTrainerCommittedCheckpointAdvances (Trainer.dqnTrainerStateAccounting accounting) == 1
            && Trainer.dqnTrainerProtocolWork (Trainer.dqnTrainerStateAccounting accounting) == 5
        )
    checkOneBelow initial fuel Trainer.DQNTrainerTransitions (limits 0 9 9 9 9 9)
    checkOneBelow initial fuel Trainer.DQNTrainerReplayAppends (limits 9 0 9 9 9 9)
    checkOneBelow initial fuel Trainer.DQNTrainerBatchEntries (limits 9 9 0 9 9 9)
    checkOneBelow initial fuel Trainer.DQNTrainerOnlineUpdates (limits 9 9 9 0 9 9)
    checkOneBelow initial fuel Trainer.DQNTrainerCheckpointAdvances (limits 9 9 9 9 0 9)
    checkOneBelow initial fuel Trainer.DQNTrainerProtocolWork (limits 9 9 9 9 9 4)

checkOneBelow ::
    Trainer.DQNTrainerState Int ->
    Trainer.DQNTrainerFuel ->
    Trainer.DQNTrainerDimension ->
    Trainer.DQNTrainerLimits ->
    IO ()
checkOneBelow initial fuel dimension restricted = do
    let result = Trainer.runDQNTrainer fuel restricted continuing initial
        report = Trainer.dqnTrainerRunReport result
    case Trainer.dqnTrainerReportStop report of
        Trainer.DQNTrainerFailed (Trainer.DQNTrainerLimitExceeded actual _ _) ->
            assert ("one-below dimension changed: " ++ show actual) (actual == dimension)
        other -> assert ("one-below limit did not fail: " ++ show other) False
    assert "preflight limit changed state" (Trainer.dqnTrainerRunState result == initial)
    assert "preflight limit consumed fuel" (Trainer.dqnTrainerReportFuelUsed report == 0)

splitAndRngChecks :: IO ()
splitAndRngChecks = do
    initial <- baselineState 0 2 2 noAutomaticTargetUpdates 0.5 47
    one <- requireRight "split one fuel" (Trainer.mkDQNTrainerFuel 1)
    two <- requireRight "split two fuel" (Trainer.mkDQNTrainerFuel 2)
    let allLimits = limits 20 20 20 20 20 100
        whole = Trainer.runDQNTrainer two allLimits continuing initial
        first = Trainer.runDQNTrainer one allLimits continuing initial
        split = Trainer.runDQNTrainer one allLimits continuing (Trainer.dqnTrainerRunState first)
    assert "split run final state differs" (Trainer.dqnTrainerRunState whole == Trainer.dqnTrainerRunState split)
    assert
        "split run generator differs"
        ( generatorState (Trainer.dqnTrainerStateGenerator (Trainer.dqnTrainerRunState whole))
            == generatorState (Trainer.dqnTrainerStateGenerator (Trainer.dqnTrainerRunState split))
        )
    let wholeSteps = Trainer.dqnTrainerReportSteps (Trainer.dqnTrainerRunReport whole)
        splitSteps =
            Trainer.dqnTrainerReportSteps (Trainer.dqnTrainerRunReport first)
                ++ Trainer.dqnTrainerReportSteps (Trainer.dqnTrainerRunReport split)
    assert "split run semantic steps differ" (wholeSteps == splitSteps)
    case wholeSteps of
        firstStep : secondStep : _ -> do
            assert "epsilon-zero tie order changed" (Trainer.dqnTrainerStepAction firstStep == 0)
            let replayAfterWhole = Trainer.dqnTrainerStateReplayBuffer (Trainer.dqnTrainerRunState whole)
            let available = fmap replayEntryId (replayEntries replayAfterWhole)
            expected <- orderedDraw (generatorFromSeed 47) available
            assert
                "ordered replay draw changed"
                (Trainer.dqnTrainerStepBatchEntryIds secondStep == expected)
        _ -> assert "expected two trainer step reports" False
    exploratory <- baselineState 1 1 1 noAutomaticTargetUpdates 0.5 47
    exploratoryFuel <- requireRight "exploration fuel" (Trainer.mkDQNTrainerFuel 1)
    let exploratoryRun = Trainer.runDQNTrainer exploratoryFuel (limits 9 9 9 9 9 20) continuing exploratory
    assert
        "multi-action exploration did not advance generator"
        ( generatorState (Trainer.dqnTrainerStateGenerator (Trainer.dqnTrainerRunState exploratoryRun))
            /= generatorState (generatorFromSeed 47)
        )

orderedDraw :: Generator -> [ReplayEntryId] -> IO [ReplayEntryId]
orderedDraw generator available = do
    distribution <- requireRight "ordered first distribution" (finiteDist [(entryId, 1) | entryId <- available])
    (first, afterFirst) <- requireRight "ordered first sample" (sampleFiniteDist generator distribution)
    let remaining = filter (/= first) available
    distribution2 <- requireRight "ordered second distribution" (finiteDist [(entryId, 1) | entryId <- remaining])
    (second, _) <- requireRight "ordered second sample" (sampleFiniteDist afterFirst distribution2)
    pure [first, second]

replayAndTargetChecks :: IO ()
replayAndTargetChecks = do
    initial <- baselineStateWithCapacity 0 1 1 2 noAutomaticTargetUpdates 0.5 3
    fuel <- requireRight "three trainer fuel" (Trainer.mkDQNTrainerFuel 3)
    let result = Trainer.runDQNTrainer fuel (limits 20 20 20 20 20 100) continuing initial
        state = Trainer.dqnTrainerRunState result
        entryIds = fmap replayEntryId (replayEntries (Trainer.dqnTrainerStateReplayBuffer state))
    assert "trainer FIFO eviction changed" (fmap replayEntryIdOrdinal entryIds == [1, 2])
    let reports = Trainer.dqnTrainerReportSteps (Trainer.dqnTrainerRunReport result)
    assert
        "trainer replay append IDs changed"
        (fmap (fmap replayEntryIdOrdinal . Trainer.dqnTrainerStepReplayEntryId) reports == [Just 0, Just 1, Just 2])
    periodic <- baselineState 0 1 1 (requireSchedule 1) 0.5 3
    periodicFuel <- requireRight "periodic trainer fuel" (Trainer.mkDQNTrainerFuel 1)
    let periodicRun = Trainer.runDQNTrainer periodicFuel (limits 9 9 9 9 9 20) continuing periodic
        periodicDQN = Trainer.dqnTrainerStateDQNState (Trainer.dqnTrainerRunState periodicRun)
    assert "periodic checkpoint did not advance" (targetSuccessfulUpdateCount (dqnTargetNetwork periodicDQN) == 1)
    assertVectorClose
        "periodic checkpoint did not copy online"
        0
        (denseParameters (dqnOnlineNetwork periodicDQN))
        (denseParameters (targetNetworkSnapshot (dqnTargetNetwork periodicDQN)))
    polyakSchedule <- requireRight "polyak trainer schedule" (polyakTargetUpdates 0.5)
    polyak <- baselineState 0 1 1 polyakSchedule 0.5 3
    let polyakRun = Trainer.runDQNTrainer periodicFuel (limits 9 9 9 9 9 20) continuing polyak
        polyakDQN = Trainer.dqnTrainerStateDQNState (Trainer.dqnTrainerRunState polyakRun)
    assert "Polyak checkpoint did not advance" (targetSuccessfulUpdateCount (dqnTargetNetwork polyakDQN) == 1)
    assert
        "Polyak checkpoint unexpectedly equals online"
        (denseParameters (dqnOnlineNetwork polyakDQN) /= denseParameters (targetNetworkSnapshot (dqnTargetNetwork polyakDQN)))

targetRuleChecks :: IO ()
targetRuleChecks = do
    standard <- targetRuleRun StandardDQN
    double <- targetRuleRun DoubleDQN
    assertClose "trainer standard target" 1e-15 4 standard
    assertClose "trainer Double-DQN target" 1e-15 1 double

targetRuleRun :: DQNTargetSelection -> IO Double
targetRuleRun selection = do
    mask <- requireRight "target-rule mask" (mkActionMask 2 [0, 1])
    observation <- requireRight "target-rule observation" (Trainer.mkDQNTrainerObservation [1] mask)
    online <- requireRight "target-rule online" (mkDenseNetwork 1 [] 2 [0, 0, 0, 2])
    target <- requireRight "target-rule target" (mkDenseNetwork 1 [] 2 [0, 0, 4, 1])
    optimizer <- requireRight "target-rule optimizer" (mkSGD 0.1)
    dqnConfig <- requireRight "target-rule config" (mkDQNConfig 1 optimizer selection noAutomaticTargetUpdates)
    dqnState <- requireRight "target-rule DQN state" (mkDQNState online target)
    trainerConfig <- requireRight "target-rule trainer config" (Trainer.mkDQNTrainerConfig dqnConfig 0 1 1)
    replay <- requireRight "target-rule replay" (mkReplayBuffer 1)
    initial <- requireRight "target-rule initial" (Trainer.mkDQNTrainerState trainerConfig (0 :: Int) observation dqnState replay (generatorFromSeed 1))
    fuel <- requireRight "target-rule fuel" (Trainer.mkDQNTrainerFuel 1)
    let result = Trainer.runDQNTrainer fuel (limits 9 9 9 9 9 20) continuingZero initial
    case Trainer.dqnTrainerReportSteps (Trainer.dqnTrainerRunReport result) of
        [step] ->
            case Trainer.dqnTrainerStepBatchEvaluation step of
                Just evaluation ->
                    case dqnBatchTargets evaluation of
                        [targetValue] -> pure targetValue
                        targets -> fail ("unexpected target count: " ++ show targets)
                Nothing -> fail "target-rule run did not update"
        steps -> fail ("unexpected target-rule reports: " ++ show steps)

terminalAndFailureChecks :: IO ()
terminalAndFailureChecks = do
    initial <- baselineState 0 1 1 noAutomaticTargetUpdates 0.5 1
    fuel <- requireRight "terminal trainer fuel" (Trainer.mkDQNTrainerFuel 1)
    let terminalRun = Trainer.runDQNTrainer fuel (limits 9 9 9 9 9 20) terminal initial
        terminalReport = Trainer.dqnTrainerRunReport terminalRun
        terminalState = Trainer.dqnTrainerRunState terminalRun
    assert "terminal callback did not stop" (Trainer.dqnTrainerReportStop terminalReport == Trainer.DQNTrainerReachedTerminal)
    assert "terminal state retains next observation" (isNothing (Trainer.dqnTrainerStateObservation terminalState))
    case Trainer.dqnTrainerReportSteps terminalReport of
        [step] ->
            case Trainer.dqnTrainerStepBatchEvaluation step of
                Just evaluation ->
                    case dqnBatchTargets evaluation of
                        [targetValue] -> assertClose "terminal payoff target changed" 1e-15 3.5 targetValue
                        targets -> assert ("terminal target count changed: " ++ show targets) False
                Nothing -> assert "terminal did not update" False
        reports -> assert ("terminal report count changed: " ++ show reports) False
    let rerun = Trainer.runDQNTrainer fuel (limits 9 9 9 9 9 20) terminal terminalState
    assert "terminal state resumed" (Trainer.dqnTrainerReportStop (Trainer.dqnTrainerRunReport rerun) == Trainer.DQNTrainerReachedTerminal)
    continuingInitial <- baselineState 0 1 1 noAutomaticTargetUpdates 0.5 1
    let truncated = Trainer.runDQNTrainer fuel (limits 9 9 9 9 9 20) continuing continuingInitial
    assert "fuel cutoff did not return ready continuation" (isJust (Trainer.dqnTrainerStateObservation (Trainer.dqnTrainerRunState truncated)))
    exploratory <- baselineState 1 1 1 noAutomaticTargetUpdates 0.5 1
    let callbackFailure = Trainer.runDQNTrainer fuel (limits 9 9 9 9 9 20) failing exploratory
        callbackState = Trainer.dqnTrainerRunState callbackFailure
    case Trainer.dqnTrainerReportStop (Trainer.dqnTrainerRunReport callbackFailure) of
        Trainer.DQNTrainerFailed (Trainer.DQNTrainerCallbackFailure CallbackFailure) -> pure ()
        other -> assert ("callback failure changed: " ++ show other) False
    assert "callback failure appended replay" (Trainer.dqnTrainerCommittedReplayAppends (Trainer.dqnTrainerStateAccounting callbackState) == 0)
    assert
        "callback failure did not retain behavior RNG"
        (generatorState (Trainer.dqnTrainerStateGenerator callbackState) /= generatorState (generatorFromSeed 1))
    let payloadFailure = Trainer.runDQNTrainer fuel (limits 9 9 9 9 9 20) badPayload initial
    case Trainer.dqnTrainerReportStop (Trainer.dqnTrainerRunReport payloadFailure) of
        Trainer.DQNTrainerFailed (Trainer.DQNTrainerSuccessorStateFailure (Trainer.DQNTrainerRewardNumericFailure _)) -> pure ()
        other -> assert ("payload failure changed: " ++ show other) False
    overflowInitial <- baselineState 0 1 1 noAutomaticTargetUpdates 1 1
    let updateFailure = Trainer.runDQNTrainer fuel (limits 9 9 9 9 9 20) overflowTerminal overflowInitial
        failedState = Trainer.dqnTrainerRunState updateFailure
    case Trainer.dqnTrainerReportStop (Trainer.dqnTrainerRunReport updateFailure) of
        Trainer.DQNTrainerFailed (Trainer.DQNTrainerUpdateFailure _) -> pure ()
        other -> assert ("update failure changed: " ++ show other) False
    assert
        "failed update advanced target count"
        (targetSuccessfulUpdateCount (dqnTargetNetwork (Trainer.dqnTrainerStateDQNState failedState)) == 0)
    assert
        "failed update discarded replay"
        (Trainer.dqnTrainerCommittedReplayAppends (Trainer.dqnTrainerStateAccounting failedState) == 1)
    assert
        "failed update did not record attempt"
        (Trainer.dqnTrainerAttemptedOnlineUpdates (Trainer.dqnTrainerStateAccounting failedState) == 1)

baselineDQN :: DQNTargetSelection -> TargetUpdateSchedule -> Double -> IO (DQNConfig, DQNState)
baselineDQN selection schedule discount = do
    online <- requireRight "baseline online" (mkDenseNetwork 1 [] 2 [0, 0, 0, 0])
    target <- requireRight "baseline target" (mkDenseNetwork 1 [] 2 [0, 0, 0, 0])
    optimizer <- requireRight "baseline optimizer" (mkSGD 0.1)
    config <- requireRight "baseline DQN config" (mkDQNConfig discount optimizer selection schedule)
    state <- requireRight "baseline DQN state" (mkDQNState online target)
    pure (config, state)

baselineState ::
    Double ->
    Int ->
    Int ->
    TargetUpdateSchedule ->
    Double ->
    Word64 ->
    IO (Trainer.DQNTrainerState Int)
baselineState epsilon batchSize warmup schedule discount seed = do
    baselineStateWithCapacity epsilon batchSize warmup warmup schedule discount seed

baselineStateWithCapacity ::
    Double ->
    Int ->
    Int ->
    Int ->
    TargetUpdateSchedule ->
    Double ->
    Word64 ->
    IO (Trainer.DQNTrainerState Int)
baselineStateWithCapacity epsilon batchSize warmup capacity schedule discount seed = do
    (dqnConfig, dqnState) <- baselineDQN StandardDQN schedule discount
    mask <- requireRight "baseline mask" (mkActionMask 2 [0, 1])
    observation <- requireRight "baseline observation" (Trainer.mkDQNTrainerObservation [1] mask)
    trainerConfig <- requireRight "baseline trainer config" (Trainer.mkDQNTrainerConfig dqnConfig epsilon batchSize warmup)
    replay <- requireRight "baseline replay" (mkReplayBuffer capacity)
    requireRight "baseline trainer state" (Trainer.mkDQNTrainerState trainerConfig (0 :: Int) observation dqnState replay (generatorFromSeed seed))

limits :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Trainer.DQNTrainerLimits
limits = Trainer.mkDQNTrainerLimits

continuing :: Int -> Int -> Either CallbackError (Trainer.DQNEnvironmentStep Int)
continuing environment _ = Right (Trainer.DQNContinuing (environment + 1) (mustObservation [1] [0, 1]) 1)

continuingZero :: Int -> Int -> Either CallbackError (Trainer.DQNEnvironmentStep Int)
continuingZero environment _ = Right (Trainer.DQNContinuing (environment + 1) (mustObservation [1] [0, 1]) 0)

terminal :: Int -> Int -> Either CallbackError (Trainer.DQNEnvironmentStep Int)
terminal environment _ = Right (Trainer.DQNTerminal (environment + 1) 2 3)

overflowTerminal :: Int -> Int -> Either CallbackError (Trainer.DQNEnvironmentStep Int)
overflowTerminal environment _ = Right (Trainer.DQNTerminal (environment + 1) maxFinite maxFinite)

failing :: Int -> Int -> Either CallbackError (Trainer.DQNEnvironmentStep Int)
failing _ _ = Left CallbackFailure

badPayload :: Int -> Int -> Either CallbackError (Trainer.DQNEnvironmentStep Int)
badPayload environment _ = Right (Trainer.DQNContinuing (environment + 1) (mustObservation [1] [0, 1]) (0 / 0))

mustObservation :: [Double] -> [Int] -> Trainer.DQNTrainerObservation
mustObservation features indices =
    case mkActionMask 2 indices of
        Left problem -> error (show problem)
        Right mask ->
            case Trainer.mkDQNTrainerObservation features mask of
                Left problem -> error (show problem)
                Right observation -> observation

requireSchedule :: Int -> TargetUpdateSchedule
requireSchedule period =
    case periodicHardTargetUpdates period of
        Left problem -> error (show problem)
        Right schedule -> schedule

maxFinite :: Double
maxFinite = 1.7976931348623157e308

assert :: String -> Bool -> IO ()
assert _ True = pure ()
assert message False = failTest message

assertClose :: String -> Double -> Double -> Double -> IO ()
assertClose label tolerance expected actual =
    assert
        (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
        (abs (expected - actual) <= tolerance + tolerance * max (abs expected) (abs actual))

assertVectorClose :: String -> Double -> [Double] -> [Double] -> IO ()
assertVectorClose label tolerance expected actual = do
    assert (label ++ ": vector lengths differ") (length expected == length actual)
    sequence_
        [ assertClose (label ++ " at index " ++ show index) tolerance left right
        | (index, (left, right)) <- zip [0 :: Int ..] (zip expected actual)
        ]

requireRight :: (Show error) => String -> Either error value -> IO value
requireRight _ (Right value) = pure value
requireRight label (Left problem) = failTest (label ++ ": " ++ show problem)

failTest :: String -> IO a
failTest message = do
    putStrLn ("FAIL: " ++ message)
    exitFailure
