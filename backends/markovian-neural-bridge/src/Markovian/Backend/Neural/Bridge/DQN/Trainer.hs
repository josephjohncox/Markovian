{-# LANGUAGE RoleAnnotations #-}

{- | Bounded, resumable composition of the checked neural DQN primitives.

This bridge owns the integration with the repository's explicit generator. The
neural package remains independent of sampled/root execution packages.
-}
module Markovian.Backend.Neural.Bridge.DQN.Trainer (
    DQNTrainerConfigError (..),
    DQNTrainerConfig,
    mkDQNTrainerConfig,
    dqnTrainerDQNConfig,
    dqnTrainerExplorationRate,
    dqnTrainerBatchSize,
    dqnTrainerWarmupEntries,
    DQNTrainerFuel,
    mkDQNTrainerFuel,
    dqnTrainerFuelTransitions,
    DQNTrainerObservation,
    mkDQNTrainerObservation,
    dqnTrainerObservationFeatures,
    dqnTrainerObservationMask,
    DQNEnvironmentStep (..),
    DQNTrainerStateError (..),
    DQNTrainerState,
    mkDQNTrainerState,
    dqnTrainerStateConfig,
    dqnTrainerStateEnvironment,
    dqnTrainerStateObservation,
    dqnTrainerStateDQNState,
    dqnTrainerStateReplayBuffer,
    dqnTrainerStateGenerator,
    dqnTrainerStateAccounting,
    DQNTrainerDimension (..),
    DQNTrainerLimits,
    mkDQNTrainerLimits,
    maximumDQNTrainerTransitions,
    maximumDQNTrainerReplayAppends,
    maximumDQNTrainerBatchEntries,
    maximumDQNTrainerOnlineUpdates,
    maximumDQNTrainerCheckpointAdvances,
    maximumDQNTrainerProtocolWork,
    DQNTrainerAccounting,
    dqnTrainerAttemptedTransitions,
    dqnTrainerCommittedReplayAppends,
    dqnTrainerSelectedBatchEntries,
    dqnTrainerAttemptedOnlineUpdates,
    dqnTrainerCommittedCheckpointAdvances,
    dqnTrainerProtocolWork,
    DQNTrainerStepStatus (..),
    DQNTrainerStepReport,
    dqnTrainerStepAction,
    dqnTrainerStepReplayEntryId,
    dqnTrainerStepBatchEntryIds,
    dqnTrainerStepBatchEvaluation,
    dqnTrainerStepTerminal,
    dqnTrainerStepStatus,
    DQNTrainerError (..),
    DQNTrainerStop (..),
    DQNTrainerReport,
    dqnTrainerReportStop,
    dqnTrainerReportFuelUsed,
    dqnTrainerReportInitialAccounting,
    dqnTrainerReportFinalAccounting,
    dqnTrainerReportSteps,
    DQNTrainerRun,
    dqnTrainerRunState,
    dqnTrainerRunReport,
    runDQNTrainer,
) where

import Data.List (delete)
import Markovian.Backend.Neural.DQN (
    DQNBatchEvaluation,
    DQNConfig,
    DQNError,
    DQNState,
    dqnBatchMeanGradient,
    dqnBatchMeanHalfSquaredLoss,
    dqnBatchPredictions,
    dqnBatchTargets,
    dqnOnlineNetwork,
    dqnTargetNetwork,
    dqnUpdateEvaluation,
    dqnUpdatedState,
    updateDQNBatch,
 )
import Markovian.Backend.Neural.Dense (
    DenseError,
    DenseNetwork,
    denseForward,
    denseHiddenSizes,
    denseInputSize,
    denseOutputSize,
    denseParameters,
 )
import Markovian.Backend.Neural.Mask (
    ActionMask,
    ActionMaskError,
    actionMaskIndices,
    actionMaskWidth,
    gatherActionMask,
 )
import Markovian.Backend.Neural.Numeric (
    NeuralNumericError,
    validateFinite,
    validateFiniteVector,
 )
import Markovian.Backend.Neural.Replay (
    ReplacementMode (WithoutReplacement),
    ReplayBuffer,
    ReplayEntry,
    ReplayEntryId,
    ReplayError,
    appendReplay,
    replayCapacity,
    replayEntries,
    replayEntryId,
    replayEntryTransition,
    replaySize,
    selectReplay,
 )
import Markovian.Backend.Neural.TargetNetwork (
    targetNetworkSnapshot,
    targetSuccessfulUpdateCount,
 )
import Markovian.Backend.Neural.Transition (
    NeuralTransition,
    SuccessorSnapshot,
    TransitionError,
    foldSuccessorSnapshot,
    mkContinuingTransition,
    mkTerminalTransition,
    transitionAction,
    transitionActionMask,
    transitionFeatures,
    transitionReward,
    transitionSuccessor,
 )
import Markovian.Probability (
    DistributionError,
    finiteDist,
 )
import Markovian.Sampling (
    Generator,
    SamplingError,
    generatorState,
    sampleFiniteDist,
 )
import Numeric.Natural (Natural)

-- | Configuration construction failures.
data DQNTrainerConfigError
    = -- | Exploration mass was nonfinite or outside @[0,1]@.
      InvalidDQNTrainerExplorationRate !Double
    | -- | Batch size was not positive.
      InvalidDQNTrainerBatchSize !Int
    | -- | Warm-up entry count was not positive.
      InvalidDQNTrainerWarmupEntries !Int
    | -- | Warm-up entry count was smaller than the batch size.
      DQNTrainerWarmupBelowBatch !Int !Int
    | -- | A run segment needs at least one transition attempt.
      DQNTrainerFuelMustBePositive
    deriving (Eq, Show)

{- | Validated behavior and replay policy.

The existing 'DQNConfig' retains discount, optimizer, target rule, and target
schedule. This reference fixes replay sampling to ordered uniform draws without
replacement.
-}
data DQNTrainerConfig = DQNTrainerConfig !DQNConfig !Double !Int !Int
    deriving (Eq, Show)

-- | Construct a finite epsilon, positive batch, and sufficient warm-up policy.
mkDQNTrainerConfig ::
    DQNConfig ->
    Double ->
    Int ->
    Int ->
    Either DQNTrainerConfigError DQNTrainerConfig
mkDQNTrainerConfig dqnConfig epsilon batchSize warmup
    | not (finiteUnit epsilon) = Left (InvalidDQNTrainerExplorationRate epsilon)
    | batchSize <= 0 = Left (InvalidDQNTrainerBatchSize batchSize)
    | warmup <= 0 = Left (InvalidDQNTrainerWarmupEntries warmup)
    | warmup < batchSize = Left (DQNTrainerWarmupBelowBatch warmup batchSize)
    | otherwise = Right (DQNTrainerConfig dqnConfig epsilon batchSize warmup)

-- | The immutable existing DQN configuration.
dqnTrainerDQNConfig :: DQNTrainerConfig -> DQNConfig
dqnTrainerDQNConfig (DQNTrainerConfig config _ _ _) = config

-- | Epsilon-greedy exploration mass in @[0,1]@.
dqnTrainerExplorationRate :: DQNTrainerConfig -> Double
dqnTrainerExplorationRate (DQNTrainerConfig _ epsilon _ _) = epsilon

-- | Number of distinct replay entries selected for one update.
dqnTrainerBatchSize :: DQNTrainerConfig -> Int
dqnTrainerBatchSize (DQNTrainerConfig _ _ batchSize _) = batchSize

-- | Retained replay count required before an update is attempted.
dqnTrainerWarmupEntries :: DQNTrainerConfig -> Int
dqnTrainerWarmupEntries (DQNTrainerConfig _ _ _ warmup) = warmup

-- | Strictly positive number of transition attempts permitted in one call.
newtype DQNTrainerFuel = DQNTrainerFuel Natural
    deriving (Eq, Show)

-- | Construct positive trainer fuel.
mkDQNTrainerFuel :: Natural -> Either DQNTrainerConfigError DQNTrainerFuel
mkDQNTrainerFuel 0 = Left DQNTrainerFuelMustBePositive
mkDQNTrainerFuel value = Right (DQNTrainerFuel value)

-- | Transition attempts permitted in this invocation.
dqnTrainerFuelTransitions :: DQNTrainerFuel -> Natural
dqnTrainerFuelTransitions (DQNTrainerFuel value) = value

{- | A finite source observation and its complete ordered availability mask.

The constructor is private so all source features are finite and nonempty.
Compatibility with a particular network is checked when trainer state is built.
-}
data DQNTrainerObservation = DQNTrainerObservation ![Double] !ActionMask
    deriving (Eq, Show)

-- | Construct one finite source observation.
mkDQNTrainerObservation ::
    [Double] ->
    ActionMask ->
    Either DQNTrainerStateError DQNTrainerObservation
mkDQNTrainerObservation features mask
    | null features = Left DQNTrainerObservationFeaturesEmpty
    | otherwise = do
        mapNumericState (validateFiniteVector "DQN trainer observation features" features)
        Right (DQNTrainerObservation features mask)

-- | Source feature snapshot.
dqnTrainerObservationFeatures :: DQNTrainerObservation -> [Double]
dqnTrainerObservationFeatures (DQNTrainerObservation features _) = features

-- | Source action availability and tie order.
dqnTrainerObservationMask :: DQNTrainerObservation -> ActionMask
dqnTrainerObservationMask (DQNTrainerObservation _ mask) = mask

{- | One callback result.

A terminal payoff is explicit because 'NeuralTransition' preserves the existing
DQN terminal target @reward + discount * terminalPayoff@.
-}
data DQNEnvironmentStep environment
    = -- | A continuing successor with next environment, observation, and reward.
      DQNContinuing !environment !DQNTrainerObservation !Double
    | -- | A terminal successor with final environment, reward, and terminal payoff.
      DQNTerminal !environment !Double !Double
    deriving (Eq, Show)

type role DQNEnvironmentStep nominal

-- | State and callback-payload failures.
data DQNTrainerStateError
    = -- | Source or successor feature vectors must be nonempty.
      DQNTrainerObservationFeaturesEmpty
    | -- | Source or successor observations contain a nonfinite feature.
      DQNTrainerObservationNumericFailure !NeuralNumericError
    | -- | FIFO capacity cannot retain the configured warm-up count.
      DQNTrainerReplayCapacityBelowWarmup !Int !Int
    | -- | A pre-existing replay entry is incompatible with the online network.
      DQNTrainerReplayTransitionIncompatible !ReplayEntryId !DQNTrainerStateError
    | -- | Observation feature width differs from the online network input width.
      DQNTrainerFeatureWidthMismatch !Int !Int
    | -- | Observation mask width differs from the online network output width.
      DQNTrainerMaskWidthMismatch !Int !Int
    | -- | A callback reward was nonfinite.
      DQNTrainerRewardNumericFailure !NeuralNumericError
    | -- | A callback terminal payoff was nonfinite.
      DQNTrainerTerminalPayoffNumericFailure !NeuralNumericError
    deriving (Eq, Show)

data DQNTrainerPosition environment
    = DQNTrainerReady !environment !DQNTrainerObservation
    | DQNTrainerTerminal !environment
    deriving (Eq, Show)

type role DQNTrainerPosition nominal

{- | The complete immutable resume token.

The generic environment is evaluated to weak head normal form. Bounded numeric
trainer-owned state and report payloads, plus their owned collection spines,
are forced before a run is returned. Library error constructors and numeric
fields are forced, but diagnostic 'String' tails may be rendered later.
-}
data DQNTrainerState environment
    = DQNTrainerState
        !DQNTrainerConfig
        !(DQNTrainerPosition environment)
        !DQNState
        !ReplayBuffer
        !Generator
        !DQNTrainerAccounting
    deriving (Eq, Show)

type role DQNTrainerState nominal

-- | Construct an initially ready state with zero cumulative accounting.
mkDQNTrainerState ::
    DQNTrainerConfig ->
    environment ->
    DQNTrainerObservation ->
    DQNState ->
    ReplayBuffer ->
    Generator ->
    Either DQNTrainerStateError (DQNTrainerState environment)
mkDQNTrainerState config environment observation dqnState replay generator = do
    let capacity = replayCapacity replay
        warmup = dqnTrainerWarmupEntries config
    if capacity < warmup
        then Left (DQNTrainerReplayCapacityBelowWarmup capacity warmup)
        else do
            validateObservationForNetwork observation (dqnOnlineNetwork dqnState)
            validateReplayForNetwork replay (dqnOnlineNetwork dqnState)
    let state =
            DQNTrainerState
                config
                (DQNTrainerReady environment observation)
                dqnState
                replay
                generator
                zeroDQNTrainerAccounting
    forceDQNTrainerState state `seq` Right state

-- | Immutable configuration retained by the state.
dqnTrainerStateConfig :: DQNTrainerState environment -> DQNTrainerConfig
dqnTrainerStateConfig (DQNTrainerState config _ _ _ _ _) = config

-- | Current callback environment, including a terminal final environment.
dqnTrainerStateEnvironment :: DQNTrainerState environment -> environment
dqnTrainerStateEnvironment (DQNTrainerState _ position _ _ _ _) =
    case position of
        DQNTrainerReady environment _ -> environment
        DQNTrainerTerminal environment -> environment

-- | Current ready observation. A terminal state has no next action observation.
dqnTrainerStateObservation :: DQNTrainerState environment -> Maybe DQNTrainerObservation
dqnTrainerStateObservation (DQNTrainerState _ position _ _ _ _) =
    case position of
        DQNTrainerReady _ observation -> Just observation
        DQNTrainerTerminal _ -> Nothing

-- | Owned online and target state.
dqnTrainerStateDQNState :: DQNTrainerState environment -> DQNState
dqnTrainerStateDQNState (DQNTrainerState _ _ dqnState _ _ _) = dqnState

-- | Owned bounded FIFO replay state.
dqnTrainerStateReplayBuffer :: DQNTrainerState environment -> ReplayBuffer
dqnTrainerStateReplayBuffer (DQNTrainerState _ _ _ replay _ _) = replay

-- | Owned explicit generator state.
dqnTrainerStateGenerator :: DQNTrainerState environment -> Generator
dqnTrainerStateGenerator (DQNTrainerState _ _ _ _ generator _) = generator

-- | Persistent cumulative accounting.
dqnTrainerStateAccounting :: DQNTrainerState environment -> DQNTrainerAccounting
dqnTrainerStateAccounting (DQNTrainerState _ _ _ _ _ accounting) = accounting

-- | Independent cumulative resource dimensions, checked in constructor order.
data DQNTrainerDimension
    = -- | Callback transition attempts.
      DQNTrainerTransitions
    | -- | FIFO replay appends.
      DQNTrainerReplayAppends
    | -- | Ordered replay draws for update batches.
      DQNTrainerBatchEntries
    | -- | Checked online-update attempts.
      DQNTrainerOnlineUpdates
    | -- | Successful target-network schedule advances.
      DQNTrainerCheckpointAdvances
    | -- | Reference trainer control-flow units.
      DQNTrainerProtocolWork
    deriving (Eq, Show)

-- | Independent cumulative caps for a trainer resume lineage.
data DQNTrainerLimits
    = DQNTrainerLimits
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
    deriving (Eq, Show)

-- | Construct independent cumulative limits in dimension order.
mkDQNTrainerLimits ::
    Natural ->
    Natural ->
    Natural ->
    Natural ->
    Natural ->
    Natural ->
    DQNTrainerLimits
mkDQNTrainerLimits = DQNTrainerLimits

-- | Cumulative cap on callback transition attempts.
maximumDQNTrainerTransitions :: DQNTrainerLimits -> Natural
maximumDQNTrainerTransitions (DQNTrainerLimits value _ _ _ _ _) = value

-- | Cumulative cap on successfully appended replay entries.
maximumDQNTrainerReplayAppends :: DQNTrainerLimits -> Natural
maximumDQNTrainerReplayAppends (DQNTrainerLimits _ value _ _ _ _) = value

-- | Cumulative cap on replay entries selected into successful or partial batches.
maximumDQNTrainerBatchEntries :: DQNTrainerLimits -> Natural
maximumDQNTrainerBatchEntries (DQNTrainerLimits _ _ value _ _ _) = value

-- | Cumulative cap on attempts to update the online network.
maximumDQNTrainerOnlineUpdates :: DQNTrainerLimits -> Natural
maximumDQNTrainerOnlineUpdates (DQNTrainerLimits _ _ _ value _ _) = value

-- | Cumulative cap on successful target-network schedule advances.
maximumDQNTrainerCheckpointAdvances :: DQNTrainerLimits -> Natural
maximumDQNTrainerCheckpointAdvances (DQNTrainerLimits _ _ _ _ value _) = value

-- | Cumulative cap on trainer control-flow work units.
maximumDQNTrainerProtocolWork :: DQNTrainerLimits -> Natural
maximumDQNTrainerProtocolWork (DQNTrainerLimits _ _ _ _ _ value) = value

-- | Committed and attempted events in one immutable resume lineage.
data DQNTrainerAccounting
    = DQNTrainerAccounting
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
        !Natural
    deriving (Eq, Show)

zeroDQNTrainerAccounting :: DQNTrainerAccounting
zeroDQNTrainerAccounting = DQNTrainerAccounting 0 0 0 0 0 0

-- | Number of callback transition attempts, including callback failures.
dqnTrainerAttemptedTransitions :: DQNTrainerAccounting -> Natural
dqnTrainerAttemptedTransitions (DQNTrainerAccounting value _ _ _ _ _) = value

-- | Number of transitions retained by the FIFO replay buffer.
dqnTrainerCommittedReplayAppends :: DQNTrainerAccounting -> Natural
dqnTrainerCommittedReplayAppends (DQNTrainerAccounting _ value _ _ _ _) = value

-- | Number of replay entries successfully drawn for batch selection.
dqnTrainerSelectedBatchEntries :: DQNTrainerAccounting -> Natural
dqnTrainerSelectedBatchEntries (DQNTrainerAccounting _ _ value _ _ _) = value

-- | Number of calls made to the checked DQN batch-update primitive.
dqnTrainerAttemptedOnlineUpdates :: DQNTrainerAccounting -> Natural
dqnTrainerAttemptedOnlineUpdates (DQNTrainerAccounting _ _ _ value _ _) = value

-- | Number of successful DQN updates whose target schedules were advanced.
dqnTrainerCommittedCheckpointAdvances :: DQNTrainerAccounting -> Natural
dqnTrainerCommittedCheckpointAdvances (DQNTrainerAccounting _ _ _ _ value _) = value

-- | Number of charged trainer control-flow units.
dqnTrainerProtocolWork :: DQNTrainerAccounting -> Natural
dqnTrainerProtocolWork (DQNTrainerAccounting _ _ _ _ _ value) = value

-- | The terminal event reached by one semantic step report.
data DQNTrainerStepStatus
    = -- | A transition appended before warm-up made updates eligible.
      DQNTrainerReplayWarming
    | -- | Batch selection and the checked update both succeeded.
      DQNTrainerUpdated
    | -- | The callback returned its error after behavior sampling.
      DQNTrainerCallbackFailed
    | -- | Callback output failed finite or shape validation.
      DQNTrainerCallbackPayloadFailed
    | -- | Replay batch selection failed after any retained draws.
      DQNTrainerBatchSelectionFailed
    | -- | The checked DQN batch update failed after replay append.
      DQNTrainerUpdateFailed
    deriving (Eq, Show)

-- | Bounded semantic evidence for one action-selection attempt.
data DQNTrainerStepReport
    = DQNTrainerStepReport
        !Int
        !(Maybe ReplayEntryId)
        ![ReplayEntryId]
        !(Maybe DQNBatchEvaluation)
        !Bool
        !DQNTrainerStepStatus
    deriving (Eq, Show)

-- | Chosen action index in the source observation's mask layout.
dqnTrainerStepAction :: DQNTrainerStepReport -> Int
dqnTrainerStepAction (DQNTrainerStepReport value _ _ _ _ _) = value

-- | FIFO-stable replay identifier appended by this step, if callback payload validation passed.
dqnTrainerStepReplayEntryId :: DQNTrainerStepReport -> Maybe ReplayEntryId
dqnTrainerStepReplayEntryId (DQNTrainerStepReport _ value _ _ _ _) = value

-- | Ordered replay identifiers drawn before the terminal step status.
dqnTrainerStepBatchEntryIds :: DQNTrainerStepReport -> [ReplayEntryId]
dqnTrainerStepBatchEntryIds (DQNTrainerStepReport _ _ value _ _ _) = value

-- | Checked DQN batch evidence, present only after a successful update.
dqnTrainerStepBatchEvaluation :: DQNTrainerStepReport -> Maybe DQNBatchEvaluation
dqnTrainerStepBatchEvaluation (DQNTrainerStepReport _ _ _ value _ _) = value

-- | Whether the callback committed an explicit terminal transition.
dqnTrainerStepTerminal :: DQNTrainerStepReport -> Bool
dqnTrainerStepTerminal (DQNTrainerStepReport _ _ _ _ value _) = value

-- | Final status after all events attempted for this semantic step.
dqnTrainerStepStatus :: DQNTrainerStepReport -> DQNTrainerStepStatus
dqnTrainerStepStatus (DQNTrainerStepReport _ _ _ _ _ value) = value

-- | Failures are returned with the last committed state and semantic receipt.
data DQNTrainerError callbackError
    = -- | A proposed event would exceed one cumulative cap.
      DQNTrainerLimitExceeded !DQNTrainerDimension !Natural !Natural
    | -- | Online-network behavior evaluation failed.
      DQNTrainerActionDenseFailure !DenseError
    | -- | The source action mask could not gather online values.
      DQNTrainerActionMaskFailure !ActionMaskError
    | -- | Behavior or replay selection distribution construction failed.
      DQNTrainerActionDistributionFailure !DistributionError
    | -- | Positive exploration was too small to represent across active actions.
      DQNTrainerExplorationMassUnderflow !Double !Int
    | -- | Behavior or replay sampling failed before a new draw was produced.
      DQNTrainerSamplingFailure !SamplingError
    | -- | User callback error after the behavior-generator advance.
      DQNTrainerCallbackFailure !callbackError
    | -- | User callback output failed trainer observation or numeric checks.
      DQNTrainerSuccessorStateFailure !DQNTrainerStateError
    | -- | Valid callback output failed neural-transition construction.
      DQNTrainerTransitionFailure !TransitionError
    | -- | A selected replay identifier could not be resolved.
      DQNTrainerReplayFailure !ReplayError
    | -- | Checked batch target, gradient, optimizer, or target update failed.
      DQNTrainerUpdateFailure !DQNError
    deriving (Eq, Show)

type role DQNTrainerError nominal

-- | Normal stop or an explicit failure.
data DQNTrainerStop callbackError
    = -- | The supplied positive fuel was exactly consumed.
      DQNTrainerFuelExhausted
    | -- | A committed terminal transition ended the segment.
      DQNTrainerReachedTerminal
    | -- | A structured trainer failure stopped the segment.
      DQNTrainerFailed !(DQNTrainerError callbackError)
    deriving (Eq, Show)

type role DQNTrainerStop nominal

{- | One bounded semantic receipt.

Its numeric payloads and owned collection spines are forced before publication.
Library error constructors and numeric fields are forced, while diagnostic
'String' tails may be rendered later.
-}
data DQNTrainerReport callbackError
    = DQNTrainerReport
        !(DQNTrainerStop callbackError)
        !Natural
        !DQNTrainerAccounting
        !DQNTrainerAccounting
        ![DQNTrainerStepReport]
    deriving (Eq, Show)

type role DQNTrainerReport nominal

-- | Stop condition for this invocation.
dqnTrainerReportStop :: DQNTrainerReport callbackError -> DQNTrainerStop callbackError
dqnTrainerReportStop (DQNTrainerReport value _ _ _ _) = value

-- | Number of callback attempts consumed before the stop condition.
dqnTrainerReportFuelUsed :: DQNTrainerReport callbackError -> Natural
dqnTrainerReportFuelUsed (DQNTrainerReport _ value _ _ _) = value

-- | Lineage accounting supplied by the input state.
dqnTrainerReportInitialAccounting :: DQNTrainerReport callbackError -> DQNTrainerAccounting
dqnTrainerReportInitialAccounting (DQNTrainerReport _ _ value _ _) = value

-- | Lineage accounting after the final committed event.
dqnTrainerReportFinalAccounting :: DQNTrainerReport callbackError -> DQNTrainerAccounting
dqnTrainerReportFinalAccounting (DQNTrainerReport _ _ _ value _) = value

-- | Chronological bounded semantic step receipts.
dqnTrainerReportSteps :: DQNTrainerReport callbackError -> [DQNTrainerStepReport]
dqnTrainerReportSteps (DQNTrainerReport _ _ _ _ value) = value

-- | The returned state is always the state named by the accompanying report.
data DQNTrainerRun environment callbackError
    = DQNTrainerRun
        !(DQNTrainerState environment)
        !(DQNTrainerReport callbackError)
    deriving (Eq, Show)

type role DQNTrainerRun nominal nominal

-- | Resume token after the final committed event.
dqnTrainerRunState :: DQNTrainerRun environment callbackError -> DQNTrainerState environment
dqnTrainerRunState (DQNTrainerRun state _) = state

-- | Semantic receipt paired with 'dqnTrainerRunState'.
dqnTrainerRunReport :: DQNTrainerRun environment callbackError -> DQNTrainerReport callbackError
dqnTrainerRunReport (DQNTrainerRun _ report) = report

{- | Run one positive-fuel segment.

A complete next-step limit plan is admitted before behavior selection or the
callback. Failures retain the state committed through their last completed
event, including any generator draw made before that failure.
-}
runDQNTrainer ::
    DQNTrainerFuel ->
    DQNTrainerLimits ->
    (environment -> Int -> Either callbackError (DQNEnvironmentStep environment)) ->
    DQNTrainerState environment ->
    DQNTrainerRun environment callbackError
runDQNTrainer fuel limits callback initial =
    go (dqnTrainerFuelTransitions fuel) 0 initial []
  where
    initialAccounting = dqnTrainerStateAccounting initial

    go remaining used state reports
        | remaining == 0 = finish DQNTrainerFuelExhausted used state reports
        | otherwise =
            case trainerPosition state of
                DQNTrainerTerminal _ -> finish DQNTrainerReachedTerminal used state reports
                DQNTrainerReady environment observation ->
                    case preflight limits state of
                        Left problem -> finish (DQNTrainerFailed problem) used state reports
                        Right plan ->
                            case chooseBehaviorAction state observation of
                                Left problem -> finish (DQNTrainerFailed problem) used state reports
                                Right (action, afterBehaviorGenerator) ->
                                    let afterBehavior =
                                            replaceGeneratorAndAccounting
                                                afterBehaviorGenerator
                                                (addAccounting 1 0 0 0 0 1 (dqnTrainerStateAccounting state))
                                                state
                                        attempted = DQNTrainerStepReport action Nothing [] Nothing False DQNTrainerCallbackFailed
                                     in case callback environment action of
                                            Left callbackProblem ->
                                                finish
                                                    (DQNTrainerFailed (DQNTrainerCallbackFailure callbackProblem))
                                                    (used + 1)
                                                    afterBehavior
                                                    (setStepStatus DQNTrainerCallbackFailed attempted : reports)
                                            Right outcome ->
                                                completeCallback
                                                    plan
                                                    remaining
                                                    (used + 1)
                                                    action
                                                    outcome
                                                    afterBehavior
                                                    attempted
                                                    reports

    completeCallback plan remaining used action outcome afterBehavior attempted reports =
        case prepareTransition afterBehavior action outcome of
            Left problem ->
                finish
                    (DQNTrainerFailed problem)
                    used
                    afterBehavior
                    (setStepStatus DQNTrainerCallbackPayloadFailed attempted : reports)
            Right (transition, nextPosition, terminal) ->
                let (entryId, replay) = appendReplay transition (dqnTrainerStateReplayBuffer afterBehavior)
                    afterReplay =
                        replacePositionReplayAccounting
                            nextPosition
                            replay
                            (addAccounting 0 1 0 0 0 1 (dqnTrainerStateAccounting afterBehavior))
                            afterBehavior
                    appended = DQNTrainerStepReport action (Just entryId) [] Nothing terminal DQNTrainerReplayWarming
                 in if stepPlanUpdates plan
                        then completeUpdate remaining used afterReplay appended reports
                        else
                            let nextReports = setStepStatus DQNTrainerReplayWarming appended : reports
                             in if terminal
                                    then finish DQNTrainerReachedTerminal used afterReplay nextReports
                                    else go (remaining - 1) used afterReplay nextReports

    completeUpdate remaining used afterReplay appended reports =
        case sampleOrderedReplayIds
            (dqnTrainerBatchSize (dqnTrainerStateConfig afterReplay))
            (dqnTrainerStateGenerator afterReplay)
            (dqnTrainerStateReplayBuffer afterReplay) of
            DQNTrainerBatchDrawFailed problem entryIds afterBatchGenerator ->
                let selectedCount = fromIntegral (length entryIds)
                    afterPartialBatch =
                        replaceGeneratorAndAccounting
                            afterBatchGenerator
                            (addAccounting 0 0 selectedCount 0 0 selectedCount (dqnTrainerStateAccounting afterReplay))
                            afterReplay
                    partialReport = setBatchIds entryIds appended
                 in finish
                        (DQNTrainerFailed problem)
                        used
                        afterPartialBatch
                        (setStepStatus DQNTrainerBatchSelectionFailed partialReport : reports)
            DQNTrainerBatchDrawSucceeded entryIds afterBatchGenerator ->
                let batchCount = fromIntegral (length entryIds)
                    afterBatch =
                        replaceGeneratorAndAccounting
                            afterBatchGenerator
                            (addAccounting 0 0 batchCount 0 0 batchCount (dqnTrainerStateAccounting afterReplay))
                            afterReplay
                    selectedReport = setBatchIds entryIds appended
                 in case selectReplay WithoutReplacement entryIds (dqnTrainerStateReplayBuffer afterBatch) of
                        Left replayProblem ->
                            finish
                                (DQNTrainerFailed (DQNTrainerReplayFailure replayProblem))
                                used
                                afterBatch
                                (setStepStatus DQNTrainerBatchSelectionFailed selectedReport : reports)
                        Right selectedEntries ->
                            let transitions = fmap replayEntryTransition selectedEntries
                                afterAttempt =
                                    replaceAccounting
                                        (addAccounting 0 0 0 1 0 1 (dqnTrainerStateAccounting afterBatch))
                                        afterBatch
                             in case updateDQNBatch (dqnTrainerDQNConfig (dqnTrainerStateConfig afterAttempt)) (dqnTrainerStateDQNState afterAttempt) transitions of
                                    Left updateProblem ->
                                        finish
                                            (DQNTrainerFailed (DQNTrainerUpdateFailure updateProblem))
                                            used
                                            afterAttempt
                                            (setStepStatus DQNTrainerUpdateFailed selectedReport : reports)
                                    Right update ->
                                        let afterUpdate =
                                                replaceDQNAccounting
                                                    (dqnUpdatedState update)
                                                    (addAccounting 0 0 0 0 1 1 (dqnTrainerStateAccounting afterAttempt))
                                                    afterAttempt
                                            completed =
                                                setStepStatus
                                                    DQNTrainerUpdated
                                                    (setBatchEvaluation (dqnUpdateEvaluation update) selectedReport)
                                            nextReports = completed : reports
                                         in if dqnTrainerStepTerminal completed
                                                then finish DQNTrainerReachedTerminal used afterUpdate nextReports
                                                else go (remaining - 1) used afterUpdate nextReports

    finish stop used state reversedReports =
        forceDQNTrainerRun
            (DQNTrainerRun state (DQNTrainerReport stop used initialAccounting (dqnTrainerStateAccounting state) (reverse reversedReports)))

data DQNTrainerStepPlan = DQNTrainerStepPlan !Bool
{-# ANN DQNTrainerStepPlan ("HLint: ignore Use newtype instead of data" :: String) #-}

stepPlanUpdates :: DQNTrainerStepPlan -> Bool
stepPlanUpdates (DQNTrainerStepPlan updates) = updates

preflight ::
    DQNTrainerLimits ->
    DQNTrainerState environment ->
    Either (DQNTrainerError callbackError) DQNTrainerStepPlan
preflight limits state = do
    let config = dqnTrainerStateConfig state
        replay = dqnTrainerStateReplayBuffer state
        afterAppendSize = nextReplaySize replay
        updates = afterAppendSize >= dqnTrainerWarmupEntries config
        batchCount = if updates then fromIntegral (dqnTrainerBatchSize config) else 0
        updateCount = if updates then 1 else 0
        checkpointCount = updateCount
        work = if updates then batchCount + 4 else 2
        plan = DQNTrainerStepPlan updates
        accounting = dqnTrainerStateAccounting state
    requireWithin
        DQNTrainerTransitions
        (maximumDQNTrainerTransitions limits)
        (dqnTrainerAttemptedTransitions accounting)
        1
    requireWithin
        DQNTrainerReplayAppends
        (maximumDQNTrainerReplayAppends limits)
        (dqnTrainerCommittedReplayAppends accounting)
        1
    requireWithin
        DQNTrainerBatchEntries
        (maximumDQNTrainerBatchEntries limits)
        (dqnTrainerSelectedBatchEntries accounting)
        batchCount
    requireWithin
        DQNTrainerOnlineUpdates
        (maximumDQNTrainerOnlineUpdates limits)
        (dqnTrainerAttemptedOnlineUpdates accounting)
        updateCount
    requireWithin
        DQNTrainerCheckpointAdvances
        (maximumDQNTrainerCheckpointAdvances limits)
        (dqnTrainerCommittedCheckpointAdvances accounting)
        checkpointCount
    requireWithin
        DQNTrainerProtocolWork
        (maximumDQNTrainerProtocolWork limits)
        (dqnTrainerProtocolWork accounting)
        work
    Right plan

requireWithin ::
    DQNTrainerDimension ->
    Natural ->
    Natural ->
    Natural ->
    Either (DQNTrainerError callbackError) ()
requireWithin dimension limit used additional
    | used + additional <= limit = Right ()
    | otherwise = Left (DQNTrainerLimitExceeded dimension limit (limit + 1))

chooseBehaviorAction ::
    DQNTrainerState environment ->
    DQNTrainerObservation ->
    Either (DQNTrainerError callbackError) (Int, Generator)
chooseBehaviorAction state observation = do
    values <-
        either
            (Left . DQNTrainerActionDenseFailure)
            Right
            (denseForward (dqnOnlineNetwork (dqnTrainerStateDQNState state)) (dqnTrainerObservationFeatures observation))
    masked <-
        either
            (Left . DQNTrainerActionMaskFailure)
            Right
            (gatherActionMask (dqnTrainerObservationMask observation) values)
    let indices = actionMaskIndices (dqnTrainerObservationMask observation)
        epsilon = dqnTrainerExplorationRate (dqnTrainerStateConfig state)
        activeCount = length indices
        exploration = epsilon / fromIntegral activeCount
    if epsilon > 0 && activeCount > 1 && exploration == 0
        then Left (DQNTrainerExplorationMassUnderflow epsilon activeCount)
        else Right ()
    let greedy = fst (maskedArgmax (zip indices masked))
        weights =
            [ (index, exploration + if index == greedy then 1 - epsilon else 0)
            | index <- indices
            ]
    distribution <-
        either
            (Left . DQNTrainerActionDistributionFailure)
            Right
            (finiteDist weights)
    either
        (Left . DQNTrainerSamplingFailure)
        Right
        (sampleFiniteDist (dqnTrainerStateGenerator state) distribution)

maskedArgmax :: [(Int, Double)] -> (Int, Double)
maskedArgmax [] = error "validated action mask unexpectedly empty"
maskedArgmax (first : remaining) = foldl choose first remaining
  where
    choose best@(_, bestValue) candidate@(_, candidateValue)
        | candidateValue > bestValue = candidate
        | otherwise = best

data DQNTrainerBatchDraw callbackError
    = DQNTrainerBatchDrawSucceeded ![ReplayEntryId] !Generator
    | DQNTrainerBatchDrawFailed !(DQNTrainerError callbackError) ![ReplayEntryId] !Generator

sampleOrderedReplayIds ::
    Int ->
    Generator ->
    ReplayBuffer ->
    DQNTrainerBatchDraw callbackError
sampleOrderedReplayIds count generator replay = go count (fmap replayEntryId (replayEntries replay)) generator []
  where
    go remaining available current reversed
        | remaining <= 0 = DQNTrainerBatchDrawSucceeded (reverse reversed) current
        | otherwise =
            case finiteDist [(entryId, 1) | entryId <- available] of
                Left problem ->
                    DQNTrainerBatchDrawFailed
                        (DQNTrainerActionDistributionFailure problem)
                        (reverse reversed)
                        current
                Right distribution ->
                    case sampleFiniteDist current distribution of
                        Left problem ->
                            DQNTrainerBatchDrawFailed
                                (DQNTrainerSamplingFailure problem)
                                (reverse reversed)
                                current
                        Right (selected, next) ->
                            go (remaining - 1) (delete selected available) next (selected : reversed)

prepareTransition ::
    DQNTrainerState environment ->
    Int ->
    DQNEnvironmentStep environment ->
    Either (DQNTrainerError callbackError) (NeuralTransition, DQNTrainerPosition environment, Bool)
prepareTransition state action outcome =
    case outcome of
        DQNContinuing environment successor reward -> do
            validateSuccessor successor
            mapReward (validateFinite "DQN trainer continuing reward" reward)
            transition <-
                either
                    (Left . DQNTrainerTransitionFailure)
                    Right
                    ( mkContinuingTransition
                        sourceFeatures
                        sourceMask
                        action
                        reward
                        (dqnTrainerObservationFeatures successor)
                        (dqnTrainerObservationMask successor)
                    )
            Right (transition, DQNTrainerReady environment successor, False)
        DQNTerminal environment reward terminalPayoff -> do
            mapReward (validateFinite "DQN trainer terminal reward" reward)
            either
                (Left . DQNTrainerSuccessorStateFailure . DQNTrainerTerminalPayoffNumericFailure)
                Right
                (validateFinite "DQN trainer terminal payoff" terminalPayoff)
            transition <-
                either
                    (Left . DQNTrainerTransitionFailure)
                    Right
                    (mkTerminalTransition sourceFeatures sourceMask action reward terminalPayoff)
            Right (transition, DQNTrainerTerminal environment, True)
  where
    sourceObservation =
        case trainerPosition state of
            DQNTrainerReady _ observation -> observation
            DQNTrainerTerminal _ -> error "terminal state was checked before callback"
    sourceFeatures = dqnTrainerObservationFeatures sourceObservation
    sourceMask = dqnTrainerObservationMask sourceObservation
    validateSuccessor successor =
        either
            (Left . DQNTrainerSuccessorStateFailure)
            Right
            (validateObservationForNetwork successor (dqnOnlineNetwork (dqnTrainerStateDQNState state)))

validateObservationForNetwork ::
    DQNTrainerObservation ->
    DenseNetwork ->
    Either DQNTrainerStateError ()
validateObservationForNetwork observation network
    | featureCount /= denseInputSize network =
        Left (DQNTrainerFeatureWidthMismatch (denseInputSize network) featureCount)
    | actionMaskWidth mask /= denseOutputSize network =
        Left (DQNTrainerMaskWidthMismatch (denseOutputSize network) (actionMaskWidth mask))
    | otherwise = Right ()
  where
    features = dqnTrainerObservationFeatures observation
    featureCount = length features
    mask = dqnTrainerObservationMask observation

validateReplayForNetwork ::
    ReplayBuffer ->
    DenseNetwork ->
    Either DQNTrainerStateError ()
validateReplayForNetwork replay network = traverseEntries (replayEntries replay)
  where
    traverseEntries [] = Right ()
    traverseEntries (entry : remaining) = do
        either
            (Left . DQNTrainerReplayTransitionIncompatible (replayEntryId entry))
            Right
            (validateTransition (replayEntryTransition entry))
        traverseEntries remaining

    validateTransition transition = do
        source <- mkDQNTrainerObservation (transitionFeatures transition) (transitionActionMask transition)
        validateObservationForNetwork source network
        foldSuccessorSnapshot validateTerminal validateContinuing (transitionSuccessor transition)

    validateTerminal payoff =
        either
            (Left . DQNTrainerTerminalPayoffNumericFailure)
            Right
            (validateFinite "DQN trainer replay terminal payoff" payoff)

    validateContinuing features mask = do
        successor <- mkDQNTrainerObservation features mask
        validateObservationForNetwork successor network

trainerPosition :: DQNTrainerState environment -> DQNTrainerPosition environment
trainerPosition (DQNTrainerState _ position _ _ _ _) = position

replaceGeneratorAndAccounting ::
    Generator ->
    DQNTrainerAccounting ->
    DQNTrainerState environment ->
    DQNTrainerState environment
replaceGeneratorAndAccounting generator accounting (DQNTrainerState config position dqnState replay _ _) =
    DQNTrainerState config position dqnState replay generator accounting

replacePositionReplayAccounting ::
    DQNTrainerPosition environment ->
    ReplayBuffer ->
    DQNTrainerAccounting ->
    DQNTrainerState environment ->
    DQNTrainerState environment
replacePositionReplayAccounting position replay accounting (DQNTrainerState config _ dqnState _ generator _) =
    DQNTrainerState config position dqnState replay generator accounting

replaceAccounting ::
    DQNTrainerAccounting ->
    DQNTrainerState environment ->
    DQNTrainerState environment
replaceAccounting accounting (DQNTrainerState config position dqnState replay generator _) =
    DQNTrainerState config position dqnState replay generator accounting

replaceDQNAccounting ::
    DQNState ->
    DQNTrainerAccounting ->
    DQNTrainerState environment ->
    DQNTrainerState environment
replaceDQNAccounting dqnState accounting (DQNTrainerState config position _ replay generator _) =
    DQNTrainerState config position dqnState replay generator accounting

addAccounting ::
    Natural ->
    Natural ->
    Natural ->
    Natural ->
    Natural ->
    Natural ->
    DQNTrainerAccounting ->
    DQNTrainerAccounting
addAccounting
    transitions
    appends
    batchEntries
    updates
    checkpoints
    work
    (DQNTrainerAccounting oldTransitions oldAppends oldBatchEntries oldUpdates oldCheckpoints oldWork) =
        DQNTrainerAccounting
            (oldTransitions + transitions)
            (oldAppends + appends)
            (oldBatchEntries + batchEntries)
            (oldUpdates + updates)
            (oldCheckpoints + checkpoints)
            (oldWork + work)

nextReplaySize :: ReplayBuffer -> Int
nextReplaySize replay
    | size < capacity = size + 1
    | otherwise = capacity
  where
    size = replaySize replay
    capacity = replayCapacity replay

setBatchIds :: [ReplayEntryId] -> DQNTrainerStepReport -> DQNTrainerStepReport
setBatchIds entryIds (DQNTrainerStepReport action entryId _ evaluation terminal status) =
    DQNTrainerStepReport action entryId entryIds evaluation terminal status

setBatchEvaluation :: DQNBatchEvaluation -> DQNTrainerStepReport -> DQNTrainerStepReport
setBatchEvaluation evaluation (DQNTrainerStepReport action entryId entryIds _ terminal status) =
    DQNTrainerStepReport action entryId entryIds (Just evaluation) terminal status

setStepStatus :: DQNTrainerStepStatus -> DQNTrainerStepReport -> DQNTrainerStepReport
setStepStatus status (DQNTrainerStepReport action entryId entryIds evaluation terminal _) =
    DQNTrainerStepReport action entryId entryIds evaluation terminal status

mapNumericState :: Either NeuralNumericError value -> Either DQNTrainerStateError value
mapNumericState = either (Left . DQNTrainerObservationNumericFailure) Right

mapReward :: Either NeuralNumericError value -> Either (DQNTrainerError callbackError) value
mapReward = either (Left . DQNTrainerSuccessorStateFailure . DQNTrainerRewardNumericFailure) Right

finiteUnit :: Double -> Bool
finiteUnit value = not (isNaN value || isInfinite value) && value >= 0 && value <= 1

forceDQNTrainerRun :: DQNTrainerRun environment callbackError -> DQNTrainerRun environment callbackError
forceDQNTrainerRun run@(DQNTrainerRun state report) =
    forceDQNTrainerState state `seq` forceDQNTrainerReport report `seq` run

forceDQNTrainerState :: DQNTrainerState environment -> DQNTrainerState environment
forceDQNTrainerState state@(DQNTrainerState config position dqnState replay generator accounting) =
    forceDQNTrainerConfig config `seq`
        forceDQNTrainerPosition position `seq`
            forceDQNState dqnState `seq`
                forceReplayBuffer replay `seq`
                    generatorState generator `seq`
                        forceDQNTrainerAccounting accounting `seq`
                            state

forceDQNTrainerConfig :: DQNTrainerConfig -> ()
forceDQNTrainerConfig (DQNTrainerConfig dqnConfig epsilon batchSize warmup) =
    dqnConfig `seq` epsilon `seq` batchSize `seq` warmup `seq` ()

forceDQNTrainerPosition :: DQNTrainerPosition environment -> ()
forceDQNTrainerPosition position =
    case position of
        DQNTrainerReady environment observation -> environment `seq` forceDQNTrainerObservation observation
        DQNTrainerTerminal environment -> environment `seq` ()

forceDQNTrainerObservation :: DQNTrainerObservation -> ()
forceDQNTrainerObservation (DQNTrainerObservation features mask) =
    forceDoubleList features `seq`
        actionMaskWidth mask `seq`
            forceIntList (actionMaskIndices mask)

forceDQNState :: DQNState -> ()
forceDQNState dqnState =
    forceDenseNetwork (dqnOnlineNetwork dqnState) `seq`
        forceDenseNetwork (targetNetworkSnapshot target) `seq`
            targetSuccessfulUpdateCount target `seq`
                ()
  where
    target = dqnTargetNetwork dqnState

forceDenseNetwork :: DenseNetwork -> ()
forceDenseNetwork network =
    denseInputSize network `seq`
        forceIntList (denseHiddenSizes network) `seq`
            denseOutputSize network `seq`
                forceDoubleList (denseParameters network)

forceReplayBuffer :: ReplayBuffer -> ()
forceReplayBuffer replay =
    replayCapacity replay `seq`
        replaySize replay `seq`
            forceReplayEntries (replayEntries replay)

forceReplayEntries :: [ReplayEntry] -> ()
forceReplayEntries [] = ()
forceReplayEntries (entry : remaining) =
    replayEntryId entry `seq`
        forceNeuralTransition (replayEntryTransition entry) `seq`
            forceReplayEntries remaining

forceNeuralTransition :: NeuralTransition -> ()
forceNeuralTransition transition =
    forceDoubleList (transitionFeatures transition) `seq`
        forceActionMask (transitionActionMask transition) `seq`
            transitionAction transition `seq`
                transitionReward transition `seq`
                    forceSuccessorSnapshot (transitionSuccessor transition)

forceSuccessorSnapshot :: SuccessorSnapshot -> ()
forceSuccessorSnapshot =
    foldSuccessorSnapshot
        (`seq` ())
        (\features mask -> forceDoubleList features `seq` forceActionMask mask)

forceActionMask :: ActionMask -> ()
forceActionMask mask = actionMaskWidth mask `seq` forceIntList (actionMaskIndices mask)

forceDQNTrainerAccounting :: DQNTrainerAccounting -> ()
forceDQNTrainerAccounting (DQNTrainerAccounting transitions appends batch updates checkpoints work) =
    transitions `seq`
        appends `seq`
            batch `seq`
                updates `seq`
                    checkpoints `seq`
                        work `seq`
                            ()

forceDQNTrainerReport :: DQNTrainerReport callbackError -> ()
forceDQNTrainerReport (DQNTrainerReport stop fuel initial final steps) =
    stop `seq`
        fuel `seq`
            forceDQNTrainerAccounting initial `seq`
                forceDQNTrainerAccounting final `seq`
                    forceStepReports steps

forceStepReports :: [DQNTrainerStepReport] -> ()
forceStepReports = foldr (seq . forceDQNTrainerStepReport) ()

forceDQNTrainerStepReport :: DQNTrainerStepReport -> ()
forceDQNTrainerStepReport (DQNTrainerStepReport action entryId entryIds evaluation terminal status) =
    action `seq`
        forceMaybeEntryId entryId `seq`
            forceEntryIds entryIds `seq`
                forceMaybeEvaluation evaluation `seq`
                    terminal `seq`
                        status `seq`
                            ()

forceMaybeEntryId :: Maybe ReplayEntryId -> ()
forceMaybeEntryId Nothing = ()
forceMaybeEntryId (Just entryId) = entryId `seq` ()

forceEntryIds :: [ReplayEntryId] -> ()
forceEntryIds = foldr seq ()

forceMaybeEvaluation :: Maybe DQNBatchEvaluation -> ()
forceMaybeEvaluation Nothing = ()
forceMaybeEvaluation (Just evaluation) =
    forceDoubleList (dqnBatchTargets evaluation) `seq`
        forceDoubleList (dqnBatchPredictions evaluation) `seq`
            dqnBatchMeanHalfSquaredLoss evaluation `seq`
                forceDoubleList (dqnBatchMeanGradient evaluation)

forceDoubleList :: [Double] -> ()
forceDoubleList = foldr seq ()

forceIntList :: [Int] -> ()
forceIntList = foldr seq ()
