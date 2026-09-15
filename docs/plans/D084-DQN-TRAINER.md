# D084: bounded resumable reference DQN trainer contract

## 1. Scope

D084 adds `Markovian.Backend.Neural.Bridge.DQN.Trainer`: a pure, bounded
integration reference around the existing `Replay`, `TargetNetwork`, and `DQN`
batch APIs. It lives in `markovian-neural-bridge`, which already owns the
root/neural integration edge and can therefore reuse the canonical explicit
generator. It supports the existing `StandardDQN` and `DoubleDQN` target
selection without changing either batch semantics.

It is not an environment framework, production trainer, convergence result,
device implementation, distributed replay, asynchronous actor, hidden thread,
or global-randomness API. The supplied environment step is a pure callback;
external effects and rollback of an external environment are outside this
contract.

## 2. Public shape

The module exposes opaque validated configuration, positive per-call fuel,
state, limits, accounting, reports, and errors. The entry points are:

```haskell
mkDQNTrainerConfig
  :: DQNConfig -> Double -> Int -> Int
  -> Either DQNTrainerConfigError DQNTrainerConfig

mkDQNTrainerFuel
  :: Natural -> Either DQNTrainerConfigError DQNTrainerFuel

mkDQNTrainerState
  :: DQNTrainerConfig -> environment -> DQNTrainerObservation
  -> DQNState -> ReplayBuffer -> Generator
  -> Either DQNTrainerStateError (DQNTrainerState environment)

runDQNTrainer
  :: DQNTrainerFuel -> DQNTrainerLimits
  -> (environment -> Int -> Either callbackError (DQNEnvironmentStep environment))
  -> DQNTrainerState environment
  -> DQNTrainerRun environment callbackError
```

The frozen public sum constructors are:

```haskell
DQNEnvironmentStep environment
  = DQNContinuing environment DQNTrainerObservation Double
  | DQNTerminal environment Double Double

DQNTrainerConfigError
  = InvalidDQNTrainerExplorationRate Double
  | InvalidDQNTrainerBatchSize Int
  | InvalidDQNTrainerWarmupEntries Int
  | DQNTrainerWarmupBelowBatch Int Int
  | DQNTrainerFuelMustBePositive

DQNTrainerStateError
  = DQNTrainerObservationFeaturesEmpty
  | DQNTrainerObservationNumericFailure NeuralNumericError
  | DQNTrainerReplayCapacityBelowWarmup Int Int
  | DQNTrainerReplayTransitionIncompatible ReplayEntryId DQNTrainerStateError
  | DQNTrainerFeatureWidthMismatch Int Int
  | DQNTrainerMaskWidthMismatch Int Int
  | DQNTrainerRewardNumericFailure NeuralNumericError
  | DQNTrainerTerminalPayoffNumericFailure NeuralNumericError

DQNTrainerDimension
  = DQNTrainerTransitions | DQNTrainerReplayAppends
  | DQNTrainerBatchEntries | DQNTrainerOnlineUpdates
  | DQNTrainerCheckpointAdvances | DQNTrainerProtocolWork

DQNTrainerStepStatus
  = DQNTrainerReplayWarming | DQNTrainerUpdated
  | DQNTrainerCallbackFailed | DQNTrainerCallbackPayloadFailed
  | DQNTrainerBatchSelectionFailed | DQNTrainerUpdateFailed

DQNTrainerError callbackError
  = DQNTrainerLimitExceeded DQNTrainerDimension Natural Natural
  | DQNTrainerActionDenseFailure DenseError
  | DQNTrainerActionMaskFailure ActionMaskError
  | DQNTrainerActionDistributionFailure DistributionError
  | DQNTrainerExplorationMassUnderflow Double Int
  | DQNTrainerSamplingFailure SamplingError
  | DQNTrainerCallbackFailure callbackError
  | DQNTrainerSuccessorStateFailure DQNTrainerStateError
  | DQNTrainerTransitionFailure TransitionError
  | DQNTrainerReplayFailure ReplayError
  | DQNTrainerUpdateFailure DQNError

DQNTrainerStop callbackError
  = DQNTrainerFuelExhausted
  | DQNTrainerReachedTerminal
  | DQNTrainerFailed (DQNTrainerError callbackError)
```

`DQNTrainerConfig`, `DQNTrainerFuel`, `DQNTrainerObservation`,
`DQNTrainerState`, `DQNTrainerLimits`, `DQNTrainerAccounting`,
`DQNTrainerStepReport`, `DQNTrainerReport`, and `DQNTrainerRun` have private
constructors. A report is read through its stop, fuel, initial/final
accounting, and chronological-step selectors; a run is read through its state
and report selectors. Bounded numeric state and receipt payloads and owned
collection spines are forced before publication. Library error constructors and
numeric fields are forced, while diagnostic `String` tails may be rendered
later. Generic environments and callback errors are forced only to weak head
normal form; the trainer neither evaluates nor owns their interior.

`mkDQNTrainerConfig` receives, in order, the existing DQN configuration,
epsilon in `[0,1]`, a positive batch size, and a warm-up entry count. Warm-up
must be at least the batch size. State construction additionally requires the
replay capacity to be at least warm-up. The trainer always draws a distinct
ordered batch uniformly without replacement; it does not add a second replay
sampling policy.

`DQNTrainerObservation` holds one finite nonempty feature vector and one
validated nonempty `ActionMask`. `DQNEnvironmentStep` is either a continuing
successor `(environment, observation, reward)` or a terminal successor
`(environment, reward, terminalPayoff)`. The terminal payoff is explicit: a
usual no-bootstrap terminal therefore passes `0`, while the existing DQN
meaning remains `reward + discount * terminalPayoff`.

The state owns its configuration, ready environment and observation (or a
terminal environment), `DQNState`, `ReplayBuffer`, `Generator`, and cumulative
accounting. This is the complete resume token. Replay IDs, capacity, and FIFO
eviction remain solely the `ReplayBuffer`'s responsibility. Online and target
parameters and the successful target-update count remain solely the `DQNState`
and its `TargetNetwork`'s responsibility; storage identity never establishes
ownership.

## 3. Fuel, limits, and reports

Fuel is strictly positive. It bounds attempts in one call, not training
quality. Fuel exhaustion returns a ready, resumable state; it does not mean an
episode or learning objective has converged.

`DQNTrainerLimits` independently caps the cumulative ledger dimensions:

1. attempted environment transitions;
2. committed replay appends;
3. selected replay-batch entries;
4. attempted online batch updates;
5. committed target-checkpoint advances; and
6. trainer protocol work.

The ledger persists in `DQNTrainerState`, so a resumed call checks the same
cumulative totals. A preflight computes the complete next-step plan from the
current replay size and its known post-append size. It checks dimensions in the
listed order, then work, reporting `limit + 1` for the first rejected
dimension. A limit failure invokes no action selection or environment callback
and consumes no fuel.

Protocol work deliberately measures only this trainer's fixed control flow.
An attempted transition/action selection costs one unit, a committed append one,
each selected batch entry one, an attempted batch update one, and a committed
target-checkpoint advance one. It does not estimate dense-network FLOPs, heap,
or wall time. The plan is therefore two work units while replay is warming and
`batchSize + 4` when it attempts an update.

`DQNTrainerReport` is a bounded semantic receipt for one call: stop or failure
kind, fuel used, beginning and ending cumulative accounting, and ordered step
reports. A committed step report records the selected action, assigned replay
ID, ordered selected replay IDs, whether it warmed replay or attempted an
update, and the existing `DQNBatchEvaluation` when present. This receipt is
semantic evidence; it is not a timing measurement.

## 4. Frozen event order

For a ready state with fuel remaining, the trainer performs this exact order:

1. compute and admit the complete next-step ledger plan;
2. forward the current online network, validate its output width against the
   current mask, construct epsilon-greedy behavior weights in mask order, and
   select one action;
3. consume one fuel unit and record the attempted transition/action-selection
   charge before invoking the environment callback;
4. invoke the callback once with the selected global action index;
5. validate the returned reward and successor payload, construct one owned
   `NeuralTransition`, and append it to replay;
6. if the post-append replay size is below warm-up, finish the step;
7. otherwise draw one ordered distinct batch from the retained replay IDs,
   select it with `selectReplay WithoutReplacement`, and call the existing
   `updateDQNBatch` exactly once;
8. publish the resulting `DQNState` only if that call succeeds. Its existing
   `afterSuccessfulUpdate` performs the target count advance and configured
   hard/Polyak/no-op schedule after the atomic online step.

The batch evaluator still sees one complete batch from one online and one
target snapshot. A failed update leaves online parameters, target parameters,
and target successful-update count unchanged. A successful update advances the
checkpoint ledger exactly once even when its configured schedule does not copy
target parameters.

## 5. Terminal, truncation, RNG, and failure boundaries

A continuing callback result stores a `ContinuingSnapshot`; its successor
environment and observation become the next ready state. If the call exhausts
fuel after this event, that is truncation of the *run*, not a terminal
transition: the stored successor remains a continuing bootstrap and a later
call resumes from it exactly.

A terminal callback result stores a `TerminalSnapshot terminalPayoff` and
publishes a terminal state only after the same append/warm-or-update sequence.
It cannot be resumed for another action. There is no invented zero terminal
payoff and no terminal payoff introduced at a fuel cutoff.

Behavior selection uses the repository's explicit `Markovian.Sampling.Generator`
and its exact binary-rational finite-distribution sampler. The behavior draw
comes first. Once replay is warm, batch positions are drawn sequentially from
the remaining FIFO IDs, preserving draw order; a singleton choice consumes no
generator state under the existing sampler. Replay append, DQN evaluation,
SGD, and target synchronization make no random draw. Thus a normal split run
is equal to one run when the complete returned state, including generator and
ledger, is reused.

The callback is called only after admission and behavior selection. A callback
failure returns the pre-callback environment and observation, but preserves the
already consumed behavior generator state and attempted-transition/work ledger;
no replay append or DQN state change occurs. Once a callback succeeds, its
transition and replay append are committed even if later batch selection or
update fails. A later update failure also preserves its batch generator state
and update-attempt ledger, while preserving the pre-update online and target
snapshots. A failed target-schedule calculation publishes neither online nor
target replacement. Every error result contains the latest committed trainer
state and bounded report, so it can be inspected or resumed under new fuel or
limits; callbacks with real external effects still need their own transaction
discipline.

If replay-batch selection fails after one or more sequential draws, the
partial ordered IDs, their batch/work ledger charges, and the generator after
the final successful draw are retained in the returned state and step report.
A failed sampler produces no new generator, so it retains that current
generator. The admitted uniform batches contain only retained IDs with unit
positive finite masses; the partial-failure path nevertheless preserves the
exact general advancement boundary.

## 6. Tests and benchmark

`DQNTrainer` tests use a deterministic tiny callback and independently
computed expected states. They cover: exact and one-below every ledger limit;
positive fuel and fuel exhaustion; split-run equality; mask order and
epsilon-zero/full behavior; warming, FIFO eviction, and sampled ID order;
standard and Double-DQN targets; no-op, periodic hard, and Polyak checkpoint
schedules; terminal payoff versus fuel truncation; callback failure; and failed
batch/schedule atomicity. Existing `DQN` and replay tests remain unchanged.

The package adds one standalone benchmark executable, without a benchmark
framework. It runs a fixed deterministic continuing environment, one excluded
warm-up and twenty measured complete trainer calls. Each call reads the same
fully built input state through a no-inline IORef operation before applying the
pure trainer, so evaluation cannot share a previous timed result. The benchmark
requires the warm-up and final semantic receipts to agree, then prints stop,
fuel used, final accounting, and step count, raw nanosecond samples, mean,
sample standard deviation, toolchain details, fixed configuration, seed, and
workload size. A timed call includes pure callback execution, behavior/batch
sampling, replay, DQN update, and checkpoint handling. It excludes Cabal
process startup, compilation, fixture construction, and report printing; it
makes no accelerator, throughput, convergence, or production claim. Timing
evidence is separate from the semantic receipt and tests.

## 7. Placement and dependency change

Production code belongs in
`backends/markovian-neural-bridge/src/Markovian/Backend/Neural/Bridge/DQN/Trainer.hs`.
Tests and the benchmark belong in the bridge package. The bridge adds direct
`markovian-numerical` and `markovian-sampling` bounds to reuse the canonical
finite-distribution constructor and explicit generator; it does not depend on
`markovian-learning` and creates no dependency cycle. This preserves the
accepted `markovian-neural` independence from the root and sampling libraries.
The bridge Cabal file owns these direct package bounds; the shared release manifests record those dependencies.

## 8. Executed benchmark

On 2026-09-14, the command below ran with GHC 9.14.1 on aarch64 Linux.
`cabal.project.ci` disables optimization. The excluded warm-up and final
semantic receipts agreed: fuel exhausted after 32 transitions, accounting
`32 / 32 / 116 / 29 / 29 / 238`, and 32 step reports.

```sh
cabal bench dqn-trainer-bench --project-file=cabal.project.ci
```

All twenty wall-time samples, in nanoseconds:

```text
6485868, 6400476, 4468840, 3142438, 3141525,
3176086, 3126998, 3073685, 3118598, 3042950,
2988693, 2998646, 3023845, 3078438, 3155509,
3050950, 3258374, 3075670, 3026773, 3044182,
```

The mean was 3.493927 ms and sample standard deviation
1.056566 ms. These local timings include host scheduling and are not a
throughput bound. Independent O2 instrumentation counted all 704 environment
callbacks across the benchmark's 22 complete calls; the measured calls did
not reuse the warm-up result. Instrumented timings are not used here.
