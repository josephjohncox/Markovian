# Practical recipes

## Compare two policies exactly

1. Construct one exact MDP.
2. Construct each exact policy.
3. Use one explicit objective for both policies.
4. Run `expectedExactReturn` for a short horizon.
5. Compile both policies for a larger finite model.
6. Compare `exactFiniteDPInitialValue` from both reports.

Do not compare policies under different terminal, horizon, or discount contracts.

## Solve a finite exact control problem

1. List every represented state and action ID.
2. Call `compileExactMDP` without a policy.
3. Select a contraction discount and explicit iteration limit.
4. Run `solveCompiledExactControl` for residual bounds.
5. Run `extractExactGreedyActions` on the returned values.
6. Use `solveCompiledExactPolicyIteration` when you need exact stable policy improvement.
7. Inspect each stop reason before using the result.

A finite value-iteration result is a bounded approximation. Report its residual and both contraction bounds.

## Debug an unexpected return

1. Run `exactTraceDistribution`.
2. Inspect every transition reward and successor.
3. Inspect each terminal or horizon stop reason.
4. Recompute the expectation from the trace masses.
5. Compare it with `expectedExactReturn`.

Check for a reward assigned to the wrong transition before you change the evaluator.

## Validate a sampled implementation

1. Build a small exact model.
2. Compute its exact trace distribution.
3. Run sampled execution with a fixed generator.
4. Check the complete seeded trace.
5. Test the sampler's support boundaries directly.

Do not use an unstable frequency threshold as the only correctness test.

## Compare tabular update targets

1. Construct one `ObservedTransition`.
2. Construct explicit Q and V tables.
3. Apply `updateTD0` for a state-value target.
4. Apply `updateSarsa` with an explicit next action.
5. Apply `updateExpectedSarsa` with an explicit epsilon value.
6. Apply `updateQ` for the greedy off-policy target.
7. Compare each returned target before you compare updated tables.

Use a terminal successor to confirm that all four targets equal `r + gamma * g`.

## Resume bounded tabular learning

1. Keep the returned table.
2. Keep the next episode index.
3. Keep the global update count.
4. Keep the returned generator.
5. Pass all four values to the algorithm's `...EpisodesFrom` function.

Do not restart a schedule or reuse the initial generator when you resume.

## Inspect one DQN update

1. Construct a dense online network and topology-compatible target network.
2. Construct validated action masks and transition snapshots.
3. Call `evaluateDQNBatch` before any update.
4. Inspect detached targets, predictions, mean loss, and mean gradient.
5. Call `updateDQNBatch` with the same nonempty batch.
6. Inspect the successful-update count and target synchronization result.

This recipe checks one update. It does not train or evaluate a policy in an environment.

## Diagnose a hidden cause

1. Build a nonempty source object.
2. Construct an exact prior.
3. Construct a normalized source-to-observation channel.
4. Call `observationEvidence` for the observed value.
5. Call `condition` for one posterior.
6. Call `bayesianInverse` when you need all supported reverse rows.

Do not transpose the likelihood matrix.

## Model one shared random cause

1. Build one stochastic circuit for the latent value.
2. Use `shareCircuit` to copy its result.
3. Feed each copy to a different downstream circuit.
4. Interpret the complete circuit exactly.
5. Check that impossible unequal copies have zero mass.

Use two circuit occurrences only when the causes must be independent.

## Compose a feed-forward network

1. Define typed input and output interfaces.
2. Build the directed labeled hypergraph.
3. Validate it with `acyclicOpenSystem`.
4. Define finite value domains for all sorts.
5. Add one local circuit for each label and signature.
6. Build the `AcyclicOpenCircuit`.
7. Run exact denotation or one input assignment.

A cycle error means the network is outside this interpreter. Do not remove the check to force evaluation.

## Add a numerical backend

1. Choose an exact semantic source.
2. Define a deterministic layout conversion.
3. State the numeric precision.
4. Define an output comparison relation.
5. Set an explicit error policy.
6. Add small exact differential fixtures.
7. Add representative benchmarks.
8. Report transfer and setup boundaries.

Keep runtime storage and device dependencies outside the semantic core.
