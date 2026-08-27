# Practical recipes

## Compare two policies exactly

1. Construct one exact MDP.
2. Construct each exact policy.
3. Use one explicit objective for both policies.
4. Run `expectedExactReturn` for a short horizon.
5. Compile both policies for a larger finite model.
6. Compare `exactFiniteDPInitialValue` from both reports.

Do not compare policies under different terminal, horizon, or discount contracts.

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
