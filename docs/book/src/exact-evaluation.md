# Exact evaluators and compilation

Markovian provides three exact policy evaluators. Each evaluator serves a different model size and proof need.

## Trace enumeration

Use trace enumeration for a short horizon:

```haskell
traces <- exactTraceDistribution objective model policy
value  <- expectedExactReturn objective model policy
```

The trace distribution records every action, transition reward, successor, and stop reason. Its expectation equals the direct exact evaluator.

Trace enumeration helps explain a result. Its support can grow exponentially with the horizon.

## Finite compilation

Compile a complete finite model before repeated evaluation:

```haskell
compiled <-
  compileExactPolicyMDP
    stateSupport
    actionSupport
    model
    policy
```

Compilation validates these facts:

- The state and action indexes contain no duplicates.
- The index contains the initial state.
- Every successor has an index.
- Every available and selected action has an index.
- Every policy has valid support.
- Every compiled distribution is normalized.

The compiled form stores integer indexes and checked transition distributions. A runtime does not need to rediscover the model graph.

## Finite-horizon dynamic programming

```haskell
report <- evaluateCompiledExactFinite objective compiled
```

Iteration zero assigns each terminal payoff to its terminal state. It assigns zero to each continuing state.

Each later iteration applies one Bellman expectation backup:

\\[
V_{n+1}(s)=
\sum_{r,s'}P_\pi(r,s'\mid s)
\left(r+\gamma V_n(s')\right).
\\]

The evaluator clamps terminal values to their payoffs on every iteration. It returns the value of each represented state and the initial state.

Use dynamic programming for finite horizons and cyclic state graphs. It does not unfold a recursive transition tree.

## Discounted Bellman fixed point

Use the Bellman solver when the discount is a contraction:

```haskell
config =
  exactBellmanConfig
    contractionDiscount
    tolerance
    maximumIterations

report <- solveCompiledExactPolicy config compiled
```

The solver computes an exact rational residual. It reports this stopping bound:

\\[
\lVert V-V^*\rVert_\infty
\le
\frac{\lVert T(V)-V\rVert_\infty}{1-\gamma}.
\\]

The report states whether the bound met the tolerance. It also reports an iteration-limit stop.

The arithmetic is exact. The requested tolerance still defines an approximation to the infinite-horizon fixed point.

## Selection guide

| Requirement | Evaluator |
| --- | --- |
| Show every path | Trace enumeration |
| Get one short-horizon value | Direct exact evaluator |
| Get all finite-horizon state values | Dynamic programming |
| Evaluate a discounted cyclic policy | Bellman solver |
| Execute one random path | Sampled interpreter |
