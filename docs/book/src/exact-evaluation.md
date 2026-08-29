# Exact evaluation and control

The exact modules use `Rational` through validated exact values. They support policy evaluation and model-based control for finite models.

## Policy-free finite compilation

Compile the complete MDP once:

```haskell
compiled <-
  compileExactMDP
    stateSupport
    actionSupport
    model
```

`compileExactMDP` does not receive or inspect a policy. For each continuing state, it stores every available action in model order. It also stores each complete joint reward-successor distribution.

Compilation validates these facts:

- The state index is nonempty and duplicate-free.
- The action index is duplicate-free and can be empty for a terminal-only model.
- The index contains the initial state.
- Every available action has an index.
- Every successor has a state index.
- Every transition distribution is valid.

Compilation preserves duplicate labeled outcomes and support order. It does not replace a joint outcome with independent reward and transition summaries.

Close a compiled model under a policy before policy evaluation:

```haskell
compiledPolicy <- closeCompiledExactPolicy compiled policy
```

This operation validates policy support once and returns a `CompiledExactMRP`. The finite-horizon and Bellman policy evaluators consume that closed process.

## Trace enumeration

Use trace enumeration for a short horizon:

```haskell
traces <- exactTraceDistribution objective model policy
value  <- expectedExactReturn objective model policy
```

The trace distribution records every action, transition reward, successor, and stop reason. Its expectation equals the direct exact evaluator on tested fixtures.

Trace enumeration helps explain a result. Its support can grow exponentially with the horizon.

## Finite-horizon policy evaluation

```haskell
report <- evaluateCompiledExactFinite objective compiledPolicy
```

Iteration zero assigns each terminal payoff to its terminal state. It assigns zero to each continuing state.

Each later iteration applies one Bellman expectation backup:

\\[
V_{n+1}^{\pi}(s)=
\sum_{r,s'}K_{\pi}(s)(r,s')
\left(r+\gamma V_n^{\pi}(s')\right).
\\]

The evaluator fixes terminal values at their payoffs on every iteration. It returns all represented state values and the initial-state value.

Use this evaluator for a finite horizon and cyclic state graph. It does not expand a recursive transition tree.

## Discounted Bellman policy evaluation

Use the Bellman solver when the discount is a contraction:

```haskell
config =
  exactBellmanConfig
    contractionDiscount
    tolerance
    maximumIterations

report <- solveCompiledExactPolicy config compiledPolicy
```

For the policy Bellman operator `T_π`, the solver computes the exact sup-norm residual:

\\[
\delta_{\pi}=\lVert T_{\pi}V-V\rVert_{\infty}.
\\]

It reports the contraction value-error bound:

\\[
\lVert V-V^{\pi}\rVert_{\infty}
\le
\frac{\delta_{\pi}}{1-\gamma}.
\\]

The arithmetic and residual are exact. A positive stopping tolerance still permits an approximation to the infinite-horizon fixed point.

## Exact value iteration

Value iteration uses the policy-free compiled MDP. For each action, it computes:

\\[
Q_V(s,a)=
\sum_{r,s'}K(s,a)(r,s')
\left(r+\gamma V(s')\right).
\\]

The optimality backup is:

\\[
(T_{\star}V)(s)=\max_{a\in A(s)}Q_V(s,a).
\\]

Construct a bounded configuration and run it:

```haskell
config =
  exactValueIterationConfig
    contractionDiscount
    tolerance
    maximumIterations

report <- solveCompiledExactControl config compiled
```

The initial vector contains terminal payoffs and zero continuing values. A zero iteration limit performs no backups. A terminal-only model has residual zero.

The report includes the validated configuration, backup count, exact values, initial-state value, residual, bounds, and stop reason. Define:

\\[
\delta_{\star}=\lVert T_{\star}V-V\rVert_{\infty}.
\\]

The value error satisfies:

\\[
\lVert V-V^{\star}\rVert_{\infty}
\le
\frac{\delta_{\star}}{1-\gamma}.
\\]

The greedy policy obtained from `V` has the reported performance bound:

\\[
\lVert V^{\star}-V^{\pi_V}\rVert_{\infty}
\le
\frac{2\gamma\delta_{\star}}{(1-\gamma)^2}.
\\]

A finite iterate is not called an exact optimum. Read `exactValueIterationStopReason` and both bounds before using its greedy policy.

## Deterministic greedy extraction

Extract greedy actions from a complete compiled value vector:

```haskell
greedy <-
  extractExactGreedyActions
    contractionDiscount
    compiled
    values
```

The extractor requires one unique value for every compiled state. Each terminal value must equal its compiled payoff. These checks prevent a caller-supplied terminal value from invalidating the residual and policy bound.

The extractor visits actions in each state's model availability order. It replaces the current choice only for a strict greater value. Exact ties therefore select the first model-available action.

This tie rule does not depend on global action-index order. Model availability order is observable only when action values tie.

## Exact policy iteration

Policy iteration starts with the first available action at each continuing state:

```haskell
config =
  exactPolicyIterationConfig
    contractionDiscount
    maximumPolicyImprovements

report <- solveCompiledExactPolicyIteration config compiled
```

Each iteration performs these operations:

1. Solve the fixed-policy equations with signed rational Gaussian elimination.
2. Compute every exact action value.
3. Select the first exact maximizer in model availability order.
4. Continue only if some state has a strict improvement.

The result reports stable or iteration-limit termination. A stable result has no strict policy improvement. The explicit limit also bounds this solver.

The implementation returns structured failures for singular systems and internal invariants. It does not route signed subtraction through the nonnegative matrix API.

## Terminal timing

A terminal state returns its payoff before any policy, action, transition, or horizon request. A transition into a terminal state contributes its reward and one discounted payoff.

For reward `2`, discount `1/2`, and terminal payoff `7`:

\\[
2+\frac{1}{2}\cdot 7=\frac{11}{2}.
\\]

The payoff is not a second transition reward.

## Selection guide

| Requirement | Evaluator |
| --- | --- |
| Show every path | Trace enumeration |
| Get one short-horizon policy value | Direct exact evaluator |
| Get all finite-horizon policy values | Dynamic programming |
| Evaluate a discounted cyclic policy | Bellman policy solver |
| Get bounded optimality values and error bounds | Exact value iteration |
| Get exact deterministic policy improvement and stable-stop evidence | Exact policy iteration |
| Execute one random path | Sampled interpreter |

## Further reading

- [Puterman: Markov decision processes](references.md#puterman-markov-decision-processes)
- [Bellman: dynamic programming](references.md#bellman-dynamic-programming)
