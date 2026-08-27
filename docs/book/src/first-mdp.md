# First exact MDP

This example defines one continuing state, one terminal state, and one action. The action gives reward `2` and moves to the terminal state.

The terminal state has payoff `7`. The objective uses horizon `1` and discount `1/2`.

## Complete program

The repository compiles and runs this source as the `Sample` executable:

```haskell
{{#include ../../../app/Sample/Main.hs}}
```

Run it with:

```sh
cabal run Sample --project-file=cabal.project.ci
```

## Model construction

The model separates four functions:

```haskell
status Start = ExactContinuing
status Done  = ExactTerminal (exactReward 7)

available Start = [finish]
available Done  = []
```

`status` defines terminal behavior. `available` defines the action support of each continuing state.

The transition kernel returns a distribution of complete outcomes:

```haskell
exactTransitionOutcome (exactReward 2) Done
```

Each outcome contains both its reward and successor. This representation preserves their correlation.

## Policy construction

The policy returns a distribution over stable action IDs:

```haskell
selectedPolicy =
  exactPolicy (exactKernel (const (exactDirac finish)))
```

The policy is deterministic in this example. The same type also permits randomized action selection.

Policy closure validates the selected support against the available actions. It does not silently discard an unavailable action.

## Objective and timing

The objective makes the horizon and discount explicit:

```haskell
objective = exactFiniteObjective horizon discount
```

Evaluation inspects terminal status before the horizon boundary. A continuing state with no remaining steps returns no terminal payoff.

This example performs one transition. Its return is:

\[
2 + \frac{1}{2} 7 = \frac{11}{2}.
\]

The evaluator includes the reached terminal payoff once. It applies the same discount placement in exact and sampled execution.

## Extend the example

Add a second outcome to model transition uncertainty:

```haskell
exactFiniteDist
  [ (exactTransitionOutcome (exactReward 2) Done, 3 / 4)
  , (exactTransitionOutcome (exactReward (-1)) Start, 1 / 4)
  ]
```

Do not define separate reward and successor distributions. Separate distributions destroy branch correlation.

For a short horizon, use exact trace enumeration to inspect every path. For a larger finite model, compile the model and use dynamic programming.
