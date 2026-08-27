# Sampling and Q-learning

The sampled and learning modules use validated floating values. They remain separate from exact rational semantics.

## Explicit generators

A sampled evaluation consumes one generator and returns the next generator:

```haskell
sample <- sampleReturn objective model policy generator

sampledReturn sample
sampledTrace sample
sampledGenerator sample
```

Use the returned generator for the next draw. Reusing the input generator repeats the same random stream.

The trace records the selected action, reward, successor, and stop reason. This data makes a sampled failure reproducible.

## Sample timing

The sampled evaluator uses the same reward and terminal timing as the exact evaluator.

1. Inspect the current state.
2. Stop and collect its payoff if it is terminal.
3. Stop without a payoff if the horizon is zero.
4. Sample an available action from the policy.
5. Sample a joint reward and successor outcome.
6. Continue with one fewer step.

## Pure Q updates

`updateQ` consumes one observed transition:

```haskell
observed =
  ObservedTransition
    { observedState = state
    , observedAction = action
    , observedReward = reward
    , observedSuccessor = successor
    }

updated <- updateQ rate discount model observed table
```

For a continuing successor, the target is:

\[
r+\gamma\max_{a'}Q(s',a').
\]

For a terminal successor, the target includes the terminal payoff exactly once. The update does not request actions from a terminal state.

A missing Q-table entry denotes zero. Constructors reject duplicate keys and nonfinite values.

## Bounded episodic learning

```haskell
config =
  qLearningConfig
    discount
    (ConstantLearningRate alpha)
    (ConstantExploration epsilon)
    episodeLimit
    episodeStepLimit

result <- learnEpisodes config model generator
```

The result contains the final table, every episode trace, each pure update, the update count, and the final generator.

Both limits are mandatory. A cyclic model cannot create an unbounded episode.

## Appropriate use

Use this module for small tabular experiments and reference learning behavior. Compare learned values with exact dynamic-programming values when possible.

Do not use a statistical-frequency assertion as the main correctness gate. Test the pure update and seeded traces directly.
