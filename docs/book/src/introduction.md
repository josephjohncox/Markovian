# The Markovian Book

Markovian is a greenfield Haskell framework for finite stochastic models. It provides exact semantics, checked composition, bounded evaluation, and optional numerical backends.

The framework separates mathematical meaning from execution. Exact rational code defines the reference behavior. Floating, sampled, GPU, and neural code cross explicit approximation boundaries.

## What the framework prevents

Many probability libraries permit operations that are type-correct but mathematically wrong. Markovian rejects these common errors:

- A matrix transpose used as Bayesian inversion.
- One random value confused with two independent draws.
- A transition reward separated from its successor state.
- A terminal payoff counted more than once.
- An observation assigned to the wrong side of a transition.
- A stochastic matrix passed through an operation that breaks normalization.
- A cyclic model evaluated by unbounded tree expansion.
- A finite support reused with a different storage layout.

## The semantic stack

The framework has five main layers.

1. Validated values define probabilities, rewards, horizons, and finite supports.
2. Kernels define one stochastic step without recursive unfolding.
3. MDP and POMDP types define decision and observation timing.
4. Matrices, circuits, and open systems define compositional structure.
5. Interpreters and backends execute the same model under different contracts.

The exact layers depend only on `base`. CUDA and neural contracts live in separate packages.

## Three operations called reversal

The framework uses separate APIs for three unrelated operations.

| Operation | Required data | Meaning |
| --- | --- | --- |
| Matrix transpose or conjugate transpose | A raw semiring matrix | Reverse source and target indexes |
| Bayesian inversion | A prior and a stochastic channel | Infer a supported source from an observed target |
| Open-boundary reversal | A structured cospan | Swap the input and output boundary legs |

No common `Dagger` class joins these operations.

## How to read this book

Read [First exact MDP](first-mdp.md) for an executable example. Read [Choose an API](choose-an-api.md) when you have a specific task.

The model chapters explain rewards, policies, evaluation, learning, and partial observability. The algebra chapters explain matrices, Bayesian inference, circuits, and open systems.

The final chapters state the execution boundaries and unsupported claims. The project references include the complete architecture and decision records.

## Project status

The package is unreleased. It makes no compatibility promise. The project removes an incorrect API instead of preserving it behind a compatibility layer.

The implemented scope is finite and discrete. Cyclic MDP value problems use bounded dynamic programming or Bellman fixed points. Arbitrary cyclic open-graph interpretation remains outside the implemented scope.
