# The Markovian Book

Markovian is a greenfield Haskell framework for finite stochastic models. It provides exact semantics, checked composition, bounded evaluation, and optional numerical backends.

The framework separates mathematical meaning from execution. Exact rational code defines the reference behavior. Floating, sampled, GPU, and neural code cross explicit approximation boundaries.

The foundation route starts with [algebra](algebra-primer.md), [category theory](category-primer.md), and [measure theory](measure-theory-primer.md). [Categorical probability](categorical-probability.md) then connects kernels, states, copying, and Bayesian inversion. [Information theory](information-theory.md) adds channel observables. [Categorical learning](categorical-learning.md) explains reverse derivatives, sharing, and optimization. [Polarity, push-pull duality, and games](polarity-and-games.md) then compares typed forward and backward flow across probability, learning, logic, and interaction.

Each foundation chapter states its assumptions and unsupported generalizations. Each chapter also links its claims to primary references and relevant Markovian APIs.

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

The exact layers depend only on `base`. CUDA and neural code lives in separate packages.

## Control and learning axes

The book classifies control and learning methods along three independent axes:

- model-based or model-free target;
- on-policy or off-policy;
- tabular or function approximation.

Exact value iteration and policy iteration are model-based. The TD, policy-gradient, and DQN updates use observed transitions or episodes.

Read [Control and learning taxonomy](control-learning-taxonomy.md) before selecting a learning API. Read [Information theory](information-theory.md) for entropy, KL, and data processing, and [Categorical learning](categorical-learning.md) for VJPs, adjoints, sharing, and optimizer boundaries.

## Four operations called reversal

The framework keeps four unrelated operations separate.

| Operation | Required data | Meaning |
| --- | --- | --- |
| Matrix transpose or conjugate transpose | A raw semiring matrix | Reverse source and target indexes |
| Bayesian inversion | A prior and a stochastic channel | Infer a supported source from an observed target |
| Open-boundary reversal | A structured cospan | Swap the input and output boundary legs |
| Reverse derivative | A differentiable map, primal point, and output cotangent | Propagate sensitivity through a local derivative adjoint |

No common `Dagger` or reversal class joins these operations. State pushforward and payoff pullback form another typed pair, but neither operation adds a fifth inverse. The [polarity and games chapter](polarity-and-games.md) explains this distinction.

## How to read this book

Read [First exact MDP](first-mdp.md) for an executable example. Read [Choose an API](choose-an-api.md) when you have a specific task.

Read [Exact evaluation and control](exact-evaluation.md) for model-based methods. Read [Sampling and tabular learning](sampling-learning.md) for sample-based table updates.

The model chapters explain rewards, policies, evaluation, learning, and partial observability. The composition chapters explain matrices, Bayesian inference, circuits, and open systems.

Use the foundation chapters as a second route through the same software. They explain why the APIs separate exact values, channels, observations, reversals, and approximations.

The final chapters state execution boundaries and unsupported claims. The project references include the complete architecture and decision records.

## Project status

The package is unreleased. It makes no compatibility promise. The project removes an incorrect API instead of preserving it behind a compatibility layer.

The implemented scope is finite and discrete. Cyclic MDP value problems use bounded dynamic programming or Bellman fixed points. Arbitrary cyclic open-graph interpretation remains outside the implemented scope.

## Mathematical guides

- [Law catalogue and proof boundaries](laws-and-boundaries.md)
- [Derived categorical and mathematical insights](categorical-insights.md)
- [References and further reading](references.md)
