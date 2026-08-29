# Public module map

## Scalars and finite witnesses

| Module | Purpose |
| --- | --- |
| `Markovian.Algebra.Semiring` | Scalar capabilities and exact coefficient validation |
| `Markovian.Algebra.NonNegativeRational` | Opaque exact nonnegative scalar |
| `Markovian.Category.Finite.Set` | Duplicate-free finite sets, including empty sets |
| `Markovian.Category.Finite.Object` | Nonempty finite object refinement |

## Probability and decision models

| Module | Purpose |
| --- | --- |
| `Markovian.Probability` | Validated floating probabilities and distributions |
| `Markovian.Probability.Exact` | Exact rational probabilities and distributions |
| `Markovian.Kernel` | Floating one-layer kernels |
| `Markovian.Kernel.Exact` | Exact one-layer kernels and Kleisli instances |
| `Markovian.MRP` | Markov reward processes |
| `Markovian.MDP` | Floating MDPs and stable action IDs |
| `Markovian.MDP.Exact` | Exact MDPs and joint transition outcomes |
| `Markovian.Policy` | Floating policies and closure |
| `Markovian.Policy.Exact` | Exact policies and checked closure |
| `Markovian.Objective` | Floating discounts and finite objectives |
| `Markovian.Objective.Exact` | Exact discounts and finite objectives |
| `Markovian.Horizon` | Validated bounded horizons |
| `Markovian.Reward` | Validated floating rewards |
| `Markovian.Reward.Exact` | Exact rational rewards |
| `Markovian.Trace` | Action, reward, successor, and stop traces |

## Evaluation and learning

| Module | Purpose |
| --- | --- |
| `Markovian.Interpreter.Exact` | Exact traces and expected returns |
| `Markovian.Compile.Exact` | Policy-free exact MDP compilation and separate policy closure |
| `Markovian.Interpreter.DynamicProgramming.Exact` | Finite-horizon policy evaluation |
| `Markovian.Interpreter.Bellman.Exact` | Discounted contraction policy evaluation |
| `Markovian.Interpreter.Control.Exact` | Exact value iteration, greedy extraction, and policy iteration |
| `Markovian.Sampling` | Explicit generators and finite-support sampling |
| `Markovian.Interpreter.Sampled` | Seeded sampled returns and traces |
| `Markovian.Interpreter.Sampled.Step` | One selected joint reward-successor sample |
| `Markovian.Learning.Tabular` | Shared finite tables, rates, schedules, and observations |
| `Markovian.Learning.EpsilonGreedy` | Canonical epsilon-greedy distribution and seeded sampler |
| `Markovian.Learning.TD0` | Pure tabular state-value update |
| `Markovian.Learning.TD0.Episodic` | Supplied-policy bounded TD(0) runner |
| `Markovian.Learning.Sarsa` | Pure carried-next-action SARSA update |
| `Markovian.Learning.Sarsa.Episodic` | Bounded epsilon-greedy SARSA runner |
| `Markovian.Learning.ExpectedSarsa` | Pure expected epsilon-greedy SARSA update |
| `Markovian.Learning.ExpectedSarsa.Episodic` | Bounded Expected SARSA runner |
| `Markovian.Learning.QLearning` | Pure greedy-target Q-learning update |
| `Markovian.Learning.QLearning.Episodic` | Bounded epsilon-greedy behavior runner |
| `Markovian.POMDP.Exact` | Exact beliefs and post-transition filtering |
| `Markovian.POMDP.Planning.Exact` | Bounded exact belief-policy evaluation |

## Matrices, Bayes, and circuits

| Module | Purpose |
| --- | --- |
| `Markovian.Category.Matrix` | Raw finite semiring matrices |
| `Markovian.Category.Matrix.Stochastic` | Exactly normalized matrices |
| `Markovian.Category.Matrix.Deterministic` | Proof-carrying one-hot matrices |
| `Markovian.Category.Convex.Exact` | Exact convex channel mixtures |
| `Markovian.Bayesian.Exact` | Priors, conditioning, and inversion |
| `Markovian.Bayesian.Channel.Exact` | Channels with checked prior flow |
| `Markovian.Circuit` | Purity-indexed free stochastic circuits |
| `Markovian.Circuit.Interpret.Exact` | Exact circuit fold and execution |
| `Markovian.Circuit.Interpret.Approximate` | Explicit approximation boundary |
| `Markovian.Circuit.Compile.Deterministic` | First-order deterministic terms |
| `Markovian.Category.Finite.Exact` | Earlier typed exact finite IR |
| `Markovian.Backend.CPU.Exact` | Dense rational lowering and execution |

## Open systems

| Module | Purpose |
| --- | --- |
| `Markovian.Open.Interface` | Finite typed interfaces and maps |
| `Markovian.Open.Hypergraph` | Finite directed labeled hypergraphs |
| `Markovian.Open.Pushout` | Explicit finite quotient pushouts |
| `Markovian.Open.StructuredCospan` | Open-system composition, tensor, and cells |
| `Markovian.Open.Circuit.Exact` | Global circuit-decorated open topology |
| `Markovian.Open.Acyclic` | Acyclic boundary-functional refinement |
| `Markovian.Open.Acyclic.Circuit.Exact` | Exact local-circuit DAG semantics |

## Optional packages

| Package and module | Purpose |
| --- | --- |
| `markovian-gpu: Markovian.Backend.GPU` | Optional CUDA dense application |
| `markovian-neural: Markovian.Backend.Neural.Approximation` | Explicit precision, error, and observation boundary |
| `markovian-neural: Markovian.Backend.Neural.Numeric` | Opaque finite scalars, checked floating arithmetic, and tolerances |
| `markovian-neural: Markovian.Backend.Neural.Dense` | Dense networks and manual VJPs |
| `markovian-neural: Markovian.Backend.Neural.Categorical` | Stable categorical values and analytic gradients |
| `markovian-neural: Markovian.Backend.Neural.Information` | Checked entropy, cross entropy, KL divergence, mutual information, and logit gradients |
| `markovian-neural: Markovian.Backend.Neural.Policy` | Linear categorical policies and scalar value functions |
| `markovian-neural: Markovian.Backend.Neural.Reinforce` | Finite-episode linear REINFORCE updates |
| `markovian-neural: Markovian.Backend.Neural.ActorCritic` | One-step linear actor-critic updates |
| `markovian-neural: Markovian.Backend.Neural.Transition` | Immutable masked transition snapshots |
| `markovian-neural: Markovian.Backend.Neural.Replay` | Bounded FIFO replay with stable IDs |
| `markovian-neural: Markovian.Backend.Neural.TargetNetwork` | Hard, periodic, and Polyak synchronization |
| `markovian-neural: Markovian.Backend.Neural.DQN` | Standard and Double-DQN batch updates |
| `markovian-neural: Markovian.Backend.Neural` | Small façade that re-exports the neural modules |

Use Haddock for complete signatures and error constructors. Use this book for semantic selection and composition rules.
