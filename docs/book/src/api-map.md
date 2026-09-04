# Public module map

This map describes the `2026.9.3.0` released API. `release/exposed-modules` contains the checked package snapshots.

The unaccepted D-061 migration keeps the root on exact and exact-neutral structural modules. Optional packages currently own numerical, sampled, learning, dense, and benchmark modules. This map records the working API, not release acceptance.

| Package | Public ownership |
| --- | --- |
| `Markovian` | Finite exact and exact-neutral structural semantics |
| `markovian-numerical` | Floating finite models and approximate circuit interpretation |
| `markovian-sampling` | Explicit generators and sampled interpreters |
| `markovian-learning` | Tabular updates and bounded episodic runners |
| `markovian-dense-exact` | Dense `Rational` circuit lowering and execution |
| `markovian-exact-benchmarks` | Exact inventory fixtures, reports, and benchmarks |

D-061 is `Accepted` for the reviewed package graph. D-067 is accepted only for the bounded effect-capable reverse interpreter and host tensor adapter.

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
| `Markovian.Action` | Root-owned nominal action IDs |
| `Markovian.Probability` | Validated floating probabilities and distributions in `markovian-numerical` |
| `Markovian.Probability.Exact` | Checked exact rational distributions in the root |
| `Markovian.Kernel` | Floating one-layer kernels in `markovian-numerical` |
| `Markovian.Kernel.Exact` | Fallible exact kernels with explicitly limited composition |
| `Markovian.MRP` | Floating Markov reward processes in `markovian-numerical` |
| `Markovian.MDP` | Floating MDPs in `markovian-numerical` |
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
| `Markovian.Benchmark.Inventory.Serial.Exact` | Synthetic bounded serial fixture, exact oracle, and base-stock comparison |
| `Markovian.Benchmark.Inventory.Report` | Deterministic primary-versus-widened inventory report |
| `Markovian.Benchmark.Inventory.Distribution.Dogru.Exact` | Exact bounded physical and signed-relaxed two-retailer comparison |
| `Markovian.Benchmark.Inventory.Distribution.Dogru.Report` | Deterministic Doğru finite-horizon evidence report |
| `Markovian.Benchmark.Inventory.ClarkScarf1960.Finite.Exact` | Section III finite state, timing, exact outcomes, reachable layouts, and budgets |
| `Markovian.Benchmark.Inventory.ClarkScarf1960.Oracle.Exact` | Exact equation-(14), equation-(15), opportunity-loss, equation-(26), and joint-policy comparison |
| `Markovian.Benchmark.Inventory.ClarkScarf1960.Report` | Cited deterministic finite-bound and truncation report |
| `Markovian.Benchmark.Inventory.Serial.FixedBatch.Exact` | Bounded two-stage physical execution, layouts, finite-horizon oracle, and policy grid |
| `Markovian.Benchmark.Inventory.Serial.FixedBatch.Newsvendor.Exact` | Separate exact stationary subsystem costs, shortfalls, and discrete inequalities |
| `Markovian.Benchmark.Inventory.Serial.FixedBatch.Report` | Deterministic widening, truncation, execution, and stationary evidence report |

## Matrices, Bayes, and circuits

| Module | Purpose |
| --- | --- |
| `Markovian.Category.Matrix` | Raw finite semiring matrices |
| `Markovian.Category.Matrix.Stochastic` | Exactly normalized matrices |
| `Markovian.Category.Payoff.Exact` | Checked exact finite payoffs, pullback, and state pairing |
| `Markovian.Category.Matrix.Deterministic` | Proof-carrying one-hot matrices |
| `Markovian.Category.Convex.Exact` | Exact convex channel mixtures |
| `Markovian.Bayesian.Exact` | Priors, conditioning, and inversion |
| `Markovian.Bayesian.Channel.Exact` | Channels with checked prior flow |
| `Markovian.Circuit` | Purity-indexed free stochastic circuits |
| `Markovian.Circuit.Interpret.Exact` | Exact circuit fold and bounded deterministic execution |
| `Markovian.Circuit.Interpret.Cost` | Bounded static owner and structural cost reports |
| `Markovian.Circuit.Interpret.Approximate` | Explicit approximation boundary |
| `Markovian.Circuit.Compile.Deterministic` | First-order deterministic terms |
| `Markovian.Circuit.Rewrite.Deterministic` | Opaque deterministic rewrite candidates |
| `Markovian.Circuit.Rewrite.Deterministic.Exact` | Exact checked witnesses and post-check costs |
| `Markovian.Backend.CPU.Exact` | Optional dense rational circuit lowering and execution |

The old `Markovian.Category.Finite.Exact` module is private regression code. No package exposes it.

## Checked finite feedback

| Module | Purpose |
| --- | --- |
| `Markovian.Feedback.Channel.Exact` | Proper first-exit coproduct routing with exact solve evidence |
| `Markovian.Feedback.Delay.Exact` | Explicit seed and bounded one-tick delayed execution |
| `Markovian.Feedback.Timed.Exact` | Nilpotent reward-, duration-, and output-preserving closure |
| `Markovian.Feedback.Value.Exact` | Proposed strict-discount affine `A` and `K` value coefficients |

These modules provide no universal trace, instantaneous fixed-point operator, cyclic open-system black-boxing, or stationary-distribution selector. The value module returns no evaluator or normalized output channel. D-078 remains `Proposed`.

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

## Finite interaction protocols

| Module | Purpose |
| --- | --- |
| `Markovian.Game.Arena` | Finite reachable acyclic alternating arenas and opaque legal-history replay |
| `Markovian.Game.Strategy` | Bounded receptive deterministic strategies, copycat, partial synchronized composition, and external-prefix equality |

These modules do not provide justification pointers, views, innocence, payoff, best response, equilibrium, chance, recursion, or full game semantics.

## Finite open games

| Module | Purpose |
| --- | --- |
| `Markovian.Game.Optic.Finite` | Bounded total finite functions, bijections, and concrete play/coplay optics |
| `Markovian.Game.Open.Finite` | Owned finite strategy profiles, composition, pure equilibria, and exhaustive observational equality |
| `Markovian.Game.Open.Exact` | Exact rational maximizing decisions, exact-payoff contexts, and deterministic reports |

These modules do not import arena histories. They provide no generic mixed lifting, continuous, repeated, or subgame-perfect game semantics and no equilibrium-existence result.

## Exact mixed, stochastic, and Bayesian games

| Module | Purpose |
| --- | --- |
| `Markovian.Game.Profile.Finite` | Checked owner products, pure profiles, exact complete simplexes, and shared game limits |
| `Markovian.Game.NormalForm.Exact` | Complete rational payoff tables, independent mixed profiles, exact expectation, and mixed-Nash candidate checks |
| `Markovian.Game.Correlated.Exact` | Joint correlation devices and distinct CE/CCE candidate reports |
| `Markovian.Game.Outcome.Exact` | Complete exact joint outcome laws and reward/successor atoms |
| `Markovian.Game.Stochastic.Exact` | Finite-horizon public-state Markov evaluation and Markov-perfect candidate checks |
| `Markovian.Game.Harsanyi.Exact` | Correlated common priors, behavioral Bayes-Nash checks, and bounded strategic-normal conversion |
| `Markovian.Game.Open.Strategic.Exact` | Checked extraction of one closed open-game context |

These are candidate semantics, not unrestricted solvers. They provide no equilibrium-existence theorem, complete real-equilibrium enumeration, private-history stochastic game, repeated game, or general mixed open-game semantics.

## Optional packages

| Package and module | Purpose |
| --- | --- |
| `markovian-continuous: Markovian.Continuous.Space` | Closed finite, real, and product standard-Borel witnesses |
| `markovian-continuous: Markovian.Continuous.Map` | Rational affine measurable real maps |
| `markovian-continuous: Markovian.Continuous.Polynomial` | Bounded rational univariate and bivariate polynomials |
| `markovian-continuous: Markovian.Continuous.Measure.Exact` | Affine-uniform laws, exact polynomial moments, and shared-noise joints |
| `markovian-continuous: Markovian.Continuous.Kernel.Exact` | Checked affine additive-uniform kernels and fallible composition |
| `markovian-continuous: Markovian.Continuous.Condition.Exact` | Positive-evidence finite affine-likelihood conditioning |
| `markovian-continuous-numerical: Markovian.Continuous.Numerical.Value` | Finite values and explicit rational rounding reports |
| `markovian-continuous-numerical: Markovian.Continuous.Numerical.Generator` | Owned SplitMix64 state and named real-law sampling |
| `markovian-continuous-numerical: Markovian.Continuous.Numerical.Quadrature` | Bounded deterministic GK15/7 with estimated error |
| `markovian-continuous-numerical: Markovian.Continuous.Numerical.MonteCarlo` | Bounded resumable Welford reports |
| `markovian-gpu: Markovian.Backend.GPU` | Prepared F64 matrix/VJP CPU and optional admitted CUDA execution with explicit cleanup and pre-launch fallback |
| `markovian-neural: Markovian.Backend.Neural.Approximation` | Explicit precision, error, and observation boundary |
| `markovian-neural: Markovian.Backend.Neural.Numeric` | Opaque finite scalars, checked floating arithmetic, and tolerances |
| `markovian-neural: Markovian.Backend.Neural.Dense` | Dense networks and manual VJPs |
| `markovian-reverse: Markovian.Reverse` | Typed parametric VJP composition and finite cotangent-module metadata |
| `markovian-reverse: Markovian.Reverse.Program` | Bounded owned reverse syntax with structural parameter ownership and opaque stored or recomputed tapes |
| `markovian-autodiff: Markovian.Autodiff.Shape` | Closed unit, scalar, fixed-vector, product, and parameter-owner shapes |
| `markovian-autodiff: Markovian.Autodiff.Language` | Closed polynomial and `tanh` syntax with explicit sharing |
| `markovian-autodiff: Markovian.Autodiff.Compile` | Exact polynomial and checked-Double lowering to opaque reverse tapes |
| `markovian-tensor: Markovian.Tensor.Shape` | Type-indexed scalar, vector, matrix, and higher-rank shape witnesses |
| `markovian-tensor: Markovian.Tensor` | Region-scoped host F64 buffers, checked layouts, finite refinement, and deterministic reports |
| `markovian-tensor: Markovian.Tensor.Primitive` | Deterministic finite CPU elementwise, reduction, copy, and matrix primitives |
| `markovian-tensor: Markovian.Tensor.Ownership` | Semantic owner keys separate from physical storage IDs |
| `markovian-tensor: Markovian.Tensor.Reverse` | Opaque tapes and VJPs for the closed CPU primitive set, backed by a private allocator capability |
| `markovian-reverse: Markovian.Reverse.Program.Effect` | Pure bounded preparation and effectful supplied forward, recomputation, pullback, and cotangent-addition execution |
| `markovian-tensor-reverse: Markovian.Tensor.Reverse.Program` | Rank-2 host adapter for closed F64 `tanh` and pointwise-multiplication symbols |
| `markovian-safetensors: Markovian.Tensor.SafeTensors` | Bounded metadata-free F64 parser and canonical encoder for the pinned SafeTensors profile |
| `markovian-neural: Markovian.Backend.Neural.Categorical` | Stable categorical values and analytic gradients |
| `markovian-neural: Markovian.Backend.Neural.Information` | Checked entropy, cross entropy, KL divergence, mutual information, and logit gradients |
| `markovian-neural: Markovian.Backend.Neural.Mask` | Sized structural masks, Boolean flags, ordered gather, and positive-zero scatter |
| `markovian-neural: Markovian.Backend.Neural.Policy` | Linear categorical policies and scalar value functions |
| `markovian-neural: Markovian.Backend.Neural.Reinforce` | Finite-episode linear REINFORCE updates |
| `markovian-neural: Markovian.Backend.Neural.ActorCritic` | One-step linear actor-critic updates |
| `markovian-neural: Markovian.Backend.Neural.Transition` | Immutable masked transition snapshots |
| `markovian-neural: Markovian.Backend.Neural.Replay` | Bounded FIFO replay with stable IDs |
| `markovian-neural: Markovian.Backend.Neural.TargetNetwork` | Hard, periodic, and Polyak synchronization |
| `markovian-neural: Markovian.Backend.Neural.DQN` | Standard and Double-DQN batch updates |
| `markovian-neural: Markovian.Backend.Neural` | Small façade that re-exports the neural modules |
| `markovian-neural-bridge: Markovian.Backend.Neural.Bridge.ExactSupportMask` | Bounded exact global action-layout and per-state availability compilation for neural heads |

Use Haddock for complete signatures and error constructors. Use this book for semantic selection and composition rules.
