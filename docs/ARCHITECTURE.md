# Target architecture

This document defines the implemented and target architecture for Markovian. Completed boundaries have compiler, test, Haddock, package, and hosted CI evidence.

`docs/DECISIONS.md` records why the project selected these boundaries. `TODO.md` controls delivery order and completion status.

## 1. Scope

Markovian will provide typed semantics and interpreters for stochastic processes and decision models.

The first supported domain is finite state, finite action, and finite support. The first evaluators use a finite horizon.

Later phases can add discounted cyclic solvers, finite POMDPs, and continuous-kernel experiments. Each later feature must pass its admission gate.

The project does not put tensors, devices, autodiff, neural networks, or sampling frameworks in the semantic core.

### 1.1 Implemented boundary

The semantic core currently contains:

- Opaque `Double`-backed `Prob`, `Weight`, `FiniteDist`, and `Reward` values.
- Separate `Rational`-backed exact probability, distribution, reward, and discount values.
- Validated finite and contraction discounts plus an unbounded natural-number horizon.
- Fail-fast structured construction errors and scaled floating normalization.
- A one-layer floating `Kernel` and a law-bearing composable `ExactKernel`.
- Typed terminal status and joint transition reward and successor outcomes.
- One-step floating and exact MDP interfaces with separate action IDs and transition outcomes.
- Validated floating policy closure and exact reference closure.
- Exact finite-horizon expectation with explicit policy and objective values.
- Explicit SplitMix64 generator state and unbiased finite-support sampling.
- Structured action-labeled traces with terminal and horizon stop reasons.
- Exact bounded trace enumeration for expectation cross-checks.
- Duplicate-free finite state and action indexes with exhaustive exact compilation.
- Exact finite-horizon dynamic programming over compiled policy models.
- Exact discounted Bellman policy evaluation with sup-norm stopping bounds.
- Validated tabular Q-values, schedules, pure updates, and bounded seeded episodes.

The current `FiniteDist` constructor preserves labeled duplicate entries. It removes input zero weights and positive weights whose normalized `Double` mass rounds to zero. Floating constructors canonicalize negative zero. POMDPs, categorical IRs, array lowering, and accelerated backends remain unimplemented.

## 2. Architecture principles

1. Define a model by its denotation, not by one execution library.
2. Separate an action ID from a stochastic outcome.
3. Validate probability, reward, and objective values at construction.
4. Make policy, horizon, discount, seed, and solver tolerance explicit.
5. Represent one model step as one layer.
6. Use Bellman fixed points for cyclic value problems.
7. Use recursion schemes only for recursive model syntax.
8. Keep model definitions separate from interpreters.
9. Keep approximate backends observationally related to a reference interpreter.
10. Admit advanced abstractions only after laws, use cases, and benchmarks justify them.

## 3. Semantic foundation

### 3.1 Stochastic kernels

A stochastic kernel from `X` to `Y` maps each input in `X` to a distribution over `Y`.

```text
K : X -> D(Y)
```

For the first implementation, `D` is a validated finite distribution. A later continuous package can use a measure-backed representation.

The kernel interface must not expose a raw vector of weights. Constructors validate the representation before a kernel can return it.

### 3.2 Kleisli composition

Validated distributions form the computational effect for stochastic choice. Kernels compose with Kleisli composition.

```text
identity(x) = dirac(x)

(L <=< K)(x)(z) = sum_y K(x)(y) * L(y)(z)
```

The implementation can combine duplicate support after composition. The public semantics do not depend on support order.

The exact reference interpreter uses exact arithmetic where practical. It tests identity and associativity as literal equalities.

A floating interpreter uses an explicit tolerance and observational equivalence. It must not claim literal floating-point associativity.

`ExactFiniteDist` bind and `ExactKernel` composition implement the exact equations. They preserve labeled duplicates and deterministic support order. The floating kernel remains one-layer only until a checked composition contract defines underflow, renormalization, and observational error.

### 3.3 Expectation

For a finite distribution `d` and finite-valued function `f`, expectation is:

```text
E_d[f] = sum_x probability(d, x) * f(x)
```

A kernel transforms an observable through expectation:

```text
expect(K, f)(x) = E_(K(x))[f]
```

Finite expectation requires finite support and finite observable values. Continuous expectation also requires measurability and integrability.

Sampling does not define expectation. Sampling is one interpreter for a kernel.

### 3.4 Markov-category structure

The categorical compiler can use these operations:

- Identity returns its input through a Dirac kernel.
- Composition uses Kleisli composition.
- Tensor combines independent kernel executions.
- Copy duplicates one value.
- Discard removes a value.

Copying one sampled value is not equal to executing the sampling kernel twice. The compiler IR must preserve this correlation difference.

Conditioning and Bayesian inversion are not operations of every stochastic kernel category. POMDP belief updates expose their extra normalization and zero-evidence requirements.

## 4. Core value types

All constructors in this section are opaque outside their defining module.

### 4.1 `Prob`

`Prob` represents finite mass in the closed interval from zero to one.

A smart constructor rejects negative, greater-than-one, NaN, and infinite values. It canonicalizes negative zero to positive zero. `ExactProb` uses a separate rational representation and literal equality.

### 4.2 `FiniteDist a`

`FiniteDist a` represents a nonempty finite probability distribution.

Its smart constructor:

1. Rejects empty support.
2. Rejects negative or non-finite weights.
3. Rejects a non-positive or non-finite normalization total.
4. Normalizes positive finite weights with a scaled algorithm.
5. Applies one documented rule for duplicate support.

A direct sum can overflow even when every floating weight is finite. The floating constructor first finds the largest positive weight `m`.

It then computes `z = sum_i (w_i / m)`. It rejects `z` when `z` is non-positive or non-finite.

The normalized mass is `(w_i / m) / z`. This algorithm must normalize two maximum finite `Double` weights to two masses of `0.5`.

Floating division can round a positive weight to zero during scaling or final normalization. The constructor removes that entry after normalization. At least the maximum-weight entry remains positive, so the resulting support stays nonempty. Public `outcomes` never returns a zero mass.

The first floating constructor preserves labeled duplicate entries. A later constructor can combine equal support when stable equality and the use case justify that behavior.

A zero-weight entry does not become a selectable outcome. The current representation also removes a positive input weight when its normalized floating mass rounds to zero.

### 4.3 `SubDist a`

`SubDist a` is not an alias for `FiniteDist a`. Add it only when missing mass represents failure or divergence.

Its total mass is at most one. Every interpreter must explain the meaning of missing mass.

### 4.4 `Reward`

`Reward` is a finite real-valued quantity. The first floating implementation rejects NaN and infinity.

`ExactReward` provides rational rewards for law and example tests. Public code must not compare floating rewards as exact mathematical reals. Floating rewards canonicalize negative zero.

### 4.5 `Discount`

`Discount` permits a finite floating value in the closed interval from zero to one. `ContractionDiscount` requires a value below one for discounted infinite-horizon objectives. Separate rational types provide the same domains for exact interpreters.

### 4.6 `Horizon`

`Horizon` stores an unbounded natural number. Its constructor accepts an `Integer` and rejects negative inputs. It counts the maximum number of transitions without adding a machine-sized overflow boundary.

A horizon of zero returns the terminal payoff for a terminal state. It returns zero for a nonterminal state.

### 4.7 Errors

The public API uses structured errors. It separates these groups:

- Probability validation errors.
- Reward and objective validation errors.
- Model consistency errors.
- Policy support errors.
- Conditioning errors.
- Solver errors.
- Backend errors.

Public interpreters must not use partial indexing, partial maxima, or unchecked division.

## 5. Reward and objective semantics

### 5.1 Reward timing

The target model assigns a reward to a transition. A terminal state can also have one terminal payoff.

```text
TransitionOutcome State = (Reward, State)
transitionOutcome : State -> ActionId -> FiniteDist (TransitionOutcome State)
terminalPayoff    : State -> Reward
```

An MRP omits the action argument. A deterministic reward function can be lifted into this joint outcome kernel.

A model receives the terminal payoff once when evaluation reaches a terminal state.

### 5.2 Finite-horizon objective

For policy `pi`, horizon `h`, and discount `gamma`:

```text
V(pi, 0, s) = terminalPayoff(s)   when s is terminal
V(pi, 0, s) = 0                   otherwise

V(pi, h+1, s) = terminalPayoff(s) when s is terminal

V(pi, h+1, s) =
  sum_a pi(a | s) *
  sum_(r,s') K(s, a)(r, s') *
    (r + gamma * V(pi, h, s'))
```

The interpreter does not request a policy or transition from a terminal state. The discount applies once per transition.

### 5.3 Discounted infinite-horizon objective

This objective requires bounded rewards and `0 <= gamma < 1`. A solver must state its norm, tolerance, residual, and stopping rule.

### 5.4 Total-reward objective

Undiscounted, unbounded total reward is not a default. It requires a proof of properness, almost-sure termination, or another integrability condition.

The API must not select these assumptions from a Boolean flag. It must use a separate objective and solver contract.

### 5.5 Other objectives

Average reward, constrained reward, risk-sensitive return, and multi-objective return are separate objective types. Do not encode them by overloading `Reward` or `Discount`.

## 6. Model interfaces

### 6.1 Markov reward process

An MRP has:

- An initial state or initial finite distribution.
- A terminal predicate.
- A transition outcome kernel from state to reward and next state.
- A terminal payoff.

The joint kernel preserves reward and successor correlation. A nonterminal state must return a valid transition outcome distribution.

A terminal state has no transition request.

The interface supports a generative representation first. A finite indexed representation can also expose matrices for exact solvers.

### 6.2 Markov decision process

An MDP has:

- An initial state or initial finite distribution.
- Available action IDs for each nonterminal state.
- A transition outcome kernel from state and action ID to reward and next state.
- A terminal predicate and terminal payoff.

A constructor can accept a next-state kernel and a deterministic transition-reward function. It combines them into the transition outcome kernel.

Available action IDs are nonempty and unique within each state. An unavailable action ID produces a structured error.

The model does not contain a learning algorithm. The model also does not contain exploration or optimizer state.

### 6.3 Policy

A policy kernel maps a state to a finite distribution over available action IDs.

```text
pi : State -> FiniteDist ActionId
```

A deterministic policy returns a Dirac distribution. A stateful or history-dependent controller uses an explicit controller state in its input type.

The policy closure operation combines an MDP and policy into an MRP. Let `K(s,a)(r,s')` be the joint transition outcome mass.

```text
K_pi(s)(r, s') = sum_a pi(a | s) * K(s, a)(r, s')
P_pi(s' | s)   = sum_r K_pi(s)(r, s')
```

Closure stores `K_pi`, not only `P_pi` and an average reward. This preserves correlations between reward and successor state.

A matrix interpreter can derive a conditional expected reward:

```text
Rbar_pi(s, s') =
  sum_r K_pi(s)(r, s') * r / P_pi(s' | s)  when P_pi(s' | s) > 0
```

When `P_pi(s' | s) = 0`, `Rbar_pi(s,s')` has no semantic value. Matrix lowering omits that pair.

A public request for that conditional value returns `ZeroMassTransition`. It never divides by zero or selects an arbitrary reward.

For every bounded observable `f` of reward and successor state, closure guarantees:

```text
E_(r,s')~K_pi(s)[f(r,s')]
  = sum_a pi(a | s) * E_(r,s')~K(s,a)[f(r,s')]
```

Kleisli composition extends this equality to finite reward-and-state traces and finite returns.

The exact reference interpreter checks this equality literally. A floating interpreter checks it under its documented observational tolerance.

Closure intentionally removes the selected action ID from an MRP trace. Code that needs action-labeled traces must use the MDP trace interpreter.

Closure validates policy support before composition.

The floating implementation returns a fallible `PolicyMRP`. It validates the requested state because an arbitrary state type cannot be exhaustively checked when closure is constructed. It reports duplicate model actions, duplicate policy actions, unavailable policy actions, and floating normalization failures. The exact implementation closes one state's policy and transition distributions for literal joint-outcome and trace laws. Neither implementation evaluates a policy at a terminal state.

Policy closure is the only standard path from MDP evaluation to MRP evaluation. An evaluator must not treat stochastic outcomes as selectable actions.

### 6.4 Tabular Q-learning interpreter

The Q-table key is `(state, ActionId)`. Missing keys denote zero. The model remains free of table, schedule, exploration, and generator state.

For one observed transition `(s,a,r,s')`, the pure update is:

```text
target = r + gamma * terminalPayoff(s')        when s' is terminal
target = r + gamma * max_a' Q(s',a')           otherwise
Q'(s,a) = Q(s,a) + alpha * (target - Q(s,a))
```

The maximum ranges only over validated actions available at `s'`. The source action must be available at `s`. Learning rate, epsilon, discount, episode count, per-episode step count, and generator state are explicit. The current schedules are validated constants. Epsilon-greedy ties choose the first available action; support order therefore defines deterministic tie-breaking, not model semantics.

Each episode checks terminal status before its step limit. Transition and terminal rewards use the bounded-interpreter discount convention. Equal initial generator state produces equal traces, updates, tables, and returned generator state.

### 6.5 Partially observable Markov decision process

A POMDP adds:

- A latent state.
- An observation type.
- An observation kernel, normally from action ID and successor state to observation.
- An initial belief distribution.
- A belief-update operation.

A belief update conditions the predicted belief on an observation. It returns a structured zero-evidence error when the normalizing mass is zero.

The public POMDP interface must state whether the observation occurs before or after the transition. The target default observes after the transition.

A belief-state MDP is an interpreter construction. It is not the definition of a POMDP.

## 7. Cyclic systems and fixed points

A state graph can contain cycles without recursive Haskell values. A one-step function can return a successor state equal to its input.

Bounded interpreters stop cyclic paths at the horizon. They require no tree expansion.

Let `N` contain nonterminal states and `Z` contain terminal states. For a fixed policy and discounted objective:

```text
V_pi(t) = terminalPayoff(t)                                  for t in Z

V_pi(s) =
  sum_(r,s') K_pi(s)(r,s') * (r + gamma * V_pi(s'))          for s in N
```

The Bellman operator acts on values for `N`. It substitutes the fixed terminal payoff for successors in `Z`.

With bounded rewards and `gamma < 1`, this operator is a contraction in the supremum norm. It has one bounded fixed point.

For a finite indexed model, define `P_NN`, `P_NZ`, terminal vector `g_Z`, and immediate reward vector `r_N` from `K_pi`.

```text
P_NN(s,s') = sum_r K_pi(s)(r,s')                             for s,s' in N
P_NZ(s,t)  = sum_r K_pi(s)(r,t)                              for s in N, t in Z
r_N(s)     = sum_(r,s') K_pi(s)(r,s') * r                    for s in N

(I_N - gamma * P_NN) * V_N = r_N + gamma * P_NZ * g_Z
V_Z = g_Z
```

The linear system applies only to nonterminal states. The right-hand side includes all terminal-payoff contributions.

An iterative solver clamps terminal values to `g_Z`. It reports the residual over the nonterminal Bellman equations.

Under the contraction conditions, residual `delta` gives the bound `delta / (1 - gamma)` in the supremum norm.

Optimal MDP value uses a maximum over available action IDs. Policy iteration and value iteration remain interpreter choices.

Undiscounted cyclic systems need separate properness conditions. The implementation must reject or isolate cases without a stated convergence contract.

## 8. Finite and continuous models

### 8.1 Finite models

Finite models are the reference domain. They support:

- Exact rational law tests.
- Enumerated expectation.
- Seeded sampling.
- Dense or sparse transition operators.
- Bounded dynamic programming.
- Discounted fixed-point solvers.

A finite indexed model records stable state and action indexes. Conversion from a generative model validates closure and uniqueness.

### 8.2 Continuous models

Continuous kernels need a separate representation and package boundary. They do not use `FiniteDist` as a false abstraction.

A continuous interface must state:

- The measurable spaces or operational substitutes.
- Which kernels can be sampled.
- Which expectations can be computed.
- Integrability assumptions.
- Conditioning support and zero-evidence behavior.
- Numerical or Monte Carlo error contracts.

The first continuous work should be an interpreter experiment. It must not widen the finite core before a use case establishes the required operations.

## 9. MDP variants

The architecture treats each variant as an explicit extension.

| Variant | Status and boundary |
| --- | --- |
| Finite episodic MDP | First supported MDP. It uses a finite horizon. |
| Discounted continuing MDP | Planned after bounded interpreters. It uses Bellman fixed points. |
| Average-reward MDP | Deferred. It needs recurrence and gain or bias semantics. |
| Constrained MDP | Deferred. It needs separate cost signals and feasibility semantics. |
| Semi-Markov decision process | Deferred. It needs explicit duration and discount timing. |
| POMDP | Planned finite extension with observation and belief semantics. |
| Multi-agent model | Out of the initial scope. It needs joint actions and equilibrium concepts. |
| Continuous-state MDP | Experimental package only after continuous-kernel admission. |

A variant does not enter the core through optional fields. It receives its own interface and objective contract.

## 10. Interpreter separation

The semantic core defines values, kernels, models, policies, and objectives. It performs validation but no execution-specific optimization.

Interpreters provide:

- Exact finite expectation.
- Seeded simulation.
- Trace generation.
- Policy evaluation.
- Value iteration and policy iteration.
- Q-learning and later learning algorithms.
- POMDP filtering and planning.
- Sparse and dense matrix lowering.
- Continuous sampling or quadrature.
- Tensor and GPU execution.

Each interpreter receives all behavior-changing configuration as an argument. This includes seeds, horizons, discounts, schedules, tolerances, iteration limits, and devices.

The exact reference interpreter implements Section 5.2 by bounded state recursion. It checks terminal status before the horizon boundary, decreases the transition count on every recursive call, and preserves rational arithmetic throughout. Its trace enumerator exposes the same branches and returns for independent expectation checks.

The sampled interpreter uses the same terminal-before-horizon and discount placement. It receives and returns opaque generator state. Floating support masses are converted to their exact binary rational values and sampled by rejection from unbiased generator bits, so every exposed positive entry remains reachable. A deterministic one-point distribution consumes no generator state.

An interpreter can cache or compile a model. The cache and compiler are not part of the model denotation.

## 11. Module and package boundaries

### 11.1 Initial module map

The implementation uses these boundaries. Entries marked "later" are not implemented:

```text
Markovian.Compile.Exact     validated finite indexes and exact policy compilation
Markovian.Probability       opaque floating probability and distribution types
Markovian.Probability.Exact exact rational probability and distribution types
Markovian.Reward            floating reward and terminal-payoff values
Markovian.Reward.Exact      exact rational reward values
Markovian.Horizon           unbounded validated transition horizons
Markovian.Objective         floating discount and finite objective values
Markovian.Objective.Exact   exact rational discount and finite objective values
Markovian.Kernel            one-layer floating stochastic kernel interface
Markovian.Kernel.Exact      exact rational kernel and Kleisli composition
Markovian.MRP               MRP interface
Markovian.MDP               MDP, unique action ID, and outcome interfaces
Markovian.MDP.Exact         exact MDP, status, outcome, and model errors
Markovian.Policy            floating policy validation and fallible closure
Markovian.Policy.Exact      exact policy, support validation, and closure
Markovian.POMDP             later finite POMDP interface
Markovian.Sampling          explicit generator and finite categorical sampling
Markovian.Trace             generic action-labeled bounded traces
Markovian.Interpreter.Exact bounded exact expectation and trace enumeration
Markovian.Interpreter.Sampled seeded floating finite sampling and traces
Markovian.Interpreter.DynamicProgramming.Exact exact finite-horizon backups
Markovian.Interpreter.Bellman.Exact exact contraction policy evaluation
Markovian.Learning.QLearning validated Q-values, configuration, and pure update
Markovian.Learning.QLearning.Episodic seeded bounded epsilon-greedy learning
```

Internal representations use `Markovian.Internal.*`. The package does not expose those modules.

Learning modules depend on model and interpreter modules. Model modules do not depend on learning modules. Applications contain examples only. Tests contain executable contracts.

### 11.2 Future package map

Split packages only when dependency or release pressure justifies the cost.

```text
markovian-core              semantic values, finite kernels, models, policies
markovian-interpreters      exact, sample, trace, and Bellman interpreters
markovian-learning          tabular learning algorithms
markovian-pomdp             POMDP filtering and planning
markovian-continuous        experimental continuous kernels
markovian-compiler          typed categorical IR and lowering
markovian-hasktorch         neural and GPU backend
markovian-accelerate        batched finite array backend
markovian-monad-bayes       optional sampling adapter
markovian-horde-ad          research autodiff backend
```

`markovian-core` has no dependency on any other package in this list. Backend packages depend inward on stable semantic interfaces.

### 11.3 Public API policy

The package is unreleased and experimental. Correctness changes can replace exposed interfaces immediately. A future stability declaration requires law tests, interpreter agreement, PVP policy, and release criteria.

A release change needs README, changelog, and source-distribution review.

## 12. GPU, tensor, and autodiff boundary

A tensor backend lowers only a supported finite, batched, shape-known subset. It does not change the mathematical model.

The lowering boundary contains:

- Stable state and action indexes.
- Dense or sparse transition data.
- Batched rewards and terminal masks.
- Explicit numeric precision.
- Explicit device and layout configuration.
- Explicit RNG semantics for stochastic execution.

The semantic core must not mention `Tensor`, device, stream, memory layout, fusion, or transfer policy.

A backend report must distinguish:

- Pathwise equality.
- Seeded reproducibility.
- Equality in distribution.
- Approximate numeric agreement.

GPU benchmarks include compilation, host-to-device transfer, device execution, and device-to-host transfer. Reports must not quote kernel time as total application time.

Autodiff belongs to a backend. A gradient of an expectation needs assumptions that justify differentiation under the expectation.

## 13. Neural model denotations

A deterministic neural network denotes a parameterized deterministic function. As a kernel, it uses a Dirac distribution.

A stochastic policy network denotes a parameterized policy kernel after validation of action support and normalized probabilities.

A stochastic transition network denotes a parameterized transition outcome kernel after validation of rewards, support, and normalization.

A learned transition model approximates an environment model. It is not another exact interpreter of the same MDP.

A value network approximates a value function. A policy network approximates or parameterizes a policy. Neither is the denotation of a solved value without an error relation.

Every neural backend must define:

- Parameter and input domains.
- Output normalization.
- Approximation or calibration metrics.
- Estimator bias and variance.
- Gradient assumptions.
- Device precision and reproducibility.
- Failure behavior for NaN, infinity, or invalid support.

Training APIs come after these denotations and error contracts.

## 14. Categorical compiler IR

D-009 defers compiler implementation. D-018 is only proposed and authorizes no work.

An accepted D-018 must supersede D-009 for compiler IR work. The target below applies only after that gate.

A compiler is optional and follows stable model semantics.

The source language should be a typed free syntax with explicit primitives. The IR should preserve:

- Identity.
- Composition.
- Tensor product.
- Copy.
- Discard.
- Deterministic functions.
- Named kernel application.
- Explicit sample binding.
- Reward and trace annotations where required.

Use an administrative-normal form or typed SSA form to make sample sharing explicit. One binding copied twice means one random draw. Two kernel applications mean two random draws.

Backends interpret the same IR into exact finite, sampling, matrix, or tensor programs. Unsupported primitives produce typed compile errors.

Compiler tests must check structure preservation and observational equivalence. Optimizer tests must include random-sharing counterexamples.

## 15. Guidance for advanced abstractions

### 15.1 Recursion schemes

Use a recursion scheme only for actual recursive model syntax, such as a bounded-horizon program DSL.

The base functor must contain recursive positions as its type parameter. The coalgebra must produce one layer. The algebra must use the supplied recursive result.

A state graph with a self-loop is not an initial-algebra tree. Evaluate it with a horizon or fixed point.

Admission requires a termination or productivity argument and equivalence tests against a direct interpreter.

### 15.2 Kan extensions

A Kan-extension claim is a theorem about a compiler construction. It is not an API decoration.

The claim must name:

- The source category.
- The target category.
- The generating functor.
- The extension.
- The natural transformation.
- The universal property.

Do not merge the claim without a derivation and tests for the preserved structure.

### 15.3 Codensity

Codensity can reassociate bind-heavy probability or free-program representations. Keep it internal.

Admission requires semantic-equivalence tests, allocation measurements, and runtime benchmarks on a representative bind-heavy workload.

### 15.4 Cayley forms

A Cayley form can improve accumulation of traces, rewards, sparse updates, or other monoidal values. Keep it internal.

Floating-point addition is not associative. An optimization must state its numeric effect and pass tolerance-based differential tests.

Admission requires an identified accumulation bottleneck and benchmark evidence.

### 15.5 Normalization by evaluation

NBE requires a typed source DSL with clear normal forms. It can normalize deterministic and finite-distribution fragments.

NBE must not sample during normalization. It must residualize unsupported neural, continuous, or recursive primitives.

Admission requires soundness, reification correctness, a termination boundary, and code-size benchmarks.

### 15.6 Diagonalization

The categorical diagonal is the copy operation. It duplicates a value, not a stochastic computation.

Matrix diagonalization is a solver optimization for suitable finite transition operators. It is not a general semantic foundation.

A matrix backend can use eigendecomposition only after it checks the required matrix conditions. It must report conditioning, residuals, and numeric error.

Admission requires comparison with a direct or iterative reference solver. A benchmark must show a benefit on representative models.

## 16. External package recommendations

These are candidates, not current dependencies. A recommendation does not authorize a Cabal change.

| Package | Recommendation | Boundary | Source |
| --- | --- | --- | --- |
| Hasktorch | Primary neural and GPU candidate | Separate neural backend | [Hackage](https://hackage.haskell.org/package/hasktorch) and [source repository](https://github.com/hasktorch/hasktorch) |
| Accelerate | Candidate for batched finite array work | Separate array backend | [Hackage](https://hackage.haskell.org/package/accelerate) and [source repository](https://github.com/AccelerateHS/accelerate) |
| monad-bayes | Optional sampling interpreter | Adapter outside the core | [Hackage](https://hackage.haskell.org/package/monad-bayes) and [source repository](https://github.com/tweag/monad-bayes) |
| horde-ad | Research autodiff backend | Experimental package | [Hackage](https://hackage.haskell.org/package/horde-ad) and [source repository](https://github.com/Mikolaj/horde-ad) |

Before adding a dependency, record its purpose, license, maintenance state, lower bound, upper bound, and tested compiler range.

Prefer `base` and `Data.List.NonEmpty` for the first value types. Use `containers` for indexed maps when direct imports require it.

The current sampler uses a package-owned SplitMix64 stream implemented with `base`. Add `random` only when interoperability supplies an owned use case that the explicit `Generator` API cannot meet.

## 17. Testing strategy

### 17.1 Test layers

1. Unit tests cover every smart-constructor boundary and structured error.
2. Law tests cover functors, finite distributions, kernels, policy closure, and compiler structure.
3. Example tests cover known values with exact arithmetic.
4. Differential tests compare optimized, floating, tensor, and GPU backends with a reference interpreter.
5. Seed tests cover pathwise reproducibility where the interpreter promises it.
6. Compile fixtures prevent README examples from drifting.
7. Integration tests cover source distributions and application entry points.

### 17.2 Required semantic cases

Tests must include:

- Empty, negative, zero-total, NaN, and infinite distribution inputs.
- Floating weights whose direct sum overflows.
- Extreme finite weights whose smaller normalized mass rounds to zero.
- The scaled-total proof and defensive error branch described by D-026.
- Finite and non-finite rewards.
- Terminal initial states.
- Empty nonterminal action sets.
- Duplicate and unavailable action IDs.
- Horizon zero and self-loops.
- Discount placement and terminal payoff timing.
- One shared random draw compared with two random draws.
- Zero-evidence POMDP observations.
- Equal-seed interpreter runs.
- Bellman residual and error bounds.

Do not use frequency thresholds as required CI gates. They are flaky and weak for exact support contracts.

### 17.3 Reference values

The exact sample has one transition reward `2`, discount `1/2`, and terminal payoff `7`. Its finite-horizon expected return is `11/2`.

The randomized exact policy fixture chooses rewards `2` and `8` with masses `1/4` and `3/4`. Its conditional expected reward is `13/2`.

## 18. CI and release gates

The pinned local environment and hosted CI run these commands. Every completion claim records output from the current revision.

Every change runs:

```sh
cabal check
cabal build all --project-file=cabal.project.ci
cabal test all --project-file=cabal.project.ci --test-show-details=direct
```

CI also needs these gates after bootstrap:

```sh
hlint src
fourmolu --mode check $(git ls-files '*.hs' '*.lhs')
cabal-fmt --check Markovian.cabal
cabal build all --prefer-oldest --project-file=cabal.project.ci
cabal test all --prefer-oldest --project-file=cabal.project.ci
cabal haddock all --project-file=cabal.project.ci \
  --enable-documentation --haddock-all --haddock-hyperlink-source
```

The source-distribution job runs `cabal check`, creates an archive, unpacks it, then builds and tests the unpacked tree.

The compiler matrix tests GHC 9.4.8 and 9.8.4. Add more compilers only after their package bounds and full gates pass.

Pin GitHub Actions by commit SHA. Pin formatter versions. Change dependency pins in separate maintenance changes.

A semantic change also requires an accepted decision, updated invariants, and deterministic contract tests.

A public API or release change also requires README updates and a factual changelog entry.

## 19. Architecture compliance

A review fails when source, package metadata, tests, and durable documents disagree.

No file has silent precedence over another. Resolve disagreement with an explicit decision and a coordinated change.
