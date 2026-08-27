# Target architecture

This document defines the implemented and target architecture for Markovian. Implemented boundaries require compiler, test, Haddock, and package evidence. Stages S1 through S5 are committed and have hosted evidence. `docs/CONTEXT.md` records revision-specific local and hosted evidence.

`docs/DECISIONS.md` records why the project selected these boundaries. `TODO.md` controls delivery order and completion status.

## 1. Scope

Markovian will provide typed semantics and interpreters for stochastic processes and decision models.

The first supported domain is finite state, finite action, and finite support. The first evaluators use a finite horizon.

The package includes an exact discounted Bellman policy evaluator and finite post-transition POMDP filtering and bounded planning. Continuous-kernel experiments remain future work and must pass their admission gate.

The project does not put tensors, devices, autodiff, neural networks, or sampling frameworks in the semantic core.

### 1.1 Implemented boundary

The root package currently contains:

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
- Canonical exact finite beliefs, post-transition filtering, and bounded belief planning.
- Typed exact finite categorical syntax with explicit copy and independent tensor.
- Duplicate-free finite sets, including empty sets, and nonempty finite-object refinements.
- Law-documented semiring, involution, exact positivity, and convex scalar contracts.
- Opaque finite semiring matrices with semantic reindexing, tensor, biproduct, dagger, compact structure, and trace.
- Exact nonnegative stochastic matrices, proof-carrying deterministic matrices, and exact convex mixtures.
- Dense row-major rational CPU lowering with exact denotational differential tests.

The current `FiniteDist` constructor preserves labeled duplicate entries. It removes input zero weights and positive weights whose normalized `Double` mass rounds to zero. Floating constructors canonicalize negative zero. Optional CUDA execution and neural categorical contracts live in separate backend packages.

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

`ExactFiniteDist` is a finite rational probability monad. It has lawful `Functor`, `Applicative`, `Monad`, `Foldable`, and `Traversable` instances. These instances preserve labeled support order and duplicate labels.

`ExactKernel` is the Kleisli category of that monad. Its `Category`, `Arrow`, and `ArrowChoice` instances provide standard composition, product, fanout, and branch combinators.

The finite IR represents a symmetric monoidal Markov fragment with explicit object witnesses:

- Identity returns its input through a Dirac kernel.
- Composition uses Kleisli composition.
- Tensor combines independent kernel executions.
- Swap, associators, and unitors provide product coherence maps.
- Copy maps an object into its full tensor square with diagonal support.
- Discard maps an object into the singleton unit object.
- Fanout copies one input before two conditionally independent kernel executions.

The IR cannot use the standard `Category` instance directly. A category identity cannot synthesize the required `FiniteObject` witness. Smart constructors retain these witnesses and reject value-level object mismatches.

Copy is natural for deterministic morphisms. It is not natural for arbitrary stochastic morphisms. Copying one sampled value differs from executing the sampling kernel twice.

Finite objects do not have `Functor` or `Traversable` instances. An arbitrary value map can introduce duplicates and break their validated invariant.

Conditioning and Bayesian inversion are not total category operations. POMDP belief updates expose their normalization and zero-evidence failures.

### 3.5 Exact finite matrix foundation

Raw finite matrices use explicit duplicate-free `FiniteSet` witnesses. A finite set can be empty. `FiniteObject` is its nonempty probability-bearing refinement. Semantic support comparison ignores layout order. Each exposed finite-witness module exports `sameFiniteLayout` as its canonical layout operation. `sameFiniteSetLayout` and `sameFiniteObjectLayout` are descriptive aliases with identical behavior. These operations and ordinary `Eq` preserve represented order.

The scalar hierarchy does not use `Num` as its law contract. `Semiring` provides zero, one, addition, and multiplication. Commutativity, involution, exact nonnegative division, and exact convex validation are separate capabilities. `NonNegativeRational` is the only implemented exact nonnegative semifield. It permits values above one and rejects negative values.

`Matrix scalar source target` is opaque and stores checked source-by-target rows. Construction uses a total value-indexed function or checked rows. Indexing returns `Maybe`. Composition checks semantic middle support and reindexes by labels. Matrix equivalence ignores layout, while layout equivalence compares witnesses and row-major entries.

Raw commutative-semiring matrices provide pointwise addition, Kronecker tensor, disjoint-sum biproducts, transpose, conjugate transpose, basis cups and caps, and explicit-object categorical trace. Tensor and biproduct are distinct operations. Trace is not exposed on normalized kernels.

`StochasticMatrix` validates every row against exact one. Its public validating constructor is limited to `NonNegativeRational`. Identity, composition, tensor, copy, and discard preserve normalization by construction. It has no transpose, dagger, compact, trace, or raw-addition API. Transposing the one-row fair coin produces two rows of mass one-half and is the required normalization counterexample.

Stochastic endpoints use `FiniteSet` because the empty-to-empty normalized arrow is valid by vacuity. A normalized arrow from a nonempty source into an empty target fails validation. Normalized states, priors, distributions, and the existing probability interfaces use `FiniteObject` and remain nonempty.

`StochasticMatrix`, `DeterministicMatrix`, and exact convex families assign nominal roles to their scalar and endpoint parameters. External `coerce` calls cannot replace the scalar laws or endpoint equality evidence. Convex coefficient validation is a fixed generic sum-to-one operation, not an overridable instance predicate.

`DeterministicMatrix` validates one-hot rows or a total represented finite function. Its forgetful embedding into `StochasticMatrix` is total. Only this proof-carrying type can authorize copy-naturality reasoning; a stochastic primitive remains stochastic even when its entries happen to be Dirac. Exact convex families are nonempty, use nonnegative coefficients summing exactly to one, and preserve stochastic normalization. Composition and tensor are separately affine, not jointly affine over correlated operand pairs.

S1 does not compare transpose with Bayesian inversion because S2 owns priors, positive support, exact division, and zero-evidence errors. S1 exact-law-tests a raw transpose normalization counterexample. S2 defines prior-indexed Bayesian inversion as a separate normalized operation on restricted support and exact-law-tests its stated fixtures.

### 3.6 Exact Bayesian structure

A `Prior a` is a normalized state from the singleton object to an explicit nonempty finite object. Construction accepts exact rational masses, aggregates duplicate labels extensionally, rejects negative or outside labels, and requires total mass one. Each prior stores a positive `Support a` in parent-object layout order.

Pushforward composes a prior with a stochastic matrix. Joint construction has mass `p(x) K(x,y)`. Evidence is the pushforward mass of one represented observation. Conditioning returns a `Posterior` and returns structured `ZeroEvidence` for a represented zero-mass observation. An observation outside the represented target has a separate error.

For prior `p` and channel `K`, Bayesian inversion first computes `q = p;K`. It then restricts to `X_p` and `Y_q`. The inverse has entries `p(x) K(x,y) / q(y)` and type `Y_q -> X_p`. No API fills rows outside `Y_q`.

`almostSureEqual p F G` checks `p(x)F(x,z) = p(x)G(x,z)` for every represented label. It is an explicit equivalence predicate. Postcomposition preserves it. Precomposition requires a transported prior.

A `BayesianChannel` stores an input prior, forward stochastic matrix, and exact output prior. Composition verifies prior flow before it composes forward matrices. It has no plain `Category` or dagger instance. Matrix conjugate transpose remains unrelated.

The generic exact distribution bridge in `Markovian.Bayesian.Exact` validates each raw weight before duplicate aggregation. It then owns canonical aggregation, pushforward, and conditioning for compatibility APIs. `Markovian.POMDP.Exact` delegates to it. The POMDP interface still observes after transition and preserves its existing errors and support order.

### 3.7 Purity-indexed stochastic circuits

`Circuit primitive purity source target` is opaque recursive syntax over a higher-kinded primitive signature. Its purity index records construction provenance. It does not inspect the denotation. Deterministic primitives and validated finite tables retain deterministic provenance. Stochastic primitives, nontrivial compositions with stochastic terms, and convex choice retain stochastic provenance. `weakenPurity` is the only cast.

Structural nodes remain visible in the syntax: identity, composition, tensor, symmetry, associators, unitors, copy, and discard. Convex choice stores an exact checked coefficient family. `shareCircuit circuit` executes the circuit once and copies its output. `fanoutCircuit left right` copies the input and executes two branches independently conditional on that input. There is no Haskell-function binder.

`copyNaturalDeterministic` implements the copy-naturality rewrite only for deterministic syntax. Compile-fail tests reject its use on stochastic syntax and reject purity strengthening. A stochastic primitive remains stochastic even if one interpreter currently returns a Dirac kernel.

`CircuitAlgebra` lists unchecked fold operations. It is not evidence of categorical or convex laws. `foldCircuit` derives sharing as composition followed by copy. It derives fanout as copy followed by tensor and composition, so interpreters cannot assign independent meanings to those nodes. The exact algebra requires deterministic primitive interpretations to return `DeterministicMatrix NonNegativeRational` and stochastic primitive interpretations to return `StochasticMatrix NonNegativeRational`. It checks primitive endpoint layouts and evaluates convex choice through exact convex enrichment. Selected fixtures exact-law-test the stated operation-preservation and coherence equations for this algebra only; they do not establish a universal theorem.

`lowerExactCircuit` converts the same exact denotation to the existing source-by-target row-major `DenseExactKernel`. Dense compatibility storage requires nonempty endpoints; the raw circuit AST and matrix layers still permit empty finite boundaries. Differential tests compare circuit sharing and fanout with both `denoteExactIR` and `lowerExactIR`.

`ApproximateInterpreterBoundary` is separate from the exact algebra. A floating, GPU, or neural implementation must provide an observational relation, precision in bits, and an error policy. This record does not establish exact categorical equality.

The Conal-style deterministic source language supports identity, composition, products, pairing, projections, and finite quoted primitive tables. Compilation maps composition to left-to-right circuit composition, products to tensor, pairing to copy followed by tensor, and projections to discard followed by a unitor. The compiler module does not depend on the exact interpreter; its vacuous primitive interpreter exists only in test support. The source and compiler contain no arbitrary Haskell function values. Bottoms, exceptions, `seq`, opaque higher-order functions, infinite types, exponentials, recursive term nodes, and stochastic cartesian closure are excluded.

### 3.8 Typed structured cospans and open circuit decorations

`Interface sort port` is a finite typed port table and can be empty. `InterfaceMap` is a validated total type-preserving table. Tensor is disjoint union. The discrete-boundary functor maps an interface to a hypergraph with the same typed vertices and no edges, and maps vertical arrows to edge-empty hypergraph maps.

`TypedHypergraph sort vertex edge label` stores duplicate-free typed vertices, duplicate-free edge identities, and directed labelled hyperedges. Each edge has ordered typed inputs and outputs. A `HypergraphMap` is total on vertices and edges and preserves vertex types, labels, port order, and incidence.

`finitePushout` computes the equivalence closure generated by a span of interface maps. Its carrier consists of opaque `PushoutPoint` quotient classes over the disjoint union. Class members are canonicalized in left-carrier order followed by right-carrier order, independent of relation-discovery order. The result exposes all classes and both canonical injections. `factorPushout` compares cocone objects by typed support, not layout, validates every quotient class, and returns the unique table factor in the selected target representation. Construction has no partial representative selection. `FinitePushout` has nominal roles for all witness parameters.

`OpenSystem` is a structured cospan from two discrete interfaces into one typed hypergraph. Legs are total and type preserving but can identify boundary ports. Sequential composition reindexes the common interface, pushes out apex vertices, remaps ordered edge ports, and combines edge identities by disjoint union. Tensor is disjoint union. The horizontal identity is the discrete cospan.

`OpenSystemCell` is a commuting square. It contains source and target open systems, two vertical interface maps, and a structure-preserving apex hypergraph map. Interface-map rows are stored in source-layout order, while `sameInterfaceMap` compares them extensionally. Vertical composition composes all three maps. Horizontal composition accepts extensionally equal middle maps and uses the induced map between pushouts. Tensor acts componentwise.

Binary pushout classes have the canonical representation described above. Nested left- and right-associated `PushoutPoint` types are not literally equal. The tests therefore flatten their canonical members to the original three tagged carriers and construct an explicit associator isomorphism. They check both vertex and edge maps in both round trips. The tests construct left and right unitor isomorphisms and perform the same checks. These are executable representative witnesses. The public API does not package general associator or unitor constructors, and the project does not claim a strict double category or a general bicategorical coherence theorem.

`reverseOpenBoundary` swaps cospan legs only. It does not reverse directed edges or dynamics. It is structured-cospan boundary reversal, not matrix conjugate transpose or prior-indexed Bayesian inversion, and it has no common `Dagger` instance.

`OpenCircuit` attaches a global directed circuit decoration to open topology. By construction, composition and tensor delegate the decoration component directly to circuit composition and tensor. Selected fixtures exact-law-test the resulting decoration equations. Hypergraph topology has no stochastic black-box denotation. A boundary-reversed circuit view exchanges only the topological input and output parameters. Its state input and output parameters retain the original directed orientation, and the view has no reverse-denotation observer. Cyclic graph semantics, feedback, arbitrary graph evaluation, continuous-time open-Markov black-boxing, and MDP black-box theorems remain outside the implementation.

### 3.9 Evidence classification and proof boundary

Opaque smart constructors and nominal roles establish their represented validation invariants by construction. Direct circuit-fold and open-decoration delegation is also by construction. Algebraic, categorical, Bayesian, compiler, and open-system equations are exact-law-tested on the stated finite fixtures unless a section explicitly says otherwise. Differential claims are fixture-based comparisons with an independent existing path.

The scalar classes cannot enforce their documented laws for third-party instances. Generic matrix, stochastic, deterministic, and convex closure therefore remains conditional on lawful scalar instances. The repository has no quantified or machine-checked theorem for all finite objects, all circuits, all compiler terms, all POMDPs, pushout universality, natural associator or unitor families, or pseudo-double-category coherence. Compiler soundness and finite pushout universality are algebraically argued and fixture-tested, not universally proved. `CircuitAlgebra` remains an unchecked operation record. The open API remains a double fragment and is not promoted to a double category.

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

The public exact POMDP interface observes after the transition. Prediction first marginalizes the latent transition. Conditioning then multiplies the predicted belief by the action-and-successor observation likelihood. An impossible observation returns `ImpossibleExactObservation` rather than normalizing zero evidence.

Exact beliefs aggregate duplicate latent states and normalize rational mass. Bounded belief planning requires each positive-mass continuing state to share the selected action. It rejects beliefs mixing terminal and continuing states. Terminal beliefs return the expected terminal payoff before the horizon boundary.

A belief-state planner is an interpreter construction. It is not the definition of a POMDP.

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
| Discounted continuing MDP | Exact contraction fixed-point policy evaluation is implemented. Control optimization remains separate future work. |
| Average-reward MDP | Deferred. It needs recurrence and gain or bias semantics. |
| Constrained MDP | Deferred. It needs separate cost signals and feasibility semantics. |
| Semi-Markov decision process | Deferred. It needs explicit duration and discount timing. |
| POMDP | Exact finite post-transition filtering and bounded belief-policy evaluation are implemented. |
| Multi-agent model | Out of the initial scope. It needs joint actions and equilibrium concepts. |
| Continuous-state MDP | Experimental package only after continuous-kernel admission. |

A variant does not enter the core through optional fields. It receives its own interface and objective contract.

## 10. Interpreter separation

The semantic core defines values, kernels, models, policies, and objectives. It performs validation but no execution-specific optimization.

Implemented interpreters provide:

- Exact finite expectation and trace enumeration.
- Seeded finite-support simulation.
- Exact finite-horizon dynamic programming and contraction policy evaluation.
- Validated Q-learning updates and bounded seeded episodes.
- Exact POMDP filtering and bounded belief-policy evaluation.
- Dense exact CPU lowering and optional dense GPU execution.

Control-optimizing value iteration or policy iteration, sparse lowering, and continuous sampling or quadrature remain future interpreter families.

Each interpreter receives all behavior-changing configuration as an argument. This includes seeds, horizons, discounts, schedules, tolerances, iteration limits, and devices.

The exact reference interpreter implements Section 5.2 by bounded state recursion. It checks terminal status before the horizon boundary, decreases the transition count on every recursive call, and preserves rational arithmetic throughout. Its trace enumerator exposes the same branches and returns for independent expectation checks.

The sampled interpreter uses the same terminal-before-horizon and discount placement. It receives and returns opaque generator state. Floating support masses are converted to their exact binary rational values and sampled by rejection from unbiased generator bits, so every exposed positive entry remains reachable. A deterministic one-point distribution consumes no generator state.

An interpreter can cache or compile a model. The cache and compiler are not part of the model denotation.

## 11. Module and package boundaries

### 11.1 Initial module map

The implementation uses these boundaries. Entries marked "later" are not implemented:

```text
Markovian.Algebra.Semiring law-bearing scalar capabilities
Markovian.Algebra.NonNegativeRational exact nonnegative scalar implementation
Markovian.Backend.CPU.Exact dense rational CPU lowering
Markovian.Bayesian.Exact exact priors, support, conditioning, and inversion
Markovian.Bayesian.Channel.Exact checked prior-flow channel composition
Markovian.Circuit raw purity-indexed stochastic-circuit AST and unchecked fold algebra
Markovian.Circuit.Compile.Deterministic first-order quoted-table compilation
Markovian.Circuit.Interpret.Exact exact matrix and kernel algebra
Markovian.Circuit.Interpret.Approximate floating and backend approximation boundary
Markovian.Open.Interface finite typed interfaces and vertical maps
Markovian.Open.Hypergraph finite typed hypergraphs and structure maps
Markovian.Open.Pushout explicit finite typed quotient pushouts
Markovian.Open.StructuredCospan open systems and commuting 2-cells
Markovian.Open.Circuit.Exact directed circuit-decorated open topology
Markovian.Category.Finite.Set duplicate-free finite sets, including empty sets
Markovian.Category.Finite.Object nonempty finite-object refinement
Markovian.Category.Finite.Exact typed exact categorical syntax and denotation
Markovian.Category.Matrix opaque finite semiring matrices
Markovian.Category.Matrix.Stochastic normalized exact nonnegative matrices
Markovian.Category.Matrix.Deterministic proof-carrying one-hot matrices
Markovian.Category.Convex.Exact exact convex families and mixtures
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
Markovian.POMDP.Exact       exact beliefs and post-transition filtering
Markovian.POMDP.Planning.Exact bounded exact belief-policy evaluation
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

### 11.2 Package map

The semantic implementation remains in the root package while hardware and framework contracts use separate packages.

```text
markovian-core              semantic values, finite kernels, models, policies
markovian-interpreters      exact, sample, trace, and Bellman interpreters
markovian-learning          tabular learning algorithms
markovian-pomdp             POMDP filtering and planning
markovian-continuous        experimental continuous kernels
markovian-compiler          typed categorical IR and lowering
markovian-gpu               optional CUDA driver backend (implemented)
markovian-neural            framework-independent neural contracts (implemented)
markovian-hasktorch         possible future tensor-framework adapter
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

The optional `markovian-gpu` package lowers row-major `Double` matrices through the CUDA driver API. Its package flag is off by default. The enabled path loads committed PTX, creates a context, transfers inputs, launches a dense kernel, transfers output, and releases resources. The reported duration includes every one of those operations.

The benchmark uses a 256-by-256 identity matrix and row-major `Double` values. It runs one excluded warmup and records 20 transfer-inclusive samples. It reports each sample, the mean, sample standard deviation, minimum, maximum, and maximum differential error.

The 2026-08-26 sample-bearing run used an NVIDIA GB10 with driver 580.173.02 and compute capability 12.1. The enabled differential command passed. The benchmark measured `267.843920400 ms` mean with `3.025869898 ms` sample standard deviation. Its range was `263.519087000 ms` to `276.777522000 ms`, and maximum error was `0.000e0`.

[The complete evidence record](evidence/CUDA-2026-08-26.md) contains the commands, tool versions, raw samples, PTX hashes, and revision context. It also retains four older mean-only measurements. Those historical values have no raw samples or dispersion, so they are execution records only.

This measurement shows local execution on one host. It is not a general performance claim.

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

The `markovian-neural` package implements stable-softmax normalization, the analytic softmax Jacobian, a score-function estimator contract with an explicit baseline flag, and max-norm comparison with exact rational categorical masses. It rejects empty, non-finite, out-of-range, and shape-mismatched values. It selects no tensor or autodiff framework.

Training APIs come after these denotations and error contracts.

## 14. Categorical compiler IR

D-035 supersedes the compiler deferral in D-009 and the unaccepted proposal in D-018 for the exact finite fragment.

The implemented category has duplicate-free finite objects and exact finite stochastic kernels. Typed syntax contains identity, primitive kernels, composition, tensor, symmetry, associators, unitors, copy, fanout, and discard. Exact denotation canonicalizes output mass in target-object order.

The typed source syntax preserves:

- Identity.
- Composition.
- Tensor product.
- Symmetry, associators, and unitors.
- Copy and shared-input fanout.
- Discard.
- Validated exact primitive kernels, including deterministic Dirac kernels.

`copyExactIR` targets the full tensor square. Its denotation assigns mass only to diagonal pairs. A stochastic expression followed by copy performs one draw. Fanout performs two conditionally independent draws from one shared input. An exact fixture demonstrates that these denotations differ.

The dense CPU backend lowers exact denotation into a source-by-target rational matrix. The optional CUDA package executes floating dense matrices after explicit conversion at the backend boundary. Unsupported future primitives require typed compile errors.

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
| Hasktorch | Possible future tensor-framework adapter | Separate neural backend | [Hackage](https://hackage.haskell.org/package/hasktorch) and [source repository](https://github.com/hasktorch/hasktorch) |
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
- Matrix category, tensor, biproduct, dagger, compact, and trace laws.
- Stochastic normalization, deterministic copy naturality, and exact convex laws.
- Exact Bayesian joint, support, identity, composition, tensor, double-inversion, and almost-sure laws.
- Exact circuit algebra, derived sharing and fanout, purity, convex choice, stated coherence equations, and deterministic compilation laws.
- Finite pushout, structured-cospan, associator-isomorphism, double-cell interchange, tensor, reversal, and decorated-denotation laws.
- Differential circuit fixtures against `denoteExactIR`, `lowerExactIR`, and dense exact CPU rows.
- POMDP differential fixtures for aggregation, support order, posterior values, zero evidence, and bounded planning.
- A concrete proof that transpose does not preserve row normalization.
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
fourmolu --mode check $(git ls-files '*.hs')
cabal-fmt --check Markovian.cabal \
  backends/markovian-gpu/markovian-gpu.cabal \
  backends/markovian-neural/markovian-neural.cabal
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
