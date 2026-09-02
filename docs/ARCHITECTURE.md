# Target architecture

This document defines the implemented and target architecture for Markovian. Implemented boundaries require compiler, test, Haddock, and package evidence. `docs/CONTEXT.md` records revision-specific local and hosted evidence.

`docs/DECISIONS.md` records why the project selected these boundaries. `TODO.md` controls delivery order and completion status.

## 1. Scope

Markovian will provide typed semantics and interpreters for stochastic processes and decision models.

The first supported domain is finite state, finite action, and finite support. The first evaluators use a finite horizon.

The package includes exact discounted policy evaluation and control. It also includes finite post-transition POMDP filtering and bounded planning. Continuous-kernel experiments remain future work and require the evidence listed in Section 15.

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
- Duplicate-free finite state and action indexes with policy-free exhaustive exact MDP compilation.
- Explicit labelled-support and represented-layout comparison for global action indexes.
- Separate compiled policy closure that produces a compiled MRP.
- Exact finite-horizon dynamic programming over compiled policy models.
- Exact discounted Bellman policy evaluation with sup-norm stopping bounds.
- Exact discounted value iteration with residual, value-error, and greedy-policy bounds.
- Exact deterministic policy iteration with signed rational linear solves.
- A synthetic bounded serial-inventory fixture, a source-crosswalked Clark--Scarf (1960) finite specialization, a source-crosswalked bounded Doğru physical-versus-balance-relaxed comparison, and a two-stage fixed-batch finite execution with separate stationary newsvendor evidence. Each has explicit finite layouts, budgets, deterministic reports, and separate timing output.
- Shared validated Q-tables, V-tables, rates, schedules, observations, and epsilon-greedy behavior.
- Pure TD(0), SARSA, Expected SARSA, and Q-learning updates.
- Bounded seeded and resumable episodic runners for all four tabular methods.
- Canonical exact finite beliefs, post-transition filtering, and bounded belief planning.
- Typed exact finite categorical syntax with explicit copy and independent tensor.
- Duplicate-free finite sets, including empty sets, and nonempty finite-object refinements.
- Law-documented semiring, involution, exact positivity, and convex scalar contracts.
- Opaque finite semiring matrices with semantic reindexing, tensor, biproduct, dagger, compact structure, and trace.
- Exact nonnegative stochastic matrices, proof-carrying deterministic matrices, and exact convex mixtures.
- Checked exact rational finite payoffs, contravariant payoff pullback, and exact state-payoff pairing.
- Finite alternating protocols plus a separate bounded owner-refined finite open-game fragment with exact pure contextual equilibrium enumeration.
- Exact owned normal-form mixtures, CE and CCE candidate checks, joint-outcome finite-horizon public-state stochastic games, and one-shot correlated-prior Harsanyi checks.
- Checked finite feedback fragments: explicit one-tick delay, exact proper first exit, and nilpotent reward-duration-output closure.
- Dense row-major rational CPU lowering with exact denotational differential tests.

The current `FiniteDist` constructor preserves labeled duplicate entries. It removes input zero weights and positive weights whose normalized `Double` mass rounds to zero. Floating constructors canonicalize negative zero. Optional CUDA execution and neural numerical updates live in separate backend packages.

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
10. Add advanced abstractions only when laws, use cases, and benchmarks justify them.

## 3. Semantic foundation

### 3.1 Stochastic kernels

A stochastic kernel from `X` to `Y` maps each input in `X` to a distribution over `Y`.

```text
K : X -> D(Y)
```

For the first implementation, `D` is a validated finite distribution. A later continuous package can use a measure-backed representation.

The kernel interface must not expose a raw vector of weights. Constructors validate the representation before a kernel can return it.

### 3.2 Checked exact sequencing

Validated exact distributions support bounded stochastic sequencing. Exact kernels compose only when the caller supplies checked bind limits.

```text
identity(x) = dirac(x)

(L <=< K)(x)(z) = sum_y K(x)(y) * L(y)(z)
```

Checked bind preserves labeled duplicate entries and their deterministic left-to-right support order. Extensional consumers can aggregate equal labels explicitly; checked bind does not do so implicitly.

The exact reference interpreter uses exact arithmetic. It tests identity and associativity only when every compared computation is admitted.

A floating interpreter uses an explicit tolerance and observational equivalence. It must not claim literal floating-point associativity.

`bindExactFiniteDistChecked` preserves labeled duplicates and deterministic support order. It limits result support, work, numerator bits, and denominator bits. Failure returns no distribution or report.

Admission and reports can depend on association. Therefore, `ExactFiniteDist` has no `Applicative` or `Monad` instance. `ExactKernel` has no unrestricted `Category`, `Arrow`, or `ArrowChoice` instance.

The floating kernel remains one-layer only. A future composition contract must define underflow, normalization, and observational error.

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

### 3.4 Exact circuit structure

`ExactFiniteDist` has safe `Functor`, `Foldable`, and `Traversable` instances. Checked bind supplies bounded sequencing without a type-class law claim.

The public exact circuit and matrix modules represent a symmetric monoidal Markov fragment with explicit object witnesses:

- Identity returns its input through a Dirac kernel.
- Composition uses Kleisli composition.
- Tensor combines independent kernel executions.
- Swap, associators, and unitors provide product coherence maps.
- Copy maps an object into its full tensor square with diagonal support.
- Discard maps an object into the singleton unit object.
- Fanout copies one input before two conditionally independent kernel executions.

The circuit API retains finite endpoint witnesses. Smart constructors reject value-level endpoint mismatches.

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

`DeterministicMatrix` validates one-hot rows or a total represented finite function. Its forgetful embedding into `StochasticMatrix` is total. Copy-naturality reasoning requires this proof-carrying type. A stochastic primitive remains stochastic even when its entries happen to be Dirac. Exact convex families are nonempty, use nonnegative coefficients summing exactly to one, and preserve stochastic normalization. Composition and tensor are separately affine, not jointly affine over correlated operand pairs.

S1 does not compare transpose with Bayesian inversion because S2 owns priors, positive support, exact division, and zero-evidence errors. S1 exact-law-tests a raw transpose normalization counterexample. S2 defines prior-indexed Bayesian inversion as a separate normalized operation on restricted support and exact-law-tests its stated fixtures.

#### 3.5.1 Exact state-payoff push-pull

`ExactPayoff value` is an opaque total rational-valued function on an explicit `FiniteSet`. The checked table constructor rejects duplicate labels, labels outside the represented set, and missing labels. The function constructor is total by evaluation over the finite witness. Empty payoff sets are valid. Payoffs are signed and need not normalize.

For a normalized channel `K : X -> Y`, `pullbackPayoff K u` computes `x -> sum_y K(x,y) * u(y)`. It checks the channel target against the payoff object and returns a payoff on the channel source. Identity and composition are exact fixture laws; composition reverses because pullback is contravariant.

`pairStatePayoff` accepts a normalized state matrix with represented source `[()]` and a payoff on the same target. It computes `sum_x p(x) * u(x)` exactly. With the existing state pushforward, fixtures check `pair (pushforward p K) u = pair p (pullbackPayoff K u)`, including reordered layouts and signed payoffs.

This API uses `Rational` rather than the nonnegative scalar class because general payoffs can be negative and the current scalar hierarchy intentionally has no additive inverses. It does not generalize to an unsupported ring abstraction. Payoff pullback requires no prior, performs no support restriction or division, and remains separate from prior-indexed Bayesian inversion. This tranche does not refactor reward-bearing Bellman backups through the payoff API.

### 3.5.2 Checked finite feedback

Normalized stochastic matrices have no total trace. Feedback is exposed only through three checked exact fragments. Delayed feedback has an explicit seed state and one-tick body and executes for a bounded horizon. Proper first-exit feedback uses coproduct routing `X+U -> Y+U`, checks that every represented internal state reaches an exit, solves `H=C+DH` exactly, and validates output normalization. Timed feedback keeps reward, duration, and output joint and therefore requires a nilpotent continuation block.

Opaque nominal witnesses retain semantic loop ownership and endpoint types. Limits preflight represented dimensions, checked combined cardinalities, cells, graph work, and outcome counts. First-exit reachability uses a reverse queue. One finite matrix-power sequence supplies nilpotence and transience evidence.

An operation-wide meter charges each rational operation. Delayed and timed execution charge each represented branch before descent. Reports separate input, matrix-power, Gaussian, delayed-path, timed-path, other-intermediate, retained-result, and overall rational maxima. These maxima include discarded values. Atomic failure returns no semantic channel, checked witness, or partial report. It does not claim transactional heap rollback.

This layer is not matrix dagger, Bayesian inversion, payoff pullback, reverse differentiation, strategic duality, disintegration, a universal trace, a stationary selector, or cyclic open-system black-boxing.

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

`CircuitAlgebra` lists unchecked fold operations. It is not evidence of categorical or convex laws. `foldCircuit` derives sharing as composition followed by copy. It derives fanout as copy followed by tensor and composition, so interpreters cannot assign independent meanings to those nodes. `foldCircuitWithNodeLimit` charges each raw constructor before descent, traverses left to right, and separates node exhaustion from algebra failure. It returns no partial result. Primitive callbacks still own their internal termination and resource use.

The exact algebra requires deterministic primitive interpretations to return `DeterministicMatrix NonNegativeRational` and stochastic primitive interpretations to return `StochasticMatrix NonNegativeRational`. It checks primitive endpoint layouts and evaluates convex choice through exact convex enrichment. Selected fixtures exact-law-test the stated operation-preservation and coherence equations for this algebra only; they do not establish a universal theorem.

`Markovian.Circuit.Interpret.Cost` uses caller-owned primitive and table charges plus framework-owned structural counts. All counts and limits use `Natural`. Convex choice charges every represented branch, including a zero-coefficient branch. Owner totals retain first-occurrence order. The reported `maximumLiveLayoutCardinality` is only the maximum represented finite-object cardinality in the elaborated fold; it is not measured heap liveness, the acyclic interpreter's live frontier, runtime, or asymptotic complexity. Maximum represented matrix cells is separately bounded.

`Markovian.Circuit.Rewrite.Deterministic` constructs opaque candidates for identity removal, composition reassociation, and deterministic fanout-to-share deduplication. `Markovian.Circuit.Rewrite.Deterministic.Exact` issues an opaque witness only after bounded analysis, exact outer layout checks, exact one-hot matrix equivalence, and literal row-major matrix-layout equality. Cost comparison follows semantic checking and can report zero or negative improvement. Stochastic syntax cannot construct the deduplication candidate, even if one interpreter gives it a Dirac denotation.

`lowerExactCircuit` converts the same exact denotation to the existing source-by-target row-major `DenseExactKernel`. Dense compatibility storage requires nonempty endpoints; the raw circuit AST and matrix layers still permit empty finite boundaries. Differential tests compare circuit sharing and fanout with the private `denoteExactIR` regression oracle and the public `lowerExactCircuit` path.

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

`OpenCircuit` attaches a global directed circuit decoration to open topology. By construction, composition and tensor delegate the decoration component directly to circuit composition and tensor. Selected fixtures exact-law-test the resulting decoration equations. That API is unchanged by S6. A boundary-reversed circuit view exchanges only the topological input and output parameters. Its state input and output parameters retain the original directed orientation, and the view has no reverse-denotation observer.

`AcyclicOpenSystem` is a separate opaque refinement. Every apex vertex must have one producer: one injective input-boundary occurrence or one edge-output occurrence. Validation rejects missing, ambiguous, or repeated production before stable topological sorting. Stable cycle diagnostics contain an actual edge cycle. Multiple consumers, repeated edge inputs, noninjective output observation, discard, passthrough, nullary edges, disconnected components, and empty boundaries remain valid.

`FiniteValueDomains` supplies one finite value carrier for each runtime sort. `Assignment` stores named ports and values, compares extensionally, and is canonicalized in interface order. Edge assignment ports are positional integers. A label table is keyed by label and both ordered sort signatures, and endpoint support is checked before interpretation. Each edge occurrence executes its selected purity-indexed circuit once. Later consumers read the stored output, so sharing is not independent repetition.

`AcyclicOpenCircuit` records the join of represented edge purities. Exact interpretation first interprets each local circuit with edge context. It composes initialization, edge-step, and observation matrices in topological order. Each step retains only vertices needed by a later edge or output observation. It marginalizes all other edge outputs in that step. The final normalization check does not recover or renormalize. Noninjective output legs impose equality constraints.

The interpreter does not enumerate the complete apex assignment object. A bounded test computes the complete-valuation sum independently and compares it with the live-frontier result. Runtime cost remains exponential in the largest live frontier and in represented boundary size. S6 makes no general efficiency claim for wide DAGs.

The supported fragment provides exact identity, sequential composition after named middle-boundary reindexing, disjoint-union tensor, normalization, stored sharing, explicit discard, conditional-product diamonds, and ready-edge scheduling independence for successful denotations. Failures are diagnostics ordered by the stable topological schedule, so changing a valid schedule can change which failing edge is reported first. The semantic laws are finite fixture laws, not error-value laws or a machine-checked theorem. Cycles, trace, feedback, recursion, arbitrary graph evaluation, continuous-time open-Markov black-boxing, and unrestricted MDP black-boxing remain outside the implementation.

### 3.9 Evidence classification and proof boundary

Opaque smart constructors and nominal roles establish their represented validation invariants by construction. Direct circuit-fold and open-decoration delegation is also by construction. Algebraic, categorical, push-pull, Bayesian, compiler, and open-system equations are exact-law-tested on the stated finite fixtures unless a section explicitly says otherwise. Differential claims are fixture-based comparisons with an independent existing path.

The scalar classes cannot enforce their documented laws for third-party instances. Generic matrix, stochastic, deterministic, and convex closure therefore remains conditional on lawful scalar instances. The repository has no quantified or machine-checked theorem for all finite objects, all circuits, all finite DAG networks, all compiler terms, all POMDPs, pushout universality, natural associator or unitor families, or pseudo-double-category coherence. Compiler soundness and finite pushout universality are algebraically argued and fixture-tested, not universally proved. `CircuitAlgebra` remains an unchecked operation record. The open API remains a double fragment and is not promoted to a double category.

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

### 6.4 Tabular temporal-difference learning

The shared tabular layer keeps state-value and action-value tables distinct. Missing keys denote zero. The model remains free of tables, schedules, exploration, and generator state.

Every update uses `x' = x + alpha * (target - x)`. For one observed transition `(s,a,r,s')`, continuing targets are:

```text
TD(0)          r + gamma * V(s')
SARSA          r + gamma * Q(s',a')
Expected SARSA r + gamma * sum_a pi_epsilon(a | s') * Q(s',a)
Q-learning     r + gamma * max_a Q(s',a)
```

Every terminal target is `r + gamma * terminalPayoff(s')`. A terminal update does not request successor actions, a policy, epsilon, or a dummy next action.

TD(0), SARSA, and Expected SARSA use on-policy targets. Q-learning separates epsilon-greedy behavior from its greedy off-policy target. The pure root updates still inspect the MDP for terminal status and action-support validation.

The canonical epsilon-greedy distribution assigns `epsilon / |A(s)|` to every available action. It adds `1 - epsilon` to the first greedy action. Exact ties choose the first model-available action. Duplicate action support fails validation.

Each algorithm has a bounded episodic runner and a resumable runner. Resume state includes the table, episode index, global update count, and generator. SARSA selects its next behavior action before the current update and carries that exact action into the next step.

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

For control, the optimality operator maximizes exact action values over each state's available actions. The value-iteration report includes `delta / (1 - gamma)` and the greedy-policy bound `2 * gamma * delta / (1 - gamma)^2`.

Exact policy iteration selects the first available initial action, solves each fixed policy over signed rationals, and selects the first exact maximizer. Value and policy iteration both have explicit limits.

### 7.1 Bounded serial-inventory benchmark

The first inventory fixture is a synthetic bounded two-echelon model, not a source-verified named model. A state at the start of a period is `(t, u, a, i)`: periods remaining, upstream on-hand inventory, the supplier order due now, and downstream net inventory. An action is `(q, x)`, where `0 <= q <= orderCap` and `0 <= x <= u + a`.

After receiving `a`, the model ships `x`, observes conditioned bounded demand `d`, and moves to:

```text
u' = u + a - x
a' = q
i' = i + x - d
t' = t - 1
```

It charges

```text
h0 * u' + (h0 + h1) * max(i', 0) + p * max(-i', 0)
```

as a negative transition reward. A state with `t = 0` is terminal with zero payoff. Successors are neither clamped nor redirected. Breadth-first reachability closes the exact support before solving and enforces explicit small state and state-action budgets.

Demand starts from `P(D=d)=2^-(d+1)` and is conditioned on `0 <= d <= demandCap`. Exact probabilities, returns, costs, and regrets therefore apply only to this conditional bounded model. The report gives one-period retained and omitted mass and `1 - retainedMass^horizon`. That truncation probability is not a value-error bound.

The oracle uses exact finite-horizon backward induction over the decreasing `periodsRemaining` field, so the model accepts exact discount `1` without imposing a contraction solver boundary. Period-specific base-stock schedules are enumerated over duplicate-free, canonically ordered finite target sets. Supplier orders and internal shipments are clipped only at the action boundary; states are not clipped. Opaque solutions retain initial-state, model, target-grid, and solver provenance and derive redundant costs and regrets. Comparison returns a checked witness only after model equality, strict order-cap increase, period-wise target-set inclusion, actual grid widening, and completed solver status are established.

Undiscounted cyclic systems need separate properness conditions. The implementation must reject or isolate cases without a stated convergence contract.

### 7.2 Clark--Scarf 1960 finite specialization

The named inventory implementation is separate from the synthetic fixture. It follows Clark and Scarf (1960), Section III, pages 481--484. Its beginning-of-period state is `(t,x1,w1,x2)`, where `w1` is downstream stock in transit and `x2` is echelon-2 stock. Construction enforces `x2-x1-w1 >= 0`. An action selects an external order `z` and a downstream post-dispatch target `y` with `x1+w1 <= y <= x2`.

For one demand draw `D`, the successor is `(t-1,x1+w1-D,y-x1-w1,x2+z-D)`. The outcome stores this successor with the realized equation-(1) holding and shortage cost for the same `D`. Thus reward and successor correlation is not reconstructed from independent marginals. External orders augment echelon 2 next period; the new downstream dispatch remains in transit while old `w1` reaches downstream.

The implementation replaces source integrals with exact finite rational sums. The integer lattice, conditioned finite demand, retained-mass diagnostic, order and isolated-target caps, and all computation budgets are repository assumptions. Reachability is complete within those bounds. There is no state clamping or successor redirection.

The oracle compares, at every represented reachable state, generic exact-MDP backward induction, a direct equation-(14) calculation, and the equations-(15)/(20)/(21)/(26) decomposition. It evaluates the decomposed policy in the joint model and requires literal rational equality and zero regret. A checked widened-cap comparison requires strict widening and reports whether the widened policy selects either boundary. This remains finite diagnostic evidence, not an unbounded proof.

### 7.3 Fixed-batch two-stage execution and stationary evidence

`Markovian.Benchmark.Inventory.Serial.FixedBatch.Exact` specializes the source serial system to `L1=0`, positive `L2`, and `Q2=n*Q1`. A validated state is `(t,x1,x2,pipeline)`. The pipeline has exactly `L2` batch-congruent entries and its head is due after the current ordering decision. A physical action releases a `Q1` multiple no larger than `x2+a0` and places an external `Q2` multiple under an explicit batch cap.

One demand draw determines both the successor and its realized exact cost:

```text
x2' = x2 + a0 - q1
x1' = x1 + q1 - D
pipeline' = tail pipeline ++ [q2]
cost = h2*x2' + (h1+h2)*max(x1',0) + p*max(-x1',0)
```

Reachability is complete for the bounded action and conditioned-demand system. State, state-action, solver-work, reorder-grid, lead-time, demand-outcome, and convolution limits are explicit. No successor is clamped or redirected. Terminal pipeline has no salvage.

`Markovian.Benchmark.Inventory.Serial.FixedBatch.Newsvendor.Exact` is separate. It implements finite exact sums for the stationary subsystem shortfalls, equation-(9) costs, equation-(13)/(14) forward differences, and Theorem 1's weak and strict discrete inequalities. Its search domain has separate finite `R1` and `R2` layouts with declared Cartesian-product semantics. The solver memoizes each required shortfall law, charges every generated law term once plus one lead-demand convolution, and rejects the complete charge before construction. A solution retains its parameter and domain provenance. The report checks that provenance against its associated execution fixture and renders a separate checked finite/stationary selection counterexample. This does not transfer Chen's infinite-horizon optimality result to the finite-horizon oracle or implement the continuous equality in Theorem 2.

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

The optional continuous packages implement that first bounded experiment. `markovian-continuous` represents rational affine combinations of compact rational uniform noises with explicit owners. It executes rational polynomial moments, affine pushforwards, checked affine additive-noise kernel composition, and finite affine-likelihood conditioning only at positive evidence. Raw list traversal is bounded before canonicalization, renaming is injective, posterior arithmetic is rationally checked, and finite disintegration shares one work account across rows. `markovian-continuous-numerical` owns explicit rational-to-`Double` conversion, deterministic bounded GK15/7 quadrature, SplitMix64 sampling, and resumable Welford Monte Carlo. Numerical interval admission checks finite positive width, and quadrature rejects nonfinite aggregate estimates, aggregate errors, and tolerance thresholds.

This is not a general continuous model layer. It provides no arbitrary measurable callback, event oracle, point conditioning, continuous-to-continuous disintegration, certified numerical enclosure, or continuous MDP interpreter. Joint laws preserve shared noise; separate marginals may not replace a joint reward-successor-observation outcome.

## 9. MDP variants

The architecture treats each variant as an explicit extension.

| Variant | Status and boundary |
| --- | --- |
| Finite episodic MDP | First supported MDP. It uses a finite horizon. |
| Discounted continuing MDP | Exact contraction policy evaluation, value iteration, and deterministic policy iteration are implemented for finite models. |
| Average-reward MDP | Deferred. It needs recurrence and gain or bias semantics. |
| Constrained MDP | Deferred. It needs separate cost signals and feasibility semantics. |
| Semi-Markov decision process | Deferred. It needs explicit duration and discount timing. |
| POMDP | Exact finite post-transition filtering and bounded belief-policy evaluation are implemented. |
| Multi-agent model | Out of the initial scope. It needs joint actions and equilibrium concepts. |
| Continuous-state MDP | Experimental package only after the continuous-kernel requirements are met. |

A variant does not enter the core through optional fields. It receives its own interface and objective contract.

## 10. Interpreter separation

The semantic core defines values, kernels, models, policies, and objectives. It performs validation but no execution-specific optimization.

Implemented interpreters provide:

- Exact finite expectation and trace enumeration.
- Seeded finite-support simulation.
- Exact finite-horizon dynamic programming and contraction policy evaluation.
- Exact bounded value iteration, greedy extraction, and rational policy iteration.
- Pure TD(0), SARSA, Expected SARSA, and Q-learning updates.
- Bounded seeded and resumable tabular episodes.
- Exact POMDP filtering and bounded belief-policy evaluation.
- Dense exact CPU lowering and optional dense GPU execution.

Sparse lowering remains a future interpreter family. Restricted continuous quadrature and sampling live only in the optional continuous numerical package.

Each interpreter receives all behavior-changing configuration as an argument. This includes seeds, horizons, discounts, schedules, tolerances, iteration limits, and devices.

The exact reference interpreter implements Section 5.2 by bounded state recursion. It checks terminal status before the horizon boundary, decreases the transition count on every recursive call, and preserves rational arithmetic throughout. Its trace enumerator exposes the same branches and returns for independent expectation checks.

The sampled interpreter uses the same terminal-before-horizon and discount placement. It receives and returns opaque generator state. Floating support masses are converted to their exact binary rational values and sampled by rejection from unbiased generator bits, so every exposed positive entry remains reachable. A deterministic one-point distribution consumes no generator state.

The shared sampled MDP step validates one selected action and samples its joint reward-successor outcome. Episodic tabular runners use this operation and return the successor generator.

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
Markovian.Benchmark.Inventory.Serial.Exact bounded synthetic serial model, exact oracle, and base-stock comparison
Markovian.Benchmark.Inventory.Report deterministic conditional-model and widened-bound report
Markovian.Benchmark.Inventory.ClarkScarf1960.Finite.Exact exact finite Section III state, timing, transitions, and layouts
Markovian.Benchmark.Inventory.ClarkScarf1960.Oracle.Exact equations (14), (15), (20), (21), and (26) differential oracle
Markovian.Benchmark.Inventory.ClarkScarf1960.Report deterministic cited finite-bound report
Markovian.Benchmark.Inventory.Distribution.Dogru.Exact bounded physical and signed-relaxed Doğru adaptation
Markovian.Benchmark.Inventory.Distribution.Dogru.Report deterministic cited physical-versus-relaxed report
Markovian.Benchmark.Inventory.Serial.FixedBatch.Exact bounded two-stage physical execution and finite-horizon oracle
Markovian.Benchmark.Inventory.Serial.FixedBatch.Newsvendor.Exact separate exact stationary subsystem evidence
Markovian.Benchmark.Inventory.Serial.FixedBatch.Report deterministic execution, widening, truncation, and stationary report
Markovian.Circuit raw purity-indexed stochastic-circuit AST and unchecked fold algebra
Markovian.Circuit.Compile.Deterministic first-order quoted-table compilation
Markovian.Circuit.Interpret.Exact exact matrix and bounded deterministic algebra
Markovian.Circuit.Interpret.Cost bounded caller-owned static accounting
Markovian.Circuit.Interpret.Approximate floating and backend approximation boundary
Markovian.Circuit.Rewrite.Deterministic opaque deterministic rewrite candidates
Markovian.Circuit.Rewrite.Deterministic.Exact bounded exact rewrite witnesses and post-check costs
Markovian.Game.Arena bounded finite alternating arenas and legal-history replay
Markovian.Game.Strategy bounded receptive strategies and partial hidden-middle composition
Markovian.Game.Optic.Finite bounded finite functions, bijections, and concrete optics
Markovian.Game.Open.Finite owner-refined finite open games and checked structural equality
Markovian.Game.Open.Exact exact rational decisions, contexts, and equilibrium reports
Markovian.Open.Interface finite typed interfaces and vertical maps
Markovian.Open.Hypergraph finite typed hypergraphs and structure maps
Markovian.Open.Pushout explicit finite typed quotient pushouts
Markovian.Open.StructuredCospan open systems and commuting 2-cells
Markovian.Open.Circuit.Exact directed circuit-decorated open topology
Markovian.Open.Acyclic opaque unique-production finite DAG refinement
Markovian.Open.Acyclic.Circuit.Exact named assignments and exact local-circuit DAG semantics
Markovian.Category.Finite.Set duplicate-free finite sets, including empty sets
Markovian.Category.Finite.Object nonempty finite-object refinement
Markovian.Category.Matrix opaque finite semiring matrices
Markovian.Category.Matrix.Stochastic normalized exact nonnegative matrices
Markovian.Category.Matrix.Deterministic proof-carrying one-hot matrices
Markovian.Category.Convex.Exact exact convex families and mixtures
Markovian.Category.Payoff.Exact checked signed rational payoffs, pullback, and state pairing
Markovian.Compile.Exact     policy-free exact MDP compilation, action layouts, and compiled policy closure
Markovian.Probability       opaque floating probability and distribution types
Markovian.Probability.Exact exact rational probability and distribution types
Markovian.Reward            floating reward and terminal-payoff values
Markovian.Reward.Exact      exact rational reward values
Markovian.Horizon           unbounded validated transition horizons
Markovian.Objective         floating discount and finite objective values
Markovian.Objective.Exact   exact rational discount and finite objective values
Markovian.Kernel            one-layer floating stochastic kernel interface
Markovian.Kernel.Exact      fallible exact kernel with explicitly limited composition
Markovian.MRP               MRP interface
Markovian.Action            root-owned nominal action IDs
Markovian.MDP               floating MDP and outcome interfaces
Markovian.MDP.Exact         exact MDP, status, outcome, and model errors
Markovian.Policy            floating policy validation and fallible closure
Markovian.Policy.Exact      exact policy, support validation, and closure
Markovian.POMDP.Exact       exact beliefs and post-transition filtering
Markovian.POMDP.Planning.Exact bounded exact belief-policy evaluation
Markovian.Sampling          explicit generator and finite categorical sampling
Markovian.Trace             generic action-labeled bounded traces
Markovian.Interpreter.Exact bounded exact expectation and trace enumeration
Markovian.Interpreter.Sampled seeded floating finite sampling and traces
Markovian.Interpreter.Sampled.Step one validated selected-action joint sample
Markovian.Interpreter.DynamicProgramming.Exact exact finite-horizon policy backups
Markovian.Interpreter.Bellman.Exact exact contraction policy evaluation
Markovian.Interpreter.Control.Exact exact value iteration, greedy extraction, and policy iteration
Markovian.Learning.Tabular shared Q/V tables, rates, schedules, and observations
Markovian.Learning.EpsilonGreedy canonical behavior distribution and seeded sampler
Markovian.Learning.TD0 pure state-value update
Markovian.Learning.TD0.Episodic bounded supplied-policy TD runner
Markovian.Learning.Sarsa pure carried-action update
Markovian.Learning.Sarsa.Episodic bounded epsilon-greedy SARSA runner
Markovian.Learning.ExpectedSarsa pure expected behavior-policy update
Markovian.Learning.ExpectedSarsa.Episodic bounded Expected SARSA runner
Markovian.Learning.QLearning pure greedy-target action-value update
Markovian.Learning.QLearning.Episodic bounded epsilon-greedy behavior runner
```

The package keeps `Markovian.Category.Finite.Set.Internal` and `Markovian.Category.Matrix.Stochastic.Internal` as non-exposed implementation modules.

Learning modules depend on model and interpreter modules. Model modules do not depend on learning modules. Applications contain examples only. Tests contain executable contracts.

### 11.2 Package map

The implemented integration overlay has 16 packages:

```text
Markovian                         finite exact and exact-neutral structural root
markovian-continuous              restricted exact affine-uniform continuous family
markovian-continuous-numerical    bounded quadrature, explicit sampling, and Monte Carlo
markovian-reverse                 pure and effect-capable bounded reverse foundation
markovian-tensor                  checked host F64 buffers and CPU primitives
markovian-tensor-reverse          bounded region-owned host reverse adapter
markovian-safetensors             bounded canonical metadata-free F64 file profile
markovian-numerical               floating finite models and approximation boundary
markovian-dense-exact             optional dense Rational circuit execution
markovian-exact-benchmarks        exact domain fixtures, reports, and benchmarks
markovian-autodiff                closed typed language and bounded reverse lowering
markovian-neural                  checked neural reference updates
markovian-sampling                explicit generators and sampled finite interpreters
markovian-gpu                     bounded optional CUDA tensor executor
markovian-neural-bridge           exact action-layout adapter for neural heads
markovian-learning                tabular updates and bounded episodic runners
```

`ci/packages.tsv` records public dependency tiers. `release/packages.tsv` adds explicit versions. These manifests do not approve publication.

The root, continuous, continuous numerical, reverse, and tensor libraries depend only on `base`. `markovian-tensor-reverse` depends on tensor and reverse. `markovian-safetensors` depends on tensor and `bytestring`; it does not create an edge into the exact root. `markovian-gpu` depends on tensor and tensor-reverse but exposes no generic reverse-program resolver. Test-only integration edges are separate from public architecture.

The numerical, dense exact, and exact benchmark libraries depend on the root. Autodiff and neural depend on reverse. Sampling depends on the root and numerical package. The neural bridge depends on root and neural. Learning depends on the root, numerical package, and sampling package.

D-061 and D-067 remain `Proposed` until all acceptance gates pass. The effect interpreter and bounded host adapter do not establish generic tensor or device lowering.

### 11.3 Public API policy

The package is unreleased and experimental. Correctness changes can replace exposed interfaces immediately. D-062 applies PVP to later public releases and requires evidence-backed `tested-with` fields, package-specific metadata, full sibling bounds, complete Haddock, and exposed-module goldens.

A release change needs README, changelog, migration, metadata, license, and source-distribution review. D-063 uses a separate reviewed integration manifest, checked API snapshots, bounded archive validation and extraction, independently validated SPDX SBOMs, complete-bundle checksums and provenance, and atomic no-replace output staging. D-075 requires separate human publication authorization. These preparation tools neither publish nor establish release readiness.

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

The optional `markovian-tensor` package implements only managed host F64 storage. Shapes are type-indexed lists of dimensions. Rank zero contains one scalar; any zero dimension gives an empty payload. Opaque layouts admit contiguous row-major storage, immutable two-dimensional transpose views, and checked contiguous reshape. Materialization makes a fresh contiguous buffer. There is no public arbitrary-stride, broadcasting, mutation, pointer, sparse, pinned, foreign, or device API.

A rank-2 region prevents ordinary buffers from escaping their session. Nominal roles protect regions, dtypes, shapes, owners, and storage identities. Numerical primitives accept the finite refinement and reject nonfinite results. Addition, Hadamard multiplication, negation, `tanh`, total reduction, matrix multiplication, and contiguous copy use deterministic single-threaded loops and fresh outputs. Shape, machine-index, cumulative payload, buffer, and work limits are checked before each operation's first payload allocation. Multi-output Hadamard and matrix-product pullbacks preflight both outputs together. Allocation uses an uncommitted temporary set. A partial allocation failure explicitly finalizes the temporary set and does not commit IDs or accounting. Successful allocations register with the session and are explicitly finalized once at close. Private deterministic tests inject first-allocation, second-allocation, rollback-cleanup, close-cleanup, `Left`, and action-exception failures.

`TensorOwner` records semantic parameter control. `StorageId` records physical allocation only. Transpose views share one storage ID. Independent owners can reference that immutable storage without becoming one owner. Reverse accumulation follows semantic primitive rules rather than physical aliasing. Closed primitive tapes provide the declared Euclidean-coordinate VJPs only. `Markovian.Reverse.Program.Effect` executes supplied effectful callbacks for a bounded closed tree. The separate `markovian-tensor-reverse` adapter resolves only host F64 `tanh` and pointwise multiplication inside a rank-2 executor; it is not arbitrary tensor lowering and has no CUDA resolver. The separate `markovian-safetensors` package validates duplicate-preserving JSON, UTF-8 names, F64 descriptors, shapes, offsets, exact coverage, payload bounds, and a complete allocation plan. Its canonical encoder sorts UTF-8 names, materializes logical row-major views, and preserves raw IEEE words. It excludes metadata and all non-F64 dtypes and does not serialize owners or execution resources. This is a bounded profile, not a general SafeTensors claim.

A backend report must distinguish:

- Pathwise equality.
- Seeded reproducibility.
- Equality in distribution.
- Approximate numeric agreement.

The optional `markovian-gpu` package accepts only prepared positive-size F64 matrix multiplication and matrix-product VJP plans over checked tensor inputs. Preparation bounds dimensions, transfer bytes, scalar work, and user launches before backend selection. CPU execution delegates to the tensor matrix primitive and primitive tape. Device execution returns opaque type-indexed finite row-major host values; it does not convert results to exact values or allocate a tensor result.

The CUDA flag is off by default. A CUDA-enabled executor selects a device by deterministic ordinal, explicit ordinal, or UUID. Admission records the driver probe, device capabilities, kernel ABI, PTX target and SHA-256, loads the module, and runs a known-answer self-test. One rank-2 scoped executor owns a private context, module, and non-default stream. Execution and teardown share one lock; close waits for in-flight work before destruction. Safe FFI transactions use heap-backed host buffers, transfer inputs, record user-launch commitment, synchronize the owned stream, copy back, validate finiteness, and release per-call buffers. Masked teardown retains primary, bounded action-exception, and bounded cleanup diagnostics.

Dispatch distinguishes requested CPU, selected CUDA, and an explicitly reported CPU fallback. Launch commitment and fallback permission are separate. Configured fallback can occur only before the first user-kernel launch and only when no cleanup failure occurred. Matrix VJP preserves first-launch commitment across its second transaction. A launch, synchronization, copy-back, numerical, or cleanup failure never silently reruns on CPU. CPU fallback remains approximate F64 execution.

The committed PTX and admitted device profile are bounded to `sm_121` (compute capability 12.1) and implement only deterministic row-major F64 matrix multiplication. Every operand and output index product is checked against the signed kernel index range before launch. Matrix VJP lowering executes `seed * transpose(right)` and `transpose(left) * seed`; these are reverse derivatives under the declared coordinate pairing, not matrix dagger or another reversal operation. Enabled builds compile against pinned CUDA 13.0 headers but link only `libdl`. The native owner opens `libcuda.so.1` with `RTLD_NOW | RTLD_LOCAL`, resolves every required versioned symbol atomically before `cuInit`, retains the handle with the executor, destroys stream/module/context before `dlclose`, and never uses a function pointer after unload. Missing libraries, incomplete ABI tables, and devices outside the pinned profile are explicit pre-launch errors. Disabled builds require no CUDA headers or driver library. A digest-pinned compile-only job reproduces the driver-header/PTX artifacts and runs no-GPU loader fixtures; UUID-bound protected sanitizer validation is separate. Successful admission is not general device-correctness evidence. Zero dimensions, arbitrary tensor graphs, generic reverse programs, arbitrary strides, F32, stochastic nodes, user PTX, other device profiles, cross-device bitwise reproducibility, and GPU speed claims remain unsupported.

The historical 2026-08-26 dense-list evidence remains in [the complete evidence record](https://github.com/josephjohncox/Markovian/blob/main/docs/evidence/CUDA-2026-08-26.md). It predates this executor and must not be used as performance or correctness evidence for the new matrix/VJP fragment.

Autodiff belongs to a backend. A gradient of an expectation needs assumptions that justify differentiation under the expectation.

The optional `markovian-autodiff` package implements D-068's closed finite language. It has static unit, scalar, vector, and product shapes. It also has explicit parameter-owner trees. The polynomial compiler has separate exact `Rational` and checked-Double paths. The smooth compiler adds only `tanh` to the Double path. The compiler supplies all primitive VJPs and lowers structural sharing to the owned reverse interpreter. Public modules do not expose callback primitives, executable constructors, or tape constructors.

Compiler limits cover source nodes, primitives, depth, owners, coordinates, and layout structure. Stored and recomputed tapes are separate policies. Reports contain deterministic structure and no timing fields. A test-only integration edge to `markovian-neural` checks one closed `2 -> 2 tanh -> 2` two-layer fixture. Every output basis seed checks all input, weight, and bias VJP coordinates against the manual neural runtime and independent central finite differences. A committed golden records both tape-policy reports. This fixture does not add a public neural, matrix, tensor, or device lowering API. The package does not support arbitrary Haskell, recursion, branches, stochastic nodes, sampling, tensor execution, devices, or higher derivatives.

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
- Gradient and detachment assumptions.
- Device precision and reproducibility when a device exists.
- Failure behavior for NaN, infinity, or invalid support.

The `markovian-neural` package uses checked `Double` arithmetic behind a package-local approximation boundary compatible with the root contract. It implements an opaque finite scalar, stable softmax, analytic categorical gradients, approximate entropy, cross entropy, KL divergence, mutual information, entropy and cross-entropy logit gradients, row-major dense networks, manual VJPs, typed parametric reverse circuits, finite owned reverse programs, and pure SGD. Dense layers use `tanh` hidden activations and a linear output head.

`Markovian.Reverse` in the optional `markovian-reverse` package keeps parameter, input, output, scalar, and cotangent types distinct. The neural package does not expose a second reverse API path. Sequential and parallel composition use explicit nested pair products for independent parameters. Input and parameter diagonals use addition from a `CotangentSpace` witness. A primitive returns its output and captured pullback together. Its pullback must preserve zero and addition and be homogeneous over that scalar structure.

`Markovian.Reverse.Program` is a finite acyclic GADT over a caller-owned primitive signature. It supports only primitive, identity, composition, tensor, shared-input pairing, and shared-parameter tensor. Primitive definitions declare finite primal spaces, finite cotangent modules, exact or approximate equality, structural parameter ownership, and one stored-pullback or recomputation policy. Preparation checks program node/depth, primitive, unique-owner, extent, and separate layout/ownership structural node/depth limits before producing an opaque prepared tree. Structural nodes are charged before descent. Recomputation is a distinct typed owner-supplied operation, not a second call to the forward callback. Opaque typed tapes are self-contained. Reverse diagonals validate and add every branch contribution. Exact `Rational` fixtures check representative composition, tensor, and diagonal laws. One heterogeneous `Double` fixture checks every input and parameter coordinate under both policies with D-052's tolerances.

Information quantities remain outside the exact rational core because logarithms of rational probabilities are generally irrational. The pure reverse API is the `Identity` specialization of the effect-capable execution core. `markovian-tensor` owns `Markovian.Tensor.Reverse` and keeps its allocator capability private; `markovian-tensor-reverse` owns only the bounded program adapter. The complete release matrix remains open. Reverse programs interpret supplied VJPs; they do not differentiate arbitrary Haskell and provide no optimizer, generic tensor lowering, CUDA resolver, stochastic-gradient semantics, recursion, cycle, checkpoint scheduler, or universal autodiff semantics. Reverse derivatives remain distinct from raw matrix dagger, categorical adjunctions, prior-indexed Bayesian inversion, and exact payoff pullback. State pushforward and exact finite payoff pullback are implemented as separate operations.

## 14. Finite alternating interaction protocols

`Markovian.Game.Arena` owns finite position and move layouts, move edges, and `Opponent` or `Player` ownership. Its opaque bounded constructor establishes reachability, one owner at each nonterminal position, an Opponent-owned initial turn, alternation, and color-DFS acyclicity. `LegalHistory` is opaque bounded replay evidence. Labelled arena equality and represented-layout equality are separate bounded checked operations.

`Markovian.Game.Strategy` reverses domain ownership and retains codomain ownership. A checked strategy is a bounded finite prefix set that is exhaustive at Opponent turns and total and single-valued at Player turns. Copycat mirrors literal move identities. Composition synchronizes a common middle identity, hides the middle, canonicalizes visible traces, and revalidates the result. One operation-wide conservative work account includes synchronization, deduplication, replay, comparisons, extensions, membership, and final validation. Composition is partial and can reject a hidden internal deadlock. Exhaustion returns no strategy. Reports are deterministic structural evidence without timing fields.

Observational equality means literal equality of external finite prefix sets under labelled-equivalent endpoints. It is not contextual equivalence or an extensional game-semantic quotient. The layer has no chance, payoff, best response, equilibrium, justification pointers, views, innocence, bracketing, cycle, recursion, or full-abstraction claim.

### 14.1 Owner-refined finite open games

`Markovian.Game.Optic.Finite` stores bounded total finite functions, explicit bijections, and concrete pairs `P : X -> Y` and `C : X x R -> S`. It does not implement arbitrary residual or coend optics. `Markovian.Game.Open.Finite` stores finite strategy profiles with structural owner trees, total play and coplay tables, and context-indexed best-response membership. It does not import arena histories.

Sequential composition uses the incumbent downstream strategy in the continuation passed upstream. Tensor composition holds the other incumbent play fixed. Both reject owner overlap because coordinated deviations by one owner across several sites are not implemented. Exact decisions in `Markovian.Game.Open.Exact` enumerate represented functions and compare `Rational` continuation values literally. Ties retain all maximizers.

Every table, product, function-space, equilibrium, and equality traversal has an explicit `Natural` preflight bound. Pure contextual equilibria satisfy `(sigma,sigma) in B(x,k)`. Strategy schemas retain a structural ownership tree. Observational equality accepts only structural leaf-preserving witnesses, canonicalizes continuations, and strictly checks play, coplay, and best-response membership over every represented finite continuation. Performed counters describe forced checks. Layout equality remains separate. No `Category` instance is exposed.

The open-game fragment itself has no equilibrium-existence result, generic mixed or correlated lifting, chance, repetition, incomplete information, continuous strategy space, subgame-perfect solver, disintegration, or MDP-agent integration. Matching pennies has no pure open-game equilibrium, and the sequential fixture records a non-credible-threat counterexample.

### 14.2 Exact mixed, stochastic, and Harsanyi games

The separate exact normal-form layer begins with an `OwnedProduct`: one nonempty finite local carrier per owner and a preflight-bounded canonical pure-profile product. `ExactSimplex` is a complete extensional `Rational` table with nonnegative masses summing literally to one. Independent mixed profiles and joint correlation devices are different opaque types.

Normal games store complete rational player-value tables. Exact candidate evaluation and mixed-Nash checking enumerate every pure unilateral deviation. This is complete for a supplied candidate because unilateral utility is affine, but it is not a finder or existence result. CE uses unconditional direct-recommendation slacks and reports zero-mass recommendations as null. CCE uses separate constant deviations and a distinct report. The irrational three-player fixture and degenerate zero-payoff fixture block complete rational-enumeration and singular-support claims. No LP/LCP or support solver is in the root.

`ExactOutcomeLaw` retains each complete reward-vector/successor atom. The finite-horizon public-state stochastic evaluator checks terminal before horizon, returns terminal utility once, requests no action at a terminal, returns zero additional value for a nonterminal at horizon zero, applies transition reward once, decreases the horizon once, and discounts continuation once. Markov-perfect checking builds every continuation normal game. The scope excludes stationary solving, private observation histories, correlated Markov policies, and equilibrium existence.

The Harsanyi layer retains a common prior over complete type profiles and permits correlated types. Behavioral rows are owner/own-type action simplexes. Positive-type interim checks use exact unnormalized sums; null types have no invented posterior. Bounded strategic-normal conversion enumerates complete contingent plans only after capped power, product, cell, and work preflight. It does not split types into independent agents. The initial fragment uses one action carrier per owner across its types and excludes refinements, extensive games, imperfect recall, and Bayes-correlated equilibrium.

`Markovian.Game.Open.Strategic.Exact` does not lift an arbitrary open game. It extracts one closed context only after a caller supplies a complete owner-local/global-profile bijection and the existing callback agrees exhaustively with exact unilateral maximization.

All game reports contain deterministic represented counts and exact values, not wall-clock time. Rational size checks follow arithmetic and do not imply transactional heap rollback. These operations are distinct from matrix dagger, Bayesian inversion, payoff pullback, reverse differentiation, feedback, strategic duality, and disintegration.

`Markovian.Backend.Neural.Mask` stores a positive complete output width and nonempty ordered active indices. Boolean flags encode membership in global output order. Checked gathering occurs before softmax or argmax. The package does not multiply logits or Q-values by numeric masks and does not add negative infinity. Checked scattering uses positive `0.0` at unavailable positions.

`markovian-neural-bridge` owns exact-to-neural support compilation. It binds a root `FiniteActionIndex` to the actual width of a linear policy or dense head, rejects reordered global layouts, preserves each continuing state's exact availability and tie order, and returns a distinct terminal branch. Complete compilation preflights explicit state, cumulative action-entry, and conservative traversal-work limits and returns no partial collection after exhaustion. It converts action indexes to machine indexes only after range checks. Nominal roles protect action IDs, finite indexes, output layouts, and support masks from representational relabelling. The bridge performs no rational-to-`Double` approximation and supplies no feature map. The root and neural packages remain independent; only this bridge depends on both.

REINFORCE and actor-critic use masked linear categorical policies and linear scalar value functions. Unavailable parameter rows receive canonical positive-zero score derivatives. Their actor, baseline, and critic gradients use immutable pre-update snapshots. REINFORCE includes the outer discount power for the discounted start-return objective. Truncated episodes require an explicit boundary bootstrap.

Replay is a positive-capacity FIFO buffer with monotonic IDs and explicit ordered ID selection. It has no random sampler. Target networks support hard, periodic hard, and Polyak synchronization. Failed updates do not advance target schedules.

DQN supports standard and Double-DQN targets over nonempty ordered action masks. One nonempty batch uses one online and target snapshot, one mean half-squared-loss gradient, one atomic SGD update, and post-success target scheduling.

The package has no tensor framework, autodiff, device execution, environment runner, or complete trainer. Its finite-difference and deterministic fixtures do not support convergence, calibration, scalability, or production claims.

## 15. Categorical compiler IR

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

## 16. Guidance for advanced abstractions

### 15.1 Recursion schemes

Use a recursion scheme only for actual recursive model syntax, such as a bounded-horizon program DSL.

The base functor must contain recursive positions as its type parameter. The coalgebra must produce one layer. The algebra must use the supplied recursive result.

A state graph with a self-loop is not an initial-algebra tree. Evaluate it with a horizon or fixed point.

Ready when: a termination or productivity argument and equivalence tests against a direct interpreter are available.

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

Ready when: semantic-equivalence tests, allocation measurements, and runtime benchmarks cover a representative bind-heavy workload.

### 15.4 Cayley forms

A Cayley form can improve accumulation of traces, rewards, sparse updates, or other monoidal values. Keep it internal.

Floating-point addition is not associative. An optimization must state its numeric effect and pass tolerance-based differential tests.

Ready when: an identified accumulation bottleneck and benchmark evidence are available.

### 15.5 Normalization by evaluation

NBE requires a typed source DSL with clear normal forms. It can normalize deterministic and finite-distribution fragments.

NBE must not sample during normalization. It must residualize unsupported neural, continuous, or recursive primitives.

Ready when: soundness, reification correctness, a termination boundary, and code-size benchmarks are available.

### 15.6 Diagonalization

The categorical diagonal is the copy operation. It duplicates a value, not a stochastic computation.

Matrix diagonalization is a solver optimization for suitable finite transition operators. It is not a general semantic foundation.

A matrix backend can use eigendecomposition only after it checks the required matrix conditions. It must report conditioning, residuals, and numeric error.

Ready when: tests compare with a direct or iterative reference solver. A benchmark must show a benefit on representative models.

## 17. External package recommendations

These are candidates, not current dependencies. This list does not change the Cabal dependencies.

| Package | Recommendation | Boundary | Source |
| --- | --- | --- | --- |
| Hasktorch | Possible future tensor-framework adapter | Separate neural backend | [Hackage](https://hackage.haskell.org/package/hasktorch) and [source repository](https://github.com/hasktorch/hasktorch) |
| Accelerate | Candidate for batched finite array work | Separate array backend | [Hackage](https://hackage.haskell.org/package/accelerate) and [source repository](https://github.com/AccelerateHS/accelerate) |
| monad-bayes | Optional sampling interpreter | Adapter outside the core | [Hackage](https://hackage.haskell.org/package/monad-bayes) and [source repository](https://github.com/tweag/monad-bayes) |
| horde-ad | Research autodiff backend | Experimental package | [Hackage](https://hackage.haskell.org/package/horde-ad) and [source repository](https://github.com/Mikolaj/horde-ad) |

Before adding a dependency, record its purpose, license, maintenance state, lower bound, upper bound, and tested compiler range.

Prefer `base` and `Data.List.NonEmpty` for the first value types. Use `containers` for indexed maps when direct imports require it.

The current sampler uses a package-owned SplitMix64 stream implemented with `base`. Add `random` only when interoperability supplies an owned use case that the explicit `Generator` API cannot meet.

## 18. Testing strategy

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
- Differential circuit fixtures against the private `denoteExactIR` oracle, `lowerExactCircuit`, and dense exact CPU rows.
- POMDP differential fixtures for aggregation, support order, posterior values, zero evidence, and bounded planning.
- A concrete proof that transpose does not preserve row normalization.
- Zero-evidence POMDP observations.
- Equal-seed interpreter runs.
- Bellman residual and error bounds.

Do not use frequency thresholds as required CI gates. They are flaky and weak for exact support contracts.

### 17.3 Reference values

The exact sample has one transition reward `2`, discount `1/2`, and terminal payoff `7`. Its finite-horizon expected return is `11/2`.

The randomized exact policy fixture chooses rewards `2` and `8` with masses `1/4` and `3/4`. Its conditional expected reward is `13/2`.

## 19. CI and release gates

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
  backends/markovian-neural/markovian-neural.cabal \
  backends/markovian-neural-bridge/markovian-neural-bridge.cabal
cabal build all --prefer-oldest --project-file=cabal.project.ci
cabal test all --prefer-oldest --project-file=cabal.project.ci
cabal haddock all --project-file=cabal.project.ci \
  --enable-documentation --haddock-all --haddock-hyperlink-source
scripts/check-refinement-roles
scripts/check-circuit-purity
scripts/check-acyclic-proof-boundary
scripts/check-acyclic-purity
scripts/check-game-core-boundary
bash backends/markovian-neural/scripts/check-reverse-program-boundary
bash backends/markovian-neural-bridge/scripts/check-exact-support-boundary
cabal bench inventory-control-bench --project-file=cabal.project.ci
cabal bench clark-scarf-1960-bench --project-file=cabal.project.ci
cabal bench dogru-inventory-bench --project-file=cabal.project.ci
cabal bench fixed-batch-rnq-bench --project-file=cabal.project.ci
scripts/check-book
```

The book check validates local links, anchors, include targets, the pinned `mdbook` version, the pinned local MathJax digest and loader, every source-to-generated display-math block, and the complete HTML build. The published book includes foundation chapters for algebra, category theory, measure theory, categorical probability, information theory, categorical learning, polarity, push-pull duality, and game semantics. Internal links connect the narrative to APIs, boundaries, and direct primary-source reading routes. Haddock remains the API-signature reference.

The separate `Pages` workflow runs the same book gate for each push to `main`. A manual deployment must also use `main`. Its build job uploads only the checked `docs/book/build` directory. Its deployment job consumes that artifact through the `github-pages` environment.

Only the deployment job has Pages and OIDC write permissions. All workflow actions use immutable commit SHAs. The `/Markovian/` mdBook site path matches the configured project URL.

The Pages workflow does not replace compiler, lower-bound, source, Haddock, or source-distribution evidence. A completion claim records those CI results separately when the change requires them.

The source-distribution job creates all nine package archives. It validates paths, entry types, modes, duplicate names, credential-like names, and byte budgets before extraction. It then runs package checks, builds, tests, applicable compile-fail boundaries, and registered benchmarks from unpacked sources. The continuous numerical archive is tested with its unpacked exact-continuous sibling. Product and traversal limits must reject before returning partial evidence.

`bash scripts/prepare-release` adds the clean-revision gate, two byte-identical archive runs, a fresh exact consumer, complete exposed-declaration Haddock coverage, complete-bundle SHA-256 checksums, independently validated SPDX 2.3 source SBOMs, and atomic no-replace output staging. The manual workflow validates revision input before privileged jobs and independently in each job. It has no Hackage credential. Its separate attestation job downloads only checked artifacts and attests archives, SBOMs, the manifest, source revision, and checksum file. Preparation has no tag, release, candidate, or publication operation.

The compiler matrix tests GHC 9.4.8 and 9.8.4. Add more compilers only after their package bounds and full checks pass.

Pin GitHub Actions by commit SHA. Pin formatter versions. Change dependency pins in separate maintenance changes.

A semantic change also requires a recorded technical decision, updated invariants, and deterministic contract tests.

A public API or release change also requires README updates and a factual changelog entry.

## 20. Architecture compliance

A review fails when source, package metadata, tests, and durable documents disagree.

No file has silent precedence over another. Resolve disagreement with an explicit decision and a coordinated change.
