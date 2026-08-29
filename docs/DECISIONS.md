# Decision log

This file records active architecture decisions. Change a recorded decision with a new superseding entry. Do not rewrite its history to hide a reversal.

Editorial note: terminology-only edits do not alter decision IDs, dates, statuses, or recorded technical outcomes.

## Status terms

- **Accepted**: selected for the target design and current work.
- **Proposed**: review is still open.
- **Deferred**: implementation is not planned.
- **Rejected**: the project selected against this design.
- **Superseded**: a later decision replaces this entry.

## Decisions

### D-001: Use stochastic kernels as the semantic foundation

**Status:** Accepted

**Decision:** A stochastic kernel maps an input to a validated distribution over outputs. Finite kernels are the first implementation.

**Rationale:** Kernels separate mathematical meaning from sampling and execution libraries. Kleisli composition gives a law-bearing composition model.

**Consequences:** Raw probability vectors are not public semantics. Exact and floating interpreters need different equality contracts.

### D-002: Separate MRP, MDP, POMDP, and policy interfaces

**Status:** Accepted

**Decision:** Each model has an explicit interface. A policy kernel closes an MDP into an MRP.

**Rationale:** The legacy `Action` combines stochastic outcomes and agent actions. One type cannot preserve both meanings.

**Consequences:** An MDP has available action IDs and a transition outcome kernel. A POMDP also has an observation kernel and belief semantics.

### D-003: Validate probability and reward values

**Status:** Accepted

**Decision:** Use opaque `Prob`, `FiniteDist`, and `Reward` types with smart constructors.

**Rationale:** Empty, negative, zero-total, NaN, and infinite values make current evaluators partial or meaningless.

**Consequences:** Constructors return structured errors. Floating normalization uses D-017. A separate `SubDist` type is required if missing mass gains meaning.

### D-004: Make each objective explicit

**Status:** Accepted

**Decision:** Every evaluator receives a finite-horizon, discounted infinite-horizon, or other named objective.

**Rationale:** Hidden discounts and unbounded recursion prevent users from knowing which return the code computes.

**Consequences:** Finite and infinite objectives use different discount validation. Total reward needs explicit termination or integrability assumptions.

### D-005: Use fixed points for cyclic value problems

**Status:** Accepted

**Decision:** Bounded evaluation uses a horizon. Discounted cyclic evaluation uses Bellman fixed-point semantics.

**Rationale:** General MDPs are cyclic state systems, not finite trees. Forced evaluation of a recursively embedded tree can diverge on cycles.

**Consequences:** Solvers must state convergence assumptions, residuals, tolerances, and error bounds.

### D-006: Restrict recursion schemes to recursive syntax

**Status:** Accepted

**Decision:** Use recursion schemes only when a one-layer functor represents actual recursive model syntax.

**Rationale:** The current `MDPF` embeds recursive children and bypasses its recursion argument.

**Consequences:** State graph evaluation uses direct stepping, bounded dynamic programming, or fixed points. Recursion machinery stays internal unless laws justify exposure.

### D-007: Keep execution dependencies outside the core

**Status:** Accepted

**Decision:** Sampling frameworks, tensors, GPU code, autodiff, learning, and neural models live in interpreters or backend packages.

**Rationale:** A device or execution library must not define the mathematical model.

**Consequences:** The core dependency graph remains small. Backends need differential tests against a reference interpreter.

### D-008: Rank external backend candidates

**Status:** Accepted

**Decision:** Evaluate Hasktorch first for neural GPU work. Evaluate Accelerate for batched finite array work. Keep `monad-bayes` optional. Treat horde-ad as research.

**Rationale:** These packages address different execution needs. None belongs in the core.

**Consequences:** This decision does not add a dependency. Each package still needs license, maintenance, bounds, and benchmark review.

### D-009: Defer advanced categorical optimization

**Status:** Deferred

**Decision:** Defer Kan extensions, Codensity, Cayley forms, NBE, categorical compilation, and matrix diagonalization.

**Rationale:** The repository has no typed source DSL, optimizer benchmarks, or stable semantic core.

**Consequences:** No named feature is planned. Each feature needs a specific use case, required evidence, and a recorded superseding decision.

### D-010: Migrate additively with two legacy adapters

**Status:** Superseded by D-030

**Decision:** Add the new core before changing old exports. Add two explicitly named legacy adapters and no generic adapter.

**Rationale:** Legacy evaluation and legacy Q-learning assign incompatible meanings to `Action`.

**Consequences:** Keep compatibility through the 0.2 series. Remove it only in a PVP-major release after the stated support period.

### D-011: Use transition rewards and terminal payoffs

**Status:** Accepted

**Decision:** The target model assigns reward to each transition. A terminal state has one optional terminal payoff.

**Rationale:** This convention states reward timing for MRP, MDP, policy closure, Bellman equations, and learning targets.

**Consequences:** Interpreters apply transition reward once and terminal payoff once.

### D-015: Preserve joint outcomes during policy closure

**Status:** Accepted

**Decision:** Policy closure produces an MRP kernel over transition reward and successor state. It does not reduce rewards to one unconditional average.

**Rationale:** Different actions can reach one successor with different rewards. An average can lose reward distributions and reward traces.

**Consequences:** Closure preserves every bounded one-step reward-and-state observable. Kleisli composition preserves finite reward-and-state trace distributions.

A conditional expected reward exists only for a successor with positive marginal mass. A zero-mass request returns `ZeroMassTransition`.

Closure removes the selected action ID. Code that needs action-labeled traces uses the MDP trace interpreter.

### D-016: Include terminal payoffs in Bellman systems

**Status:** Accepted

**Decision:** Bellman solvers clamp terminal values to terminal payoffs. Linear systems solve only for nonterminal values.

**Rationale:** Terminal states have no transition row but can have a nonzero payoff. A full transition equation omits that boundary.

**Consequences:** The nonterminal right-hand side includes discounted transitions into terminal payoffs. Residuals cover the nonterminal Bellman equations.

### D-017: Scale floating weights before normalization

**Status:** Accepted

**Decision:** Divide floating weights by their largest positive weight before summation. Reject a non-positive or non-finite scaled total.

**Rationale:** A finite list of finite positive `Double` weights can overflow under direct summation.

**Consequences:** Two maximum finite weights normalize to two masses of `0.5`. Tests must cover direct-sum overflow and scaled-total failure.

## Open design proposals

### D-012: Select the first numeric representation

**Status:** Superseded by D-022 for the initial finite runtime representation

**Question:** Should production `FiniteDist` store normalized `Double`, exact `Rational`, or use separate concrete types?

**Required evidence:** API sketches, performance data, duplicate-support behavior, and law-test design.

### D-013: Select the public error accumulation style

**Status:** Superseded by D-022 for the initial smart constructors

**Question:** Should constructors stop at the first error or return all independent validation errors?

**Required evidence:** Examples for distribution, policy, model, and POMDP validation.

### D-014: Select the test framework

**Status:** Proposed

**Question:** Which unit and property test packages fit the supported compiler and dependency policy?

**Required evidence:** A small characterization-test change, Cabal bounds, maintenance review, and CI runtime.

### D-018: Evaluate a categorical compiler IR

**Status:** Superseded by D-035 and D-040

**Question:** Does a typed source DSL have a measured need for shared stochastic compilation across two or more backends?

**Required evidence:** Source and target categories, a typed IR sketch, structure laws, random-sharing tests, and a representative workload.

**Superseding effect:** If selected, D-018 supersedes D-009 only for categorical compiler IR work. D-009 continues to defer other named features.

### D-019: Select the Q-learning contract

**Status:** Superseded by D-033

**Question:** Which schedules, terminal target, exploration rule, seed contract, and step-limit behavior define the replacement learner?

**Required evidence:** A pure update example, configuration errors, scripted traces, and the convergence claims that the API will not make.

### D-020: Select the finite POMDP contract

**Status:** Superseded by D-034

**Question:** Which observation timing, belief representation, and zero-evidence error define the first finite POMDP?

**Required evidence:** Filtering equations and exact examples for normalization and impossible observations.

### D-021: Evaluate one continuous-kernel use case

**Status:** Proposed

**Question:** Which continuous model and interpreter justify a package outside the finite core?

**Required evidence:** Measurability, integrability, supported operations, errors, and a reference or statistical validation plan.

### D-022: Implement the bounded Foundation Kickoff core slice

**Status:** Completed and superseded by D-030

**Decision:** The user-selected Foundation Kickoff adds one semantic-core slice before the blocked P0 baseline completes. The slice uses separate opaque `Double`-backed `Prob` and `Weight` values, scaled floating normalization, and fail-fast structured construction errors. `FiniteDist` preserves labeled duplicate entries and removes zero-weight entries. Exact-reference numeric types remain separate future work.

The slice can add `Reward`, `Kernel`, `Policy`, and one-step MRP and MDP interfaces. It cannot add policy closure, objectives, evaluators, adapters, learning changes, recursion abstractions, or backends.

**Rationale:** The requested task explicitly changed priority while the local Haskell toolchain was unavailable. A narrow additive interface could establish the action-versus-outcome boundary without changing legacy behavior or eagerly unfolding a state graph.

**Consequences:** D-012 and D-013 are superseded for these initial constructors. The P0 command gates are not waived. Source and test presence can be recorded from static inspection, but no build, test, Haddock, or completion claim is valid until a supported toolchain runs the required commands. The new API remains provisional until that evidence exists.

**Normalization clarification:** The constructor removes input zero weights. It also removes positive weights whose normalized `Double` mass rounds to zero. The maximum-weight entry remains positive, and public support contains only positive representable masses.

**Risk:** Compiler errors, warning failures, and package-format defects can remain undetected. No later semantic slice can start before this slice is compiled and tested.

### D-023: Pin the GHCup and direnv development toolchain

**Status:** Accepted

**Decision:** The development environment uses GHC 9.8.4, Cabal 3.16.1.0, HLS 2.14.0.0, Fourmolu 0.20.0.0, HLint 3.10, and cabal-fmt 0.1.12. `toolchain.env` is the version authority. `.envrc` creates a project-local GHCup binary directory without downloading tools. `scripts/bootstrap-tools` installs missing tools after explicit execution.

**Rationale:** GHC 9.8.4 supports the package's verified `base` range, the installed HLS release, and the pinned formatter packages. A project-local direnv path avoids changes to the globally active GHCup compiler. The ignored `.direnv/` directory also isolates Cabal-installed development executables.

**Consequences:** The package supports `base` 4.17 through 4.19. The local GHC 9.8.4 build, tests, Haddock generation, format checks, and source-distribution creation have command evidence. CI must still test GHC 9.4.8 and can add newer compilers after their bounds pass. Linux systems without the unversioned GMP linker file use the local runtime-library link created by the bootstrap script.

**Risk:** One development compiler cannot prove the supported range. GHCup metadata and external package availability can change. CI must use explicit versions and pinned action commits.

### D-024: Test the development and lower-bound compilers in pinned CI

**Status:** Accepted

**Decision:** GitHub CI uses Ubuntu 22.04 and tests GHC 9.4.8 and 9.8.4 with Cabal 3.16.1.0. It pins `actions/checkout` v7.0.1 to commit `3d3c42e5aac5ba805825da76410c181273ba90b1` and `haskell-actions/setup` v2.12.0 to peeled commit `6037f33647c3f17758a2356c80fc4a53d7e0685d`. A separate format job reads exact versions from `toolchain.env`.

**Rationale:** GHC 9.4.8 tests the declared `base-4.17` lower boundary. GHC 9.8.4 matches the development environment. Commit pins prevent mutable action tags from changing executable CI code. Fourmolu uses GHCup's third-party channel. HLint and cabal-fmt require separate Cabal installation plans because a combined plan has incompatible parser and filepath constraints.

**Consequences:** The build matrix runs package checks, project-scoped `-Werror` builds, tests, warning-free Haddocks, and source-distribution builds. The source-check job installs Fourmolu, HLint, and cabal-fmt through their verified separate plans. The first hosted matrix passed at <https://github.com/josephjohncox/Markovian/actions/runs/32537958654>.

**Risk:** A successful run does not freeze GitHub's hosted image, network, or action runtime. Exact action commits and tool versions limit change but cannot remove external service risk.

### D-025: Scope warning failures and lower-bound workarounds to CI

**Status:** Warning scope accepted; dependency workarounds superseded by D-030

**Decision:** `cabal.project.ci` applies `-Werror` only to the local Markovian package. The package requires monad-bayes 1.3.0.5 or later within the 1.3 series. CI constrains mwc-random to 0.15 or later and unix-compat to 0.7 or later for the `--prefer-oldest` plan.

**Rationale:** A command-line `--ghc-options=-Werror` also changes dependency builds and makes third-party warnings fail the project. monad-bayes 1.3.0.4 permits vty 6.1 and 6.2, whose package metadata omits a required utf8-string dependency. monad-bayes 1.3.0.5 fixes that range but permits mwc-random 0.13.6, which does not type-check its sampler implementation, and unix-compat 0.1.2, which fails to link on the development Linux system. The selected constraints are the smallest tested corrections.

**Consequences:** Normal and `--prefer-oldest` plans retain `-Werror` for project code without imposing it on dependencies. The tested lower plan uses monad-bayes 1.3.0.5, mwc-random 0.15.0.1, unix-compat 0.7, MonadRandom 0.6, and vector 0.13.0.0. Unused direct dependencies were removed from each component.

**Risk:** These are project-level corrections for upstream lower-bound defects. A future upstream revision can make them redundant. Remove them only after an unconstrained lower-bound plan builds and tests.

### D-026: Separate exact reference values and objective domains

**Status:** Accepted

**Decision:** Exact reference probabilities, weights, finite distributions, rewards, and discounts use opaque `Rational`-backed types in separate modules. Floating finite-horizon `Discount` accepts the closed interval from zero to one. `ContractionDiscount` and its exact counterpart accept the half-open interval from zero to one. `Horizon` stores an unbounded `Natural` and validates an `Integer` input.

**Rationale:** Exact law tests need literal equality and cannot inherit floating rounding. Finite-horizon evaluation permits a unit discount, while Bellman contraction arguments require a discount below one. A machine-sized horizon would add an unrelated overflow boundary. Separate types make each required invariant visible in signatures.

**Floating normalization proof:** After validation and zero removal, the largest weight `m` is positive and finite. Every scaled term `w_i / m` lies in the interval from zero to one, and at least one term equals one. Therefore the scaled total is positive. It can become infinite only after more than `maxFiniteDouble` positive terms, which no executable finite list can materialize. The defensive `InvalidScaledTotal` check remains, but a public deterministic unit test cannot reach it without an infeasible list. Tests instead cover invalid inputs, direct-sum overflow, rounded-zero removal, and positive exposed mass.

**Consequences:** `Markovian.Probability.Exact`, `Markovian.Reward.Exact`, `Markovian.Objective`, and `Markovian.Objective.Exact` are semantic-core modules. Exact distributions preserve duplicate labeled entries and remove zero weights. Floating probability, weight, reward, and discount constructors canonicalize negative zero to positive zero.

**Risk:** `Rational` is a reference representation, not a low-level runtime format. Large numerators and denominators can consume unbounded memory. Optimized interpreters must remain observationally related to exact results without importing this representation into GPU or tensor storage.

### D-027: Exact-law-test Kleisli laws in the exact kernel domain

**Status:** Accepted

**Decision:** `ExactFiniteDist` has an explicit bind operation, and `ExactKernel` composes from left to right with that operation. The API does not add a `Monad` instance or claim literal floating-kernel laws.

**Rationale:** Rational mass multiplication preserves positivity and normalization exactly. Nonempty support remains nonempty, duplicate labels remain distinct, and `NonEmpty` bind preserves deterministic support order. These properties make left identity, right identity, and associativity literal equalities. Floating multiplication can underflow and any renormalization changes rounding, so it needs a checked numeric contract instead of an unqualified law claim.

**Consequences:** `Markovian.Kernel.Exact` is the law-bearing reference kernel. Tests check exact distribution functor identity and composition plus all three exact Kleisli laws. `Markovian.Kernel` remains a one-layer floating runtime interface; later floating composition must state its error and observational-equivalence contract.

**Risk:** Exact bind multiplies support sizes and can grow exponentially across repeated composition. It is a reference operation for finite laws and bounded examples, not an optimized execution strategy.

### D-028: Make policy closure validated and fallible at each requested state

**Status:** Accepted

**Decision:** Policy closure validates model and policy action supports before it combines action and transition mass. Duplicate available action IDs, duplicate policy action IDs, and unavailable policy actions are distinct structured errors. A terminal state returns its payoff without evaluating the policy. Floating closure returns a fallible `PolicyMRP`; exact one-state closure returns an exact joint outcome distribution.

**Rationale:** An arbitrary state type cannot be exhaustively validated when closure is constructed. Returning the existing total `MRP` would either hide an error, assume an unproved global invariant, or require partial code. Per-state validation is explicit and total. Exact closure computes the joint-outcome equation directly by construction. Floating closure multiplies masses and revalidates them, so underflow or normalization failure remains visible.

**Consequences:** Closure removes action IDs but preserves every reward and successor label. Two actions that reach the same successor with different rewards remain separate outcomes. Conditional reward queries divide only by positive successor mass and return `ZeroMassTransition` otherwise. Exact two-step traces and bounded trace observables match direct policy-and-MDP execution.

**Risk:** Validation repeats when the same state is stepped more than once. Exact and floating support can grow by the product of policy and transition support sizes. A later finite-state compiler can validate once and cache a closed representation, but it must preserve these errors and joint-outcome semantics.

### D-029: Evaluate finite exact objectives by bounded state recursion

**Status:** Accepted

**Decision:** The exact reference evaluator receives an `ExactMDP`, an `ExactPolicy`, and one `ExactFiniteObjective` containing a validated horizon and exact discount. It inspects terminal status before the horizon boundary. A terminal state returns its payoff. A continuing state at horizon zero returns zero. Each other step computes the exact expectation of transition reward plus one discounted recursive value.

**Rationale:** This is the finite-horizon equation in Section 5.2. Recursion decreases an unbounded natural horizon, so a self-loop cannot cause semantic nontermination. Separate exact model and policy types avoid converting floating values and preserve literal law tests. A named objective prevents hidden discount or stopping defaults.

**Consequences:** Exact model, policy, and evaluation errors remain separate and are wrapped by the evaluator. Transition reward and reached terminal payoff each occur once. No policy or transition runs at a terminal state. The evaluator uses direct finite sums without sampling, matrix conversion, memoization, or recursive transition trees.

**Risk:** Direct evaluation can revisit states and grow exponentially with horizon and support size. It is a reference interpreter for bounded examples. Dynamic programming and compiled finite-state evaluators are later implementations that must match it exactly.

### D-030: Remove defective experimental APIs instead of preserving compatibility

**Status:** Accepted

**Decision:** Markovian is greenfield and unreleased. It has no compatibility obligation. Remove the branch-weight process, recursive `MDPF`, defective Q-learning code, legacy examples, compatibility tests, adapter roadmap, and dependencies used only by those artifacts. Do not provide shims or deprecation periods for interfaces known to be semantically wrong.

**Rationale:** Compatibility preserves value only when users depend on a coherent contract. These interfaces had no users and encoded contradictory meanings for actions, unchecked probability, unbounded recursion, partial operations, and incorrect learning semantics. Retaining them would increase defect surface and constrain the correct design for no benefit.

**Consequences:** The package exposes only the validated semantic modules. The library, sample, and tests depend only on `base` and the local package. Learning returns later under a new explicit contract. D-010, D-022 compatibility limits, and the dependency-workaround portion of D-025 no longer apply to current work.

**Risk:** This branch cannot serve as a drop-in update for code written against the prototype. That break is intentional. Git history remains the only record of the deleted API.

### D-031: Own explicit generator state and sample finite support without fixed-width truncation

**Status:** Accepted

**Decision:** Seeded interpreters receive and return an opaque SplitMix64 generator state. Finite categorical sampling converts each positive floating mass to its exact binary rational value, renormalizes those rational weights, and draws an unbiased integer by rejection. Exact distributions use their rational masses directly. A deterministic one-point choice consumes no generator state.

**Rationale:** A fixed 53-bit unit interval makes positive `Double` masses below `2^-53` unreachable. Statistical frequency assertions also cannot prove categorical correctness. Rational integer selection preserves all exposed support and gives deterministic equal-seed behavior without global randomness or a new package dependency.

**Consequences:** `Markovian.Sampling` owns generator progression and returns resumable state. `Markovian.Interpreter.Sampled` preserves the exact interpreter's terminal-before-horizon, reward-timing, and discount conventions. `Markovian.Trace` records action IDs, transition rewards, successors, and explicit stop reasons. Exact trace enumeration provides a distribution whose expected return is tested against direct exact evaluation.

**Risk:** The package now owns a stable pseudo-random stream contract. Changing the mixing function, rejection-bit order, or deterministic-choice consumption rule is an observable semantic change. Large rational denominators can require large integer draws and must be benchmarked before high-throughput use.

### D-032: Compile exact finite policies before dynamic programming and fixed-point evaluation

**Status:** Accepted

**Decision:** Exact finite compilation receives explicit state and action-ID supports. It rejects duplicate supports, an unindexed initial state, unindexed available or policy actions, and every transition to an unindexed successor. Terminal-only models may use an empty action index. Compiled policy closure is checked against direct per-state closure before solver use.

**Rationale:** Dynamic programming is sound only when every lookup domain is closed and finite. Exhaustive compilation moves model and policy validation ahead of iterative evaluation and removes partial indexing from solver loops. Index order is representation only; decoded semantics must remain unchanged under support reordering.

**Consequences:** Finite-horizon dynamic programming uses exact backward induction and reports its objective, iteration count, value vector, and initial value. Discounted policy evaluation requires an exact contraction discount and positive tolerance, clamps terminal payoffs, uses the sup norm, and reports residual `r` with stopping bound `r / (1 - gamma)`. An iteration limit is a reported stop reason rather than silent convergence.

**Risk:** Compilation traverses every indexed state and available transition eagerly. List-backed indexes are intentionally simple reference structures and have linear lookup cost. CPU array lowering must preserve these semantics while replacing their representation.

### D-033: Make tabular Q-learning pure at the update boundary and bounded at the episode boundary

**Status:** Accepted

**Decision:** A Q-table key is `(state, ActionId)` and an absent key denotes zero. One pure update receives a validated learning rate, discount, model, observed transition, and table. A terminal successor target is transition reward plus discounted terminal payoff. A continuing target uses the maximum only over that state's nonempty validated available actions. Episodic learning uses explicit constant learning-rate and epsilon schedules, episode and step horizons, and generator state.

**Rationale:** The deleted prototype hid learning rate, discount, epsilon, and termination behavior, ignored transition weights, omitted terminal payoff, and used partial vector maxima. Separating the pure algebraic update from seeded sampling makes every parameter and target independently testable. Bounded horizons rule out negative-count and cyclic nontermination.

**Consequences:** `Markovian.Learning.QLearning` owns finite Q-values, duplicate-free tables, schedules, configuration, and pure updates. `Markovian.Learning.QLearning.Episodic` samples the validated MDP, returns structured episode traces and final generator state, and uses deterministic first-action tie-breaking. Equal seeds produce equal updates and final tables. Statistical frequency thresholds are not correctness checks.

**Risk:** List-backed tables and constant schedules are reference implementations. More efficient maps and decaying schedules must preserve key semantics, validation, visit indexing, generator ownership, and deterministic scripted fixtures.

### D-034: Observe after transition and separate exact prediction from conditioning

**Status:** Accepted

**Decision:** The exact POMDP observation kernel maps an action ID and successor latent state to an exact observation distribution. Exact beliefs aggregate duplicate states and normalize rational mass. Filtering predicts the successor belief first and then conditions on the observed value. Zero evidence returns `ImpossibleExactObservation`.

**Rationale:** Observation timing changes the filtering equation and cannot remain implicit. Separate prediction and conditioning permit direct normalization and Bayes-law tests. Canonical belief support prevents duplicate latent labels from making equality and planning depend on representation.

**Consequences:** Bounded belief planning evaluates transition reward before discounted posterior-belief value. Every selected action must be available in all positive-mass continuing states. Beliefs mixing terminal and continuing states are rejected. A fully terminal belief returns its expected terminal payoff before the horizon boundary and without evaluating a policy.

**Risk:** Exact belief recursion can grow exponentially in horizon and observation support. Mixed termination is rejected rather than assigned ad hoc absorbing semantics. Finite belief compilation or approximation requires a separate decision and error contract.

### D-035: Use typed finite exact syntax and distinguish copy from independent tensor

**Status:** Accepted

**Decision:** The implemented categorical fragment has duplicate-free nonempty finite objects and exact finite stochastic kernels. Typed syntax contains identity, validated primitive kernels, composition, tensor, copy, and discard. Denotation canonicalizes equal output labels in target-object order. Tensor samples each side independently; copy duplicates one already-produced value.

**Rationale:** Rewriting one shared stochastic result as two kernel executions changes correlation. Typed source and target objects reject out-of-support primitive outputs and mismatched composition before lowering. Exact rational denotation makes identity, composition, tensor, copy, discard, and shared-draw counterexamples literal tests.

**Consequences:** `Markovian.Category.Finite.Exact` is the implemented exact finite IR. `Markovian.Backend.CPU.Exact` lowers it to a source-by-target row-major rational matrix. Source and target support order defines indexes only; dense execution is differentially tested against exact denotation.

**Risk:** This fragment has no recursive syntax, optimizer, sparse storage, or reward annotations. New primitives require preservation laws and cannot blur shared and independent stochastic execution.

### D-036: Keep CUDA and neural contracts in optional backend packages

**Status:** Accepted

**Decision:** CUDA and neural code live under `backends/` in packages separate from the semantic library. The CUDA flag is disabled by default. When enabled, the package uses the CUDA 13 driver API and committed PTX for dense `Double` execution. The neural package selects no framework; it defines stable softmax, analytic probability gradients, score-function estimator assumptions, and max-norm approximation error.

**Rationale:** Core denotation must not depend on devices, memory transfer, tensor frameworks, autodiff, or estimator choices. A disabled-by-default CUDA flag keeps ordinary and hosted builds portable. Actual GPU evidence must compare with a CPU reference and measure transfers, context setup, kernel execution, and cleanup together.

**Consequences:** The NVIDIA GB10 differential fixture reports zero observed error. Neural tests cover normalization, logit-shift invariance, analytic Jacobian row sums, explicit estimator choice, invalid values, and exact-reference approximation.

**Evidence correction, 2026-08-26:** The sample-bearing benchmark runs one excluded warmup before 20 measured runs. A final-correction worktree based on `2efb1c6` measured a `267.843920400 ms` transfer-inclusive mean and `3.025869898 ms` sample standard deviation. The range was `263.519087000 ms` to `276.777522000 ms`, and maximum error was `0.000e0`. The enabled differential test passed.

The [complete evidence record](https://github.com/josephjohncox/Markovian/blob/main/docs/evidence/CUDA-2026-08-26.md) contains raw samples, commands, tool versions, hashes, and revision context. It retains four older mean-only values as historical execution records because their raw samples and dispersion are unavailable. None of these local values is a general performance claim.

The host used driver 580.173.02 and compute capability 12.1. CUDA 13.0 `nvcc` V13.0.88 was available. The PTX build script reproduced the committed artifacts exactly.

**Risk:** The committed PTX targets CUDA compute capability 12.1 and the enabled package currently expects CUDA headers under `/usr/local/cuda/include`. Hosted CI checks the disabled contract only because its runners have no GPU. Any new device architecture, precision, or framework adapter requires fresh differential tests and transfer-inclusive benchmarks.

### D-037: Use standard probability and Kleisli abstractions without hiding finite object witnesses

**Status:** Accepted

**Decision:** `ExactFiniteDist` implements the standard finite probability monad interfaces. `ExactKernel` implements `Category`, `Arrow`, and `ArrowChoice`. The finite IR keeps explicit `FiniteObject` witnesses and adds symmetry, associator, unitor, deterministic, and fanout combinators.

**Rationale:** Standard type classes remove private copies of lawful map, bind, composition, product, and branch operations. The finite IR has a different constraint. Its category identity needs a value-level finite object, which the unconstrained `Category` class cannot provide.

The copy target must be the full tensor square. Diagonal support belongs to the denotation, not to the target object. A diagonal target subobject prevents general fanout and breaks the intended Markov-category typing.

**Consequences:** Exact distributions support `Functor`, `Applicative`, `Monad`, `Foldable`, and `Traversable`. Exact kernels support standard category and arrow combinators. IR tests cover full-tensor copy, symmetry, associator and unitor inverses, deterministic copy naturality, and stochastic shared-draw counterexamples.

**Risk:** The implementation is a finite symmetric monoidal Markov fragment, not a higher-category framework. It does not add a dagger, trace, compact closure, or total Bayesian inverse. Such structures need separate denotations and laws.

### D-038: Stage the exact semantic tower and keep reversal operations distinct

**Status:** Accepted

**Decision:** Implement four additive layers in stages: exact finite semiring matrices and their normalized refinements; prior-indexed exact Bayesian operations; purity-indexed finite stochastic circuits and the exact deterministic categorical compiler fragment; and syntax-only finite typed structured-cospan open systems. The matrix foundation uses explicit finite witnesses, exact nonnegative scalars, proof-carrying deterministic arrows, and exact convex enrichment.

D-037 supersedes only D-027's prohibition on a `Monad ExactFiniteDist` instance. D-027's exact-versus-floating law boundary remains active. D-035 supersedes D-018 and D-009 only for the implemented exact categorical compiler fragment. D-009 continues to defer unrelated categorical optimizations.

The design has three distinct reversal-like operations: matrix conjugate transpose, prior-indexed Bayesian inversion, and structured-cospan boundary reversal. No shared `Dagger` class or instance can contain these operations. Arbitrary open-system black-boxing and feedback semantics remain deferred.

**Rationale:** Raw matrices, normalized kernels, Bayesian inversion, and open boundaries have different domains and preservation laws. A transpose need not preserve stochastic normalization. Bayesian inversion depends on a prior and positive support. Boundary reversal swaps cospan feet without reversing directed internal edges. One overloaded operation would make false equations expressible.

**Consequences:** Implementation proceeds through explicit roadmap stages. Empty finite sets are valid for raw matrices, open boundaries, and vacuous normalized arrows from the empty set. A normalized arrow into the empty set exists only when its source is empty. Normalized states, priors, distributions, and existing probability objects remain nonempty. Each finite-witness module exports `sameFiniteLayout` as the canonical layout operation. The descriptive `sameFiniteSetLayout` and `sameFiniteObjectLayout` aliases remain available. Copy naturality is available only through proof-carrying deterministic syntax or matrices. Exact circuit and Bayesian layers can depend on the matrix foundation, but the foundation cannot depend on distributions, POMDPs, circuits, or backends. Open hypergraphs initially have syntax-only semantics except for a separately validated acyclic fragment.

The S1 transpose counterexample proves only that raw transpose does not preserve stochastic normalization. A comparison with prior-indexed Bayesian inversion belongs to S2 because that operation requires a prior, positive support restriction, division, and structured zero-evidence errors. Adding a placeholder inversion to S1 would erase those requirements and assert a false total operation. D-038 separates the names and types now; S2 must supply the Bayesian comparison evidence.

Proof-carrying matrix refinements use nominal roles for scalars and endpoints. Constructor opacity alone is insufficient because representational coercion could otherwise replace the scalar laws or endpoint equality evidence.

**Required evidence:** Each stage must add literal law tests for the structure that it exposes. The matrix stage must test category, tensor, biproduct, dagger, compact, trace, normalization, determinism, convexity, and a transpose-normalization counterexample. Bayesian work must test support-restricted Bayes laws and preserve existing POMDP behavior differentially. Circuit work must preserve stochastic sharing and reject dishonest deterministic tags. Open-system work must validate pushouts and 2-cells up to explicit canonical isomorphism and must reject unsupported feedback.

**Deferred:** A total black-box functor from arbitrary open hypergraphs to stochastic kernels, stationary feedback selection, unrelated NBE or Kan-extension optimizations, and a common dagger abstraction remain outside the supported scope.

### D-039: Restrict exact Bayesian inversion to positive prior support

**Status:** Accepted

**Decision:** An exact `Prior` is a normalized state on an explicit nonempty finite object. It stores its positive `Support`. Pushforward, joint construction, evidence, and conditioning use exact nonnegative rational matrices. A `BayesianInverse source target` contains a normalized map from positive output support to positive input support. It does not choose rows for zero-evidence outputs.

`almostSureEqual prior left right` is the explicit prior-indexed equality operation. Ordinary `Eq` does not express almost-sure equality. A `BayesianChannel` stores its input prior, forward channel, and exact pushforward prior. Composition checks that the second input prior equals the first output prior. It is not a `Category` over plain endpoint types.

**Rationale:** The equation `p(x) K(x,y) / q(y)` is defined only when `q(y)` is positive. Arbitrary rows on zero-output support are not Bayesian data. Support restriction gives a total normalized inverse on its mathematical domain and makes the almost-sure uniqueness boundary explicit.

**Consequences:** Prior-indexed Bayesian inversion remains distinct from matrix conjugate transpose and structured-cospan boundary reversal. The package adds no shared `Dagger` class and no `Dagger ExactKernel` instance. Exact POMDP prediction and conditioning delegate to the Bayesian distribution algebra while preserving post-transition timing, duplicate aggregation, support order, posterior values, and `ImpossibleExactObservation`.

**Required evidence:** Exact tests cover normalization, support extraction, the Bayes joint equation, identity, composition reversal, independent tensor, double inversion after support restriction, almost-sure equivalence, zero-evidence errors, and checked Bayesian-channel prior flow. Floating inference remains outside these literal law claims.

### D-040: Reify purity-indexed stochastic circuits and first-order deterministic compilation

**Status:** Accepted

**Decision:** `Circuit primitive purity source target` is opaque recursive syntax. `purity` records deterministic or stochastic construction provenance. Identity, composition, tensor, symmetry, associators, unitors, copy, discard, validated finite tables, convex choice, sharing, and fanout remain explicit nodes. `share circuit` executes once and copies one result. `fanout left right` copies the input and executes both branches independently conditional on that input.

Only deterministic syntax can use the copy-naturality rewrite. Deterministic primitives must interpret to `DeterministicMatrix`; stochastic primitives interpret to `StochasticMatrix`. The only purity cast weakens deterministic syntax to stochastic syntax. Compile-fail evidence rejects strengthening and deterministic copy optimization on stochastic circuits.

`CircuitAlgebra` is an unchecked fold-operation record. It carries no categorical laws. `foldCircuit` derives sharing from composition and copy, and derives fanout from copy, tensor, and composition. An algebra that claims homomorphism laws must prove them separately. The exact algebra targets proof-carrying deterministic and normalized stochastic nonnegative-rational matrices. Exact kernel application and dense CPU lowering use that same matrix denotation. Floating, CUDA, and neural backends must use the separate approximation boundary with an observation relation, precision, and error policy.

The deterministic source fragment contains identity, composition, products, pairing, projections, and finite quoted tables. Projection compilation uses discard and unitors. The source contains no Haskell function values or function equality.

**Rationale:** A provenance index prevents a currently Dirac stochastic gate from enabling copy rewrites. Reified sharing prevents accidental duplication of random effects. Finite tables give total inspectable first-order compilation without pretending that arbitrary Haskell functions form a quoted language.

**Consequences:** Recursive circuits are supported, but recursive circuit definitions can diverge before construction and no feedback node is provided. The syntax is not claimed to be a quotient or a mechanically proved initial object. Arbitrary Haskell functions, bottoms, exceptions, `seq`, opaque higher-order functions, infinite types, finite exponentials, and cartesian closure of stochastic maps remain unsupported. Open hypergraphs and structured cospans remain S5 work.

**Required evidence:** Exact tests cover structural-fold preservation, derived sharing and fanout, deterministic-only copy naturality, convex choice, pentagon, triangle, symmetry hexagon, structural naturality, both unitors, reordered layouts, source compilation equations, validation failures, and differential agreement with `denoteExactIR`, `lowerExactIR`, and dense CPU rows.

### D-041: Implement typed structured-cospan syntax and only decorated circuit denotation

**Status:** Accepted

**Decision:** Open syntax uses finite typed interfaces and directed labelled hypergraphs. Hyperedges have identities and ordered typed input and output ports. Interface maps and hypergraph maps are total, type preserving, and opaque. Structured cospans have total, not necessarily injective, legs from discrete boundary hypergraphs.

Horizontal composition constructs the finite pushout of apex vertices along the shared discrete boundary. The pushout exposes explicit quotient classes, canonical injections, and checked cocone factorization. Members of each binary quotient class are stored in left-carrier order followed by right-carrier order. Cocones and middle vertical arrows are compared extensionally, so reordered but typed-support-equivalent interfaces do not block factorization or horizontal cell composition. Hyperedges combine by disjoint union because the glued boundary is discrete. Tensor is disjoint union. Boundary reversal swaps the two cospan legs without changing the apex, hyperedges, labels, or the directed circuit state orientation.

The implemented double fragment has interfaces as objects, interface maps as vertical arrows, structured cospans as horizontal arrows, and commuting squares as 2-cells. A 2-cell contains both vertical boundary maps and a type-, label-, order-, and incidence-preserving apex hypergraph map. Vertical composition is map composition. Horizontal composition is the induced pushout map. Tensor acts componentwise.

`OpenCircuit` is a structured cospan with a global directed circuit decoration. Sequential gluing composes both topology and decoration; tensor combines both. Only the circuit decoration has exact stochastic denotation. Hypergraph labels and internal topology are not black-boxed. Boundary reversal exchanges only the topological boundary parameters. It retains the original circuit state input and output parameters and returns a view with no reversed stochastic-denotation observer.

**Rationale:** Finite pushouts justify structured-cospan composition and the double cells. A separate global decoration permits exact composition tests without claiming an unimplemented semantics for arbitrary directed hypergraphs, cycles, feedback, MDPs, or continuous-time open Markov processes.

**Consequences:** Structured-cospan boundary reversal is distinct from matrix conjugate transpose and prior-indexed Bayesian inversion. No common `Dagger` class or instance is added. Binary quotient classes have an implemented canonical order. Nested associativity is not literal equality because the carrier types differ. Representative tests construct the canonical member-flattening isomorphism; this is not an exported general associator, unitor, or coherence theorem. The implementation does not claim a strict double category, a general bicategorical coherence theorem, graph black-boxing, feedback semantics, or existing continuous-time open-Markov theorems for MDPs.

**Required evidence:** Tests cover interface-map row canonicalization, hypergraph validation, nominal pushout witnesses, noninjective pushout quotients, canonical class order, layout-independent cocone factorization, gluing, disjoint-union tensor, reversal with unchanged directed state orientation, both unitor isomorphism fixtures, both associator round trips on vertex and edge maps, extensionally matched horizontal cell composition, interchange, and exact decorated-circuit composition, tensor, associativity, and units.

### D-042: Interpret only boundary-functional finite DAG networks

**Status:** Accepted

**Decision:** Add the opaque `AcyclicOpenSystem` refinement and validate every apex vertex as having exactly one producer: one injective input-boundary occurrence or one edge-output occurrence. Reject unproduced vertices, boundary-and-edge production, multiple edge producers, repeated outputs within one edge, self-loops, and directed cycles before interpretation. Stable Kahn sorting uses edge layout order and cycle errors contain an actual, canonically rotated cycle.

`FiniteValueDomains` assigns one finite Haskell carrier to each runtime sort. `Assignment` retains named port identity and compares extensionally, so boundary layout changes can be reindexed without treating bare value lists as typed assignments. Label circuits are keyed by label and ordered input and output sort signatures. Every represented edge selects one purity-indexed `Circuit`; repeated edge occurrences execute separately, while multiple consumers read one stored result.

`AcyclicOpenCircuit` has aggregate purity equal to the join of selected represented edges. Its exact denotation interprets each local circuit with edge context. It composes initialization, topological edge-step, and observation matrices from left to right. Each step drops values that no later edge or output observation can read. The step sums over dropped edge outputs. The interpreter validates final normalization without renormalizing. Output duplication is an equality observation.

The implementation does not build the complete apex assignment object. A bounded differential fixture evaluates the complete-valuation product-and-sum equation independently. Runtime cost can still grow exponentially with live-frontier width and boundary size.

Composition delegates to structured-cospan pushout and validates the refinement again. Tensor is disjoint union. The supported laws are exact normalization, identity, sequential composition after named-boundary reindexing, independent tensor, stored-value sharing, discard marginalization, conditional products, and schedule independence for successful denotations. Nontrivial fixtures compare composition and tensor with directly composed and reindexed matrices using `stochasticEquivalent`; literal nested pushout equality is not required. Interpretation failures remain schedule-ordered diagnostics: stable topological order determines which failing edge is reported first.

**Rationale:** A generic label-to-circuit function cannot select arbitrary Haskell endpoint types from runtime sorts. Positional assignments over one finite carrier support arbitrary finite arity without claiming dependent typing. Unique production and acyclicity make causal evaluation total and distinguish stored sharing from repeated stochastic execution.

**Consequences:** `OpenSystem`, `OpenCircuit`, `openCircuitDenotation`, boundary reversal, and the boundary-reversed view are unchanged. A reversed topology can only receive an ordinary forward interpretation after fresh validation. The implementation adds no reverse dynamics.

**Required evidence:** Deterministic fixtures cover every topology rejection constructor, producer-error precedence, and the public reachable domain, assignment, label, endpoint, primitive, run-input, and purity paths used by the fragment. Opaque internal-invariant branches are not claimed as externally constructible fixtures. Further fixtures cover zero and arbitrary arity, identity, nonidentity chains, parallel composition, sharing, independent execution, full and partial discard, diamonds, duplicated observations, boundary layout changes, successful schedule and renaming independence, pushout composition, mismatched composition boundaries, tensor, units, and associations. Differential checks compare the live-frontier construction with a bounded complete-valuation evaluator, directly built circuits, directly composed matrices, and a named-assignment reindexing of `tensorStochastic`. A twelve-edge narrow chain guards against retention of complete apex history. Compile-fail gates reject raw topology, inaccessible constructors, forged cycles, aggregate-purity strengthening, reverse observation, substitution for the existing global decoration denotation, and representational coercion of validated label tables.

**Deferred:** Arbitrary cyclic graphs, trace, feedback, recursion, fixed points, factor normalization, implicit priors, merge semantics, Bayesian reversal, `OpenSystemCell` denotation, infinite or continuous carriers, continuous-time open Markov black-boxing, unrestricted MDP black-boxing, and a machine-checked theorem for all finite DAGs.

### D-043: Use a pinned mdBook stack for the user and contributor book

**Status:** Accepted

**Decision:** Keep conceptual documentation in `docs/book` and build it with the `mdbook` version pinned in `toolchain.env`. Keep Haddock as the exact API-signature reference. Include the architecture, decision, and workflow records as book appendices instead of copying their content.

Run a repository-owned check for local links, anchors, include targets, the tool version, and the complete HTML build. Run the same check in hosted CI. Include all book sources and documentation scripts in the source distribution.

Publish the checked HTML with the separate `Pages` workflow. Run it for every push to `main`, or by manual request from `main`. Keep its build and deployment jobs separate. Upload `docs/book/build` only after `scripts/check-book` passes. Use the `github-pages` environment and the deployment action URL output.

Grant `contents: read` only to the build job. Grant `pages: write` and `id-token: write` only to the deployment job. Use one Pages concurrency group and do not cancel an active deployment.

Pin the official actions to these releases and commits:

- `actions/checkout` v7.0.1 at `3d3c42e5aac5ba805825da76410c181273ba90b1`.
- `actions/configure-pages` v6.0.0 at `45bfe0192ca1faeb007ade9deae92b16b8254a0d`.
- `actions/upload-pages-artifact` v5.0.0 at `fc324d3547104276b827a68afc52ff2a11cc49c9`.
- `actions/deploy-pages` v5.0.0 at `cd2ce8fcbc39b97be8ca5fce6e763baed58fa128`.

Set the mdBook `site-url` to `/Markovian/`. This path supports the configured project URL at <https://josephjohncox.github.io/Markovian/>.

**Rationale:** The existing README and architecture record describe contracts, but they do not provide a guided path for users. A versioned Markdown book gives the project one navigable conceptual guide without adding a runtime dependency to the Haskell packages. A separate Pages workflow keeps publication permissions out of package CI jobs and permits a manual publication retry.

**Consequences:** A user-visible semantic change must update its book chapter. Documentation changes can fail CI before a package build when navigation, includes, or the book renderer break. Generated HTML remains untracked. The Pages workflow checks the book but does not replace package CI evidence.

**Required evidence:** `scripts/check-book` must pass with the pinned tool. `cabal check` and source-distribution gates must include the complete book source, its scripts, and the Pages workflow. A publication claim also needs a successful hosted `Pages` run for the deployed revision and a reachable public URL.

### D-044: Compile exact MDPs without a policy and keep exact control separate

**Status:** Accepted

**Decision:** Compile a finite exact MDP before selecting a policy. Store every model-available action in per-state availability order. Store each complete joint reward-successor distribution, including duplicate labels and support order.

Close a compiled MDP under an exact policy as a separate operation. Finite-horizon and Bellman policy evaluators consume the resulting compiled MRP.

Implement discounted exact value iteration over the policy-free compiled MDP. Report the completed backup count, exact value vector, initial value, sup-norm optimality residual, contraction value-error bound, greedy-policy performance bound, and stop reason. A zero iteration limit performs no backups.

Extract deterministic greedy actions in model availability order. Replace a selected action only for a strict greater value, so exact ties retain the first available action.

Implement bounded deterministic policy iteration with exact rational policy evaluation. Solve signed linear systems with a dedicated rational Gaussian solver. Do not use the nonnegative matrix API for subtraction.

**Rationale:** Policy-free compilation validates the complete finite control problem once. Joint outcome storage preserves reward-successor correlation. Exact action comparisons make deterministic tie behavior inspectable.

**Consequences:** D-032 is superseded where it requires policy-bound compilation. Policy evaluation still requires explicit policy closure. Finite value iteration reports an approximation bound and does not label a finite iterate an exact optimum.

**Required evidence:** Deterministic fixtures cover terminal-only compilation, all actions, joint and duplicate outcomes, index failures, terminal timing, negative rewards, zero discount, zero limits, residual bounds, ties, permutations, greedy-policy bounds, exact policy residuals, exhaustive deterministic-policy comparison, and agreement between policy and value iteration within reported bounds.

### D-045: Share sampled steps and tabular TD foundations

**Status:** Accepted

**Decision:** Use one sampled-step operation for episodic interpreters. It receives the selected action and an explicit generator. It validates through `stepMDP`, samples one joint reward-successor outcome, and returns one trace step plus the next generator.

Keep Q-tables, V-tables, finite values, learning rates, exploration rates, constant schedules, and observed transitions in a shared tabular module. Use one canonical epsilon-greedy distribution for behavior and Expected SARSA targets. Exact ties retain the first model-available action.

Provide pure terminal-aware updates for TD(0), SARSA, Expected SARSA, and Q-learning. Use `r + gamma * g` for every terminal target. Use separate terminal and continuing bootstrap constructors when an algorithm needs successor policy data.

Provide bounded episodic runners with empty-start and resumable APIs. A resume call owns the current table, episode index, global update count, and generator. Q-learning separates epsilon-greedy behavior from its greedy target. SARSA selects and carries the exact next behavior action.

**Rationale:** One joint step prevents reward-successor decorrelation. Shared behavior logic prevents different epsilon-greedy meanings. Pure updates expose the algorithmic target without hiding environment or generator effects.

**Consequences:** The root learning APIs use checked `Double` arithmetic. They are sample-based, but they still inspect the supplied MDP for terminal status and action-support validation. A bounded run makes no convergence claim.

**Required evidence:** Deterministic fixtures cover distinct continuing targets, common terminal targets, no terminal policy query, behavior-target separation, carried SARSA actions, Expected SARSA distributions, unavailable actions, malformed models, arithmetic failures, exact seeded generator states, zero bounds, terminal timing, bounded loops, split-run equality, and exact-formula differentials.

### D-046: Keep neural numerical update references framework-independent

**Status:** Accepted

**Decision:** Keep `markovian-neural` optional and independent of tensors, autodiff, devices, and global randomness. Keep the released neural package `base`-only and define its approximation boundary locally so its source archive builds independently. Test agreement with the root `Markovian` package in a separate integration suite. Use an opaque finite scalar where a durable finite claim is needed; validate raw `Double` inputs and arithmetic results at operation boundaries. Use explicit dimensions, stable categorical operations, immutable snapshots, and structured nonfinite failures.

Provide row-major dense networks with zero or more `tanh` hidden layers, a linear output head, and manual input and parameter VJPs. Provide pure SGD from one pre-update snapshot.

Provide executable masked linear REINFORCE and one-step actor-critic updates. Normalize over each observation's nonempty action mask and scatter score gradients into global parameter order. For the discounted start-return objective, weight each REINFORCE actor term by `gamma^t`. Detach baselines, targets, and advantages at their stated boundaries. Compute all participating gradients from pre-update parameters and return all updated models or an error.

Provide a bounded FIFO replay buffer with monotonic IDs and explicit ordered ID selection. Provide topology-checked hard, periodic, and Polyak target synchronization. Count only successful online updates.

Provide standard and Double-DQN targets over explicit nonempty action masks. Evaluate one complete nonempty batch from one online and target snapshot. Aggregate one mean half-squared-loss gradient, apply one atomic SGD step, and schedule target synchronization only after success.

**Rationale:** Small framework-independent implementations make numerical and detachment contracts executable without adding a runtime framework to the semantic core. Immutable snapshots make update timing and failure atomicity explicit.

**Consequences:** D-036 is superseded where it describes the neural package as metadata only. The package remains an experimental `Double` reference. It has no environment runner, random replay adapter, complete trainer, convergence guarantee, accelerator support, or production claim.

**Required evidence:** Central finite differences cover every represented dense VJP and categorical derivative on finite fixtures. Worked examples cover REINFORCE and actor-critic targets, detachment, and failure atomicity. Replay, target-network, DQN, batch-loss, and root Q-learning differential fixtures cover ordering, timing, masks, snapshot behavior, and successful updates.

### D-047: Self-host mathematical rendering and connect foundation chapters to primary sources

**Status:** Accepted

**Decision:** Disable mdBook's external MathJax 2 loader. Commit the complete MathJax 3.2.2 SVG bundle and its Apache-2.0 license under `docs/book/theme/vendor`. Load the local configuration before the local bundle through `additional-js`. Pin the bundle digest in `toolchain.env`.

Extend `scripts/check-book` to verify the MathJax digest, reject the external loader, require the generated local script tags, and compare every source display-math block with its generated chapter HTML. Before MathJax startup, recover TeX subscripts and ASCII primes that mdBook can transform into emphasis tags or smart punctuation. Reject unsupported generated tags and ambiguous bare TeX stars.

Add separate foundation chapters for algebra, category theory, measure theory, and categorical probability. State the finite specialization, the categorical structure at each refinement layer, and the analytic requirements that do not disappear under categorical notation. Link explanations to the implementation chapters, the law catalogue, and annotated primary or standard references.

**Rationale:** The external MathJax loader can be blocked or unavailable even when the generated HTML contains correct delimiters. A local complete SVG bundle removes that runtime dependency. Foundation chapters let readers with basic mathematical background understand why the API separates semiring matrices, stochastic refinements, Bayesian inversion, fixed points, and approximate execution.

**Consequences:** The source distribution grows by the vendored bundle. Updating MathJax requires an explicit version, digest, license, local build, and Pages check. Documentation equations continue to use mdBook's required double-backslash display delimiters. A mathematical claim should link to its internal explanation and then to a direct primary or standard source where practical.

**Required evidence:** `scripts/check-book` verifies the local loader and all display blocks. Local links and anchors must pass. A browser-startup check must turn every generated display block into one MathJax container with no raw delimiters or MathJax errors. The source archive must contain the configuration, bundle, license, and foundation chapters. A publication claim requires a successful Pages run and reachable rendered pages.

### D-048: Keep information observables and reverse learning structure explicit

**Status:** Accepted

**Decision:** Add separate information-theory and categorical-learning foundation chapters. Distinguish linear adjoints, reverse derivatives, adjoint functors, and prior-indexed Bayesian inversion. Treat losses and optimizer updates as additional structure rather than consequences of forward categorical composition.

Keep exact finite states, channels, supports, sharing, and tensor factorization in the rational semantic layer. Evaluate Shannon entropy, cross entropy, KL divergence, mutual information, and their logit gradients in the optional checked-`Double` neural package because logarithms of rational probabilities are generally irrational.

Use the forward diagonal to represent shared values and parameters. In a future differentiable interpreter, require its reverse rule to accumulate cotangents from every consumer. Permit deterministic share-versus-fanout rewrites only with purity evidence. Use exact availability evidence to derive approximate action masks when a concrete lowering supplies the feature map.

Treat Clark--Scarf and Doğru inventory models as named benchmark families only when their event timing, lead times, costs, demand assumptions, policy class, and approximation assumptions are explicit. In particular, represent Doğru's balance assumption as a named relaxation and compare it with a bounded physical allocation model; do not hide the relaxation inside a kernel.

**Rationale:** Information quantities make channel loss and representation quality measurable, while reverse-derivative structure explains compositional VJPs, tied-parameter accumulation, and fusion. Keeping these layers separate prevents four incompatible meanings of “reverse” from entering one API. Named operational-research benchmarks provide stronger approximation evidence than synthetic training curves when their assumptions are reproduced exactly.

**Consequences:** The exact core acquires no logarithm, autodiff, tensor, or optimizer dependency. `markovian-neural` gains finite information references and gradients but no convergence claim. A future parametric-circuit API must state parameter products, cotangent objects, primitive reverse laws, optimizer state, and numerical equality before implementation. Inventory approximation work begins with a small exact oracle and deterministic error report.

**Required evidence:** Information fixtures cover uniform entropy, shift invariance, product additivity, cross-entropy decomposition, self-KL, independent and shared mutual information, shape failures, and central finite differences for entropy and cross-entropy gradients. Categorical rewrites require exact denotational equality or an explicit approximate commuting-square relation. Inventory benchmarks require exact bounded comparisons, truncation-mass reporting, policy regret, runtime, and direct source citations for published parameters.

### D-049: Treat polarity and push-pull structure as typed analogies, not one universal duality

**Status:** Accepted

**Decision:** Document state pushforward, payoff pullback, tangent push, cotangent pull, proof-theoretic polarity, game-semantic role reversal, optics, and open-game utility flow in one comparative chapter. Give each operation its own source and target types, required structure, equations, and failure boundaries.

Use the exact state-payoff pairing law to motivate a future finite payoff interpreter. Keep Bayesian inversion prior-indexed and support-restricted. Keep cotangents signed and non-normalized. Do not treat chance as an adversarial player or call an MDP a stochastic game without explicit agents, strategy spaces, information, payoffs, and a solution concept.

Treat game semantics and compositional games as research guides for typed protocol ownership. The current structured-cospan and stochastic-circuit APIs do not acquire arena, strategy, best-response, coutility, innocence, or equilibrium semantics through documentation alone.

**Rationale:** The forward-backward shape recurs across probability, dynamic programming, differentiation, lenses, and games. A typed comparison reveals reusable API structure and pairing tests. Strict separation prevents the same visual reversal from collapsing expectation, inference, differentiation, logical negation, and strategic interaction.

**Consequences:** The book gains a foundation chapter and primary references for focusing, call-by-push-value, predicate transformers, game semantics, polarized games, optics, and open games. S7.3 includes a checked finite payoff pullback and its exact pairing law. Any future game layer must define move ownership, legal interaction, strategy composition, and its observational equality before implementation.

**Required evidence:** The chapter must type every direction-changing operation, distinguish implemented APIs from design signatures, state the state-payoff and tangent-cotangent pairing laws, and include counterclaims for unsupported game semantics. A future payoff interpreter requires exact finite pairing tests. A future game interpreter requires strategy-composition laws and finite interaction fixtures.

## Required evidence for advanced work

| Feature | Required evidence before implementation | Implementation evidence |
| --- | --- | --- |
| Bellman solver | State contraction or properness assumptions. Derive the stopping-error relation. | Residual tests and comparison with an exact finite reference. |
| Q-learning | Define transition sampling, reward timing, schedules, and step limits. | Pure update tests, seeded traces, and scripted behavior tests. |
| Recursion scheme | Give a one-layer functor and termination or productivity argument. | Functor laws and equivalence with a direct interpreter. |
| Categorical compiler | Define source and target categories. Preserve identity, composition, tensor, copy, and discard. | Law tests and shared-draw counterexamples. |
| Kan extension | Name both categories, the generating functor, extension, natural transformation, and universal property. | A derivation reviewed with the compiler laws. |
| Codensity | Prove or test observational equivalence for the supported fragment. | Allocation and runtime benchmarks on bind-heavy programs. |
| Cayley form | State the monoid and floating reassociation policy. | Differential tests and accumulation benchmarks. |
| NBE | Define normal forms, soundness, reification, residualization, and termination boundary. | Correctness tests and generated-code size benchmarks. |
| Matrix diagonalization | State diagonalizability, conditioning, precision, and residual checks. | Differential solver tests and representative benchmarks. |
| GPU backend | Define numeric tolerance, RNG meaning, device subset, and transfer boundary. | CPU and GPU differential tests with transfer-inclusive benchmarks. |
| Neural backend | Define denotation, approximation relation, normalization, detachment, and gradient assumptions. | Finite differences, worked updates, failure atomicity, and exact or tabular differential fixtures. |
| POMDP conditioning | Define observation timing, normalization, and zero-evidence behavior. | Exact finite filtering tests, including impossible observations. |
| Continuous kernel | State measurability, integrability, supported operations, and error semantics. | A use-case decision and reference or statistical validation plan. |

## Decision procedure

1. Add a proposed entry before a semantic implementation.
2. State alternatives, assumptions, and required evidence.
3. Link tests or benchmarks when the decision depends on evidence.
4. Mark the entry accepted only after review records the selected outcome.
5. Add a new entry when later work reverses a recorded decision.
