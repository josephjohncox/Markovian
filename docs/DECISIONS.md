# Decision log

This file records active architecture decisions. Change an accepted decision with a new superseding entry. Do not rewrite its history to hide a reversal.

## Status terms

- **Accepted**: required for target design and new work.
- **Proposed**: review is still open.
- **Deferred**: no implementation is authorized.
- **Rejected**: do not implement without a superseding decision.
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

### D-009: Gate advanced categorical optimization

**Status:** Deferred

**Decision:** Defer Kan extensions, Codensity, Cayley forms, NBE, categorical compilation, and matrix diagonalization.

**Rationale:** The repository has no typed source DSL, optimizer benchmarks, or stable semantic core.

**Consequences:** No named feature is authorized. Each feature needs a specific use case, its proof obligations, and an accepted superseding decision.

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

### D-018: Admit a categorical compiler IR

**Status:** Superseded by D-035 and D-040

**Question:** Does a typed source DSL have a measured need for shared stochastic compilation across two or more backends?

**Required evidence:** Source and target categories, a typed IR sketch, structure laws, random-sharing tests, and a representative workload.

**Superseding effect:** If accepted, D-018 supersedes D-009 only for categorical compiler IR work. D-009 continues to defer other named features.

### D-019: Select the Q-learning contract

**Status:** Superseded by D-033

**Question:** Which schedules, terminal target, exploration rule, seed contract, and step-limit behavior define the replacement learner?

**Required evidence:** A pure update example, configuration errors, scripted traces, and the convergence claims that the API will not make.

### D-020: Select the finite POMDP contract

**Status:** Superseded by D-034

**Question:** Which observation timing, belief representation, and zero-evidence error define the first finite POMDP?

**Required evidence:** Filtering equations and exact examples for normalization and impossible observations.

### D-021: Admit one continuous-kernel use case

**Status:** Proposed

**Question:** Which continuous model and interpreter justify a package outside the finite core?

**Required evidence:** Measurability, integrability, supported operations, errors, and a reference or statistical validation plan.

### D-022: Authorize the bounded Foundation Kickoff core slice

**Status:** Completed and superseded by D-030

**Decision:** The user-authorized Foundation Kickoff can add one semantic-core slice before the blocked P0 baseline completes. The slice uses separate opaque `Double`-backed `Prob` and `Weight` values, scaled floating normalization, and fail-fast structured construction errors. `FiniteDist` preserves labeled duplicate entries and removes zero-weight entries. Exact-reference numeric types remain separate future work.

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

**Rationale:** Exact law tests need literal equality and cannot inherit floating rounding. Finite-horizon evaluation permits a unit discount, while Bellman contraction arguments require a discount below one. A machine-sized horizon would add an unrelated overflow boundary. Separate types make each proof obligation visible in signatures.

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

**Consequences:** The package exposes only the validated semantic modules. The library, sample, and tests depend only on `base` and the local package. Learning returns later under a new explicit contract. D-010, D-022 compatibility limits, and the dependency-workaround portion of D-025 no longer govern current work.

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

**Consequences:** `Markovian.Learning.QLearning` owns finite Q-values, duplicate-free tables, schedules, configuration, and pure updates. `Markovian.Learning.QLearning.Episodic` samples the validated MDP, returns structured episode traces and final generator state, and uses deterministic first-action tie-breaking. Equal seeds produce equal updates and final tables. Statistical frequency thresholds are not correctness gates.

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

**Consequences:** `Markovian.Category.Finite.Exact` is the accepted exact finite IR. `Markovian.Backend.CPU.Exact` lowers it to a source-by-target row-major rational matrix. Source and target support order defines indexes only; dense execution is differentially tested against exact denotation.

**Risk:** This fragment has no recursive syntax, optimizer, sparse storage, or reward annotations. New primitives require preservation laws and cannot blur shared and independent stochastic execution.

### D-036: Keep CUDA and neural contracts in optional backend packages

**Status:** Accepted

**Decision:** CUDA and neural code live under `backends/` in packages separate from the semantic library. The CUDA flag is disabled by default. When enabled, the package uses the CUDA 13 driver API and committed PTX for dense `Double` execution. The neural package selects no framework; it defines stable softmax, analytic probability gradients, score-function estimator assumptions, and max-norm approximation error.

**Rationale:** Core denotation must not depend on devices, memory transfer, tensor frameworks, autodiff, or estimator choices. A disabled-by-default CUDA flag keeps ordinary and hosted builds portable. Actual GPU evidence must compare with a CPU reference and measure transfers, context setup, kernel execution, and cleanup together.

**Consequences:** The NVIDIA GB10 differential fixture reports zero observed error. Neural tests cover normalization, logit-shift invariance, analytic Jacobian row sums, explicit estimator choice, invalid values, and exact-reference approximation.

**Evidence correction, 2026-08-26:** The sample-bearing benchmark runs one excluded warmup before 20 measured runs. A final-correction worktree based on `2efb1c6` measured a `267.843920400 ms` transfer-inclusive mean and `3.025869898 ms` sample standard deviation. The range was `263.519087000 ms` to `276.777522000 ms`, and maximum error was `0.000e0`. The enabled differential test passed.

The [complete evidence record](evidence/CUDA-2026-08-26.md) contains raw samples, commands, tool versions, hashes, and revision context. It retains four older mean-only values as historical execution records because their raw samples and dispersion are unavailable. None of these local values is a general performance claim.

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

**Decision:** Authorize staged implementation of four additive layers: exact finite semiring matrices and their normalized refinements; prior-indexed exact Bayesian operations; purity-indexed finite stochastic circuits and the exact deterministic categorical compiler fragment; and syntax-only finite typed structured-cospan open systems. The matrix foundation uses explicit finite witnesses, exact nonnegative scalars, proof-carrying deterministic arrows, and exact convex enrichment.

D-037 supersedes only D-027's prohibition on a `Monad ExactFiniteDist` instance. D-027's exact-versus-floating law boundary remains active. D-035 supersedes D-018 and D-009 only for the implemented exact categorical compiler fragment. D-009 continues to defer unrelated categorical optimizations.

The design has three distinct reversal-like operations: matrix conjugate transpose, prior-indexed Bayesian inversion, and structured-cospan boundary reversal. No shared `Dagger` class or instance can contain these operations. Arbitrary open-system black-boxing and feedback semantics remain deferred.

**Rationale:** Raw matrices, normalized kernels, Bayesian inversion, and open boundaries have different domains and preservation laws. A transpose need not preserve stochastic normalization. Bayesian inversion depends on a prior and positive support. Boundary reversal swaps cospan feet without reversing directed internal edges. One overloaded operation would make false equations expressible.

**Consequences:** Implementation proceeds through explicit roadmap stages. Empty finite sets are valid for raw matrices, open boundaries, and vacuous normalized arrows from the empty set. A normalized arrow into the empty set exists only when its source is empty. Normalized states, priors, distributions, and existing probability objects remain nonempty. Each finite-witness module exports `sameFiniteLayout` as the canonical layout operation. The descriptive `sameFiniteSetLayout` and `sameFiniteObjectLayout` aliases remain available. Copy naturality is available only through proof-carrying deterministic syntax or matrices. Exact circuit and Bayesian layers can depend on the matrix foundation, but the foundation cannot depend on distributions, POMDPs, circuits, or backends. Open hypergraphs initially have syntax-only semantics except for a separately validated acyclic fragment.

The S1 transpose counterexample proves only that raw transpose does not preserve stochastic normalization. A comparison with prior-indexed Bayesian inversion belongs to S2 because that operation requires a prior, positive support restriction, division, and structured zero-evidence errors. Adding a placeholder inversion to S1 would erase those requirements and assert a false total operation. D-038 separates the names and types now; S2 must supply the Bayesian comparison evidence.

Proof-carrying matrix refinements use nominal roles for scalars and endpoints. Constructor opacity alone is insufficient because representational coercion could otherwise replace the scalar laws or endpoint equality evidence.

**Proof obligations:** Each stage must add literal law tests for the structure that it exposes. The matrix stage must test category, tensor, biproduct, dagger, compact, trace, normalization, determinism, convexity, and a transpose-normalization counterexample. Bayesian work must test support-restricted Bayes laws and preserve existing POMDP behavior differentially. Circuit work must preserve stochastic sharing and reject dishonest deterministic tags. Open-system work must validate pushouts and 2-cells up to explicit canonical isomorphism and must reject unsupported feedback.

**Deferred:** A total black-box functor from arbitrary open hypergraphs to stochastic kernels, stationary feedback selection, unrelated NBE or Kan-extension optimizations, and a common dagger abstraction are not authorized.

### D-039: Restrict exact Bayesian inversion to positive prior support

**Status:** Accepted

**Decision:** An exact `Prior` is a normalized state on an explicit nonempty finite object. It stores its positive `Support`. Pushforward, joint construction, evidence, and conditioning use exact nonnegative rational matrices. A `BayesianInverse source target` contains a normalized map from positive output support to positive input support. It does not choose rows for zero-evidence outputs.

`almostSureEqual prior left right` is the explicit prior-indexed equality operation. Ordinary `Eq` does not express almost-sure equality. A `BayesianChannel` stores its input prior, forward channel, and exact pushforward prior. Composition checks that the second input prior equals the first output prior. It is not a `Category` over plain endpoint types.

**Rationale:** The equation `p(x) K(x,y) / q(y)` is defined only when `q(y)` is positive. Arbitrary rows on zero-output support are not Bayesian data. Support restriction gives a total normalized inverse on its mathematical domain and makes the almost-sure uniqueness boundary explicit.

**Consequences:** Prior-indexed Bayesian inversion remains distinct from matrix conjugate transpose and structured-cospan boundary reversal. The package adds no shared `Dagger` class and no `Dagger ExactKernel` instance. Exact POMDP prediction and conditioning delegate to the Bayesian distribution algebra while preserving post-transition timing, duplicate aggregation, support order, posterior values, and `ImpossibleExactObservation`.

**Proof obligations:** Exact tests cover normalization, support extraction, the Bayes joint equation, identity, composition reversal, independent tensor, double inversion after support restriction, almost-sure equivalence, zero-evidence errors, and checked Bayesian-channel prior flow. Floating inference remains outside these literal law claims.

### D-040: Reify purity-indexed stochastic circuits and first-order deterministic compilation

**Status:** Accepted

**Decision:** `Circuit primitive purity source target` is opaque recursive syntax. `purity` records deterministic or stochastic construction provenance. Identity, composition, tensor, symmetry, associators, unitors, copy, discard, validated finite tables, convex choice, sharing, and fanout remain explicit nodes. `share circuit` executes once and copies one result. `fanout left right` copies the input and executes both branches independently conditional on that input.

Only deterministic syntax can use the copy-naturality rewrite. Deterministic primitives must interpret to `DeterministicMatrix`; stochastic primitives interpret to `StochasticMatrix`. The only purity cast weakens deterministic syntax to stochastic syntax. Compile-fail evidence rejects strengthening and deterministic copy optimization on stochastic circuits.

`CircuitAlgebra` is an unchecked fold-operation record. It carries no categorical laws. `foldCircuit` derives sharing from composition and copy, and derives fanout from copy, tensor, and composition. An algebra that claims homomorphism laws must prove them separately. The exact algebra targets proof-carrying deterministic and normalized stochastic nonnegative-rational matrices. Exact kernel application and dense CPU lowering use that same matrix denotation. Floating, CUDA, and neural backends must use the separate approximation boundary with an observation relation, precision, and error policy.

The deterministic source fragment contains identity, composition, products, pairing, projections, and finite quoted tables. Projection compilation uses discard and unitors. The source contains no Haskell function values or function equality.

**Rationale:** A provenance index prevents a currently Dirac stochastic gate from authorizing copy rewrites. Reified sharing prevents accidental duplication of random effects. Finite tables give total inspectable first-order compilation without pretending that arbitrary Haskell functions form a quoted language.

**Consequences:** Recursive circuits are supported, but recursive circuit definitions can diverge before construction and no feedback node is provided. The syntax is not claimed to be a quotient or a mechanically proved initial object. Arbitrary Haskell functions, bottoms, exceptions, `seq`, opaque higher-order functions, infinite types, finite exponentials, and cartesian closure of stochastic maps remain unsupported. Open hypergraphs and structured cospans remain S5 work.

**Proof obligations:** Exact tests cover structural-fold preservation, derived sharing and fanout, deterministic-only copy naturality, convex choice, pentagon, triangle, symmetry hexagon, structural naturality, both unitors, reordered layouts, source compilation equations, validation failures, and differential agreement with `denoteExactIR`, `lowerExactIR`, and dense CPU rows.

### D-041: Implement typed structured-cospan syntax and only decorated circuit denotation

**Status:** Accepted

**Decision:** Open syntax uses finite typed interfaces and directed labelled hypergraphs. Hyperedges have identities and ordered typed input and output ports. Interface maps and hypergraph maps are total, type preserving, and opaque. Structured cospans have total, not necessarily injective, legs from discrete boundary hypergraphs.

Horizontal composition constructs the finite pushout of apex vertices along the shared discrete boundary. The pushout exposes explicit quotient classes, canonical injections, and checked cocone factorization. Members of each binary quotient class are stored in left-carrier order followed by right-carrier order. Cocones and middle vertical arrows are compared extensionally, so reordered but typed-support-equivalent interfaces do not block factorization or horizontal cell composition. Hyperedges combine by disjoint union because the glued boundary is discrete. Tensor is disjoint union. Boundary reversal swaps the two cospan legs without changing the apex, hyperedges, labels, or the directed circuit state orientation.

The implemented double fragment has interfaces as objects, interface maps as vertical arrows, structured cospans as horizontal arrows, and commuting squares as 2-cells. A 2-cell contains both vertical boundary maps and a type-, label-, order-, and incidence-preserving apex hypergraph map. Vertical composition is map composition. Horizontal composition is the induced pushout map. Tensor acts componentwise.

`OpenCircuit` is a structured cospan with a global directed circuit decoration. Sequential gluing composes both topology and decoration; tensor combines both. Only the circuit decoration has exact stochastic denotation. Hypergraph labels and internal topology are not black-boxed. Boundary reversal exchanges only the topological boundary parameters. It retains the original circuit state input and output parameters and returns a view with no reversed stochastic-denotation observer.

**Rationale:** Finite pushouts justify structured-cospan composition and the double cells. A separate global decoration permits exact composition tests without claiming an unimplemented semantics for arbitrary directed hypergraphs, cycles, feedback, MDPs, or continuous-time open Markov processes.

**Consequences:** Structured-cospan boundary reversal is distinct from matrix conjugate transpose and prior-indexed Bayesian inversion. No common `Dagger` class or instance is added. Binary quotient classes have an implemented canonical order. Nested associativity is not literal equality because the carrier types differ. Representative tests construct the canonical member-flattening isomorphism; this is not an exported general associator, unitor, or coherence theorem. The implementation does not claim a strict double category, a general bicategorical coherence theorem, graph black-boxing, feedback semantics, or existing continuous-time open-Markov theorems for MDPs.

**Proof obligations:** Tests cover interface-map row canonicalization, hypergraph validation, nominal pushout witnesses, noninjective pushout quotients, canonical class order, layout-independent cocone factorization, gluing, disjoint-union tensor, reversal with unchanged directed state orientation, both unitor isomorphism fixtures, both associator round trips on vertex and edge maps, extensionally matched horizontal cell composition, interchange, and exact decorated-circuit composition, tensor, associativity, and units.

## Proof obligations for advanced work

| Feature | Proof obligation before implementation | Evidence before acceptance |
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
| Neural backend | Define denotation, approximation relation, normalization, and gradient assumptions. | Calibration or error metrics and estimator bias or variance analysis. |
| POMDP conditioning | Define observation timing, normalization, and zero-evidence behavior. | Exact finite filtering tests, including impossible observations. |
| Continuous kernel | State measurability, integrability, supported operations, and error semantics. | A use-case decision and reference or statistical validation plan. |

## Decision procedure

1. Add a proposed entry before a semantic implementation.
2. State alternatives, assumptions, and proof obligations.
3. Link tests or benchmarks when the decision depends on evidence.
4. Mark the entry accepted only after review.
5. Add a new entry when later work reverses an accepted decision.
