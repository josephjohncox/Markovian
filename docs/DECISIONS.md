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

**Status:** Accepted

**Decision:** Add the new core before changing old exports. Add two explicitly named legacy adapters and no generic adapter.

**Rationale:** Legacy evaluation and legacy Q-learning assign incompatible meanings to `Action`.

**Consequences:** Keep compatibility through the 0.2 series. Remove it only in a PVP-major release after the stated support period.

### D-011: Use transition rewards and terminal payoffs

**Status:** Accepted

**Decision:** The target model assigns reward to each transition. A terminal state has one optional terminal payoff.

**Rationale:** This convention states reward timing for MRP, MDP, policy closure, Bellman equations, and learning targets.

**Consequences:** Migration adapters must map legacy state rewards explicitly. Interpreters apply transition reward once and terminal payoff once.

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

**Status:** Proposed

**Question:** Does a typed source DSL have a measured need for shared stochastic compilation across two or more backends?

**Required evidence:** Source and target categories, a typed IR sketch, structure laws, random-sharing tests, and a representative workload.

**Superseding effect:** If accepted, D-018 supersedes D-009 only for categorical compiler IR work. D-009 continues to defer other named features.

### D-019: Select the Q-learning contract

**Status:** Proposed

**Question:** Which schedules, terminal target, exploration rule, seed contract, and step-limit behavior define the replacement learner?

**Required evidence:** A pure update example, configuration errors, scripted traces, and the convergence claims that the API will not make.

### D-020: Select the finite POMDP contract

**Status:** Proposed

**Question:** Which observation timing, belief representation, and zero-evidence error define the first finite POMDP?

**Required evidence:** Filtering equations and exact examples for normalization and impossible observations.

### D-021: Admit one continuous-kernel use case

**Status:** Proposed

**Question:** Which continuous model and interpreter justify a package outside the finite core?

**Required evidence:** Measurability, integrability, supported operations, errors, and a reference or statistical validation plan.

### D-022: Authorize the bounded Foundation Kickoff core slice

**Status:** Accepted

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

**Rationale:** GHC 9.4.8 tests the declared `base-4.17` lower boundary. GHC 9.8.4 matches the development environment. Commit pins prevent mutable action tags from changing executable CI code. Fourmolu and cabal-fmt require separate installation plans because their `Cabal-syntax` constraints conflict when Cabal solves them together.

**Consequences:** The build matrix runs package checks, `-Werror` builds, tests, warning-free Haddocks, and source-distribution creation. The format job installs Fourmolu from GHCup's third-party channel and installs cabal-fmt separately through Cabal. The source distribution includes the workflow.

**Risk:** Local simulation cannot prove that GitHub's hosted image, network, or action runtime works. P0.1 remains active until a successful workflow run has a durable URL.

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
