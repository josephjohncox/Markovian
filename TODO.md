# Markovian implementation plan

Status terms: `DONE`, `NEXT`, `READY`, `BLOCKED`.

## Completed foundation

- [x] **P0 Reproducible package and CI baseline.**
  - GHC 9.4.8 and 9.8.4 build project code with `-Werror`.
  - Package checks, HLint, Fourmolu, cabal-fmt, warning-free Haddock, lower-bound resolution, and unpacked source-distribution tests pass.
  - GitHub Actions and tool versions are pinned.
- [x] **P1 Valid semantic core.**
  - Floating and exact probability, reward, objective, kernel, MRP, MDP, and policy types have opaque validated boundaries.
  - Action IDs are separate from stochastic transition outcomes.
  - Policy closure validates unique available actions, unique policy support, and action availability.
  - Exact distributions satisfy functor and Kleisli laws literally.
- [x] **P2.1 Exact finite-horizon expectation.**
  - The evaluator receives an exact model, policy, horizon, and discount.
  - Terminal payoff, horizon zero, reward timing, weighted support, error propagation, and bounded self-loops have deterministic tests.
  - Hosted evidence: <https://github.com/josephjohncox/Markovian/actions/runs/32907754545>.
- [x] **Greenfield cleanup.**
  - Removed the ambiguous branch-weight process and defective Q-learning implementation.
  - Removed compatibility adapters, deprecation phases, and third-party dependencies required only by deleted code.
  - Replaced legacy examples and generated changelog text.
  - Hosted evidence: <https://github.com/josephjohncox/Markovian/actions/runs/32920753099>.

## P2: Bounded interpreters

Risks: reward timing, horizon boundaries, discount placement, and generator ownership can silently diverge between interpreters.

- [x] **P2.2 Add seeded finite-support sampling.**
  - Receive an explicit seed or generator.
  - Define generator ownership and returned generator state.
  - Sample only positive support exposed by `FiniteDist`.
  - Preserve the exact evaluator's terminal, horizon, reward, and discount semantics.
  - Acceptance: equal seeds produce equal traces and returns.
  - Acceptance: support-membership tests replace frequency thresholds.
  - Acceptance: terminal, horizon-zero, weighted, and self-loop examples pass.
- [x] **P2.3 Add structured traces and errors.**
  - Include action ID, transition reward, successor state, and stop reason.
  - Keep model, policy, objective, conditioning, and sampling errors distinct.
  - Acceptance: no interpreter uses partial indexing, partial maxima, global randomness, or unchecked normalization.
  - Acceptance: exact expectation of bounded trace observables matches direct evaluation.

P2 gate: `DONE`. Exact and sampling interpreters implement the same terminal-before-horizon, reward-timing, and discount semantics.

## P3: Finite-state compilation and Bellman solvers

- [x] **P3.1 Add validated finite state and action indexing.**
  - Reject duplicate indexes and transitions to unindexed states.
  - Cache model and policy validation only after proving equivalence with per-state closure.
- [x] **P3.2 Add finite-horizon dynamic programming.**
  - Match the exact reference evaluator on finite examples.
  - Report objective, iteration count, and residual where applicable.
- [x] **P3.3 Add discounted Bellman fixed-point solvers.**
  - Require `ContractionDiscount`.
  - Clamp terminal values to terminal payoffs.
  - State the norm, tolerance, residual, and stopping bound.

P3 gate: `DONE`. Compiled closure and finite dynamic programming agree with direct exact evaluation on finite reference fixtures. The exact Bellman solver reports a sup-norm residual and contraction stopping bound.

## P4: Learning

- [x] **P4.1 Specify tabular Q-learning before implementation.**
  - Define Q-table key, learning-rate schedule, exploration schedule, discount, episode limit, and per-episode step limit.
  - Define terminal targets and generator ownership.
- [x] **P4.2 Implement one pure Q-update.**
  - No partial action maximum.
  - No update from an unavailable action.
  - Acceptance: deterministic algebraic tests cover terminal and continuing targets.
- [x] **P4.3 Add seeded episodic Q-learning.**
  - Use the validated MDP and sampling interpreter.
  - Return structured traces and final generator state.
  - Acceptance: equal seeds produce equal updates and tables.

P4 gate: `DONE`. Learning behavior is explicit, bounded, seeded, and compared with deterministic scripted fixtures.

## P5: POMDPs

- [x] **P5.1 Define observation timing and exact finite beliefs.**
  - Default to observing after transition.
  - Reject zero-evidence conditioning with a structured error.
- [x] **P5.2 Add exact finite filtering.**
  - Test prediction, conditioning, normalization, and impossible observations.
- [x] **P5.3 Add belief-state planning only after filtering laws pass.**

P5 gate: `DONE`. Prediction, post-transition conditioning, impossible observations, normalization, and bounded belief planning pass exact deterministic fixtures.

## P6: Compiler and accelerated backends

- [x] **P6.1 Define a typed finite categorical IR.**
  - State source and target categories and preservation laws.
  - Cover shared-draw counterexamples before adding rewrites.
- [x] **P6.2 Add CPU array lowering.**
  - Define indexing, shapes, precision, sparse/dense policy, and RNG semantics.
- [x] **P6.3 Add GPU and neural packages outside the semantic core.**
  - Require CPU/GPU differential tests and transfer-inclusive benchmarks.
  - Define approximation, normalization, gradient, and estimator assumptions.
  - The 2026-08-26 sample-bearing run used one excluded warmup and 20 measured runs on an NVIDIA GB10.
  - The transfer-inclusive mean was `267.843920400 ms`, and sample standard deviation was `3.025869898 ms`.
  - The [evidence record](docs/evidence/CUDA-2026-08-26.md) retains raw samples and older mean-only execution records.
  - These local measurements are not general performance claims.
  - Hosted three-package evidence: <https://github.com/josephjohncox/Markovian/actions/runs/32998596001>.

P6 gate: `DONE`. Exact IR laws, full-tensor copy, symmetric monoidal coherence maps, deterministic copy naturality, shared-draw counterexamples, dense CPU differential tests, actual CUDA differential execution, transfer-inclusive benchmarking, and neural normalization and gradient contracts pass.

## Exact semantic tower roadmap

D-038 authorizes this roadmap in dependency order. A later stage cannot weaken the exact-law boundary established by an earlier stage.

- [x] **S1 Algebraic foundation.** Add duplicate-free finite sets, nonempty finite objects, lawful scalar classes, exact nonnegative rationals, opaque raw matrices, stochastic and deterministic refinements, and exact convex enrichment. Each finite-witness module exports `sameFiniteLayout` and retains its descriptive alias. Nominal roles protect proof refinements, and convex validation uses a fixed exact sum. Tests cover the canonical layout API, representative scalar fixtures, nontrivial and noncommutative dagger fixtures, biproduct decomposition, compact and trace laws, stochastic closure, deterministic subcategory and embedding laws, convex laws, and the transpose-normalization counterexample.
- [x] **S2 Exact Bayesian layer.** Added normalized priors, positive support restriction, exact pushforward and joints, structured conditioning, prior-indexed Bayesian inversion, almost-sure equality, checked Bayesian-channel composition, and differential POMDP integration. Exact tests cover joint balance, identity, composition reversal, tensor, double inversion, zero-evidence behavior, and almost-sure uniqueness.
- [x] **S3 Purity-indexed stochastic circuits.** Added recursive deterministic and stochastic syntax, structural folds, exact matrix and kernel interpretation, explicit sharing and fanout, deterministic-only copy optimization, exact convex choice, dense CPU lowering, an approximation boundary, differential legacy-IR tests, and purity compile-fail evidence.
- [x] **S4 Deterministic categorical compiler.** Added the approved first-order fragment with identity, composition, products, pairing, projections, and finite quoted tables. Compilation tests cover composition, tensor, pairing, projections, and independent finite-table denotation.
- [x] **S5 Structured-cospan open syntax.** Added finite typed interfaces, directed labelled hypergraphs, total structure maps, explicit quotient pushouts and cocone factors, structured-cospan gluing, disjoint-union tensor, boundary reversal, and commuting open-system cells with vertical, horizontal, tensor, and interchange evidence.
- [x] **S6 Partial acyclic open interpretation.** Added opaque boundary-functional DAG validation, stable topological schedules, cycle witnesses, and named finite assignments. Added signature-keyed local circuits, aggregate purity, and exact live-frontier matrix interpretation. Bounded differential tests compare this interpretation with the complete-valuation equation and direct matrix paths. Tests distinguish stored sharing, independent execution, and discard. `OpenCircuit` remains the unchanged global-decoration API. Arbitrary cyclic graphs, feedback, continuous-time black-boxing, and unrestricted MDP black-boxing remain deferred.

S6 gate: `DONE`.

**NEXT:** Select a new task that passes the admission gates. No post-S6 implementation stage is currently authorized.

## Admission gates

- Continuous kernels require a concrete use case plus measurability and integration contracts.
- Recursion schemes require explicit recursive syntax and termination or productivity evidence.
- Codensity, Cayley, normalization-by-evaluation, and Kan-extension work require laws and benchmarks.
- New package dependencies require an owned use case, maintenance review, bounds, and CI evidence.
