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

## P2: Bounded interpreters

Risks: reward timing, horizon boundaries, discount placement, and generator ownership can silently diverge between interpreters.

- [ ] **P2.2 `NEXT` Add seeded finite-support sampling.**
  - Receive an explicit seed or generator.
  - Define generator ownership and returned generator state.
  - Sample only positive support exposed by `FiniteDist`.
  - Preserve the exact evaluator's terminal, horizon, reward, and discount semantics.
  - Acceptance: equal seeds produce equal traces and returns.
  - Acceptance: support-membership tests replace frequency thresholds.
  - Acceptance: terminal, horizon-zero, weighted, and self-loop examples pass.
- [ ] **P2.3 Add structured traces and errors.**
  - Include action ID, transition reward, successor state, and stop reason.
  - Keep model, policy, objective, conditioning, and sampling errors distinct.
  - Acceptance: no interpreter uses partial indexing, partial maxima, global randomness, or unchecked normalization.
  - Acceptance: exact expectation of bounded trace observables matches direct evaluation.

P2 gate: exact and sampling interpreters implement the same documented finite objective.

## P3: Finite-state compilation and Bellman solvers

- [ ] **P3.1 Add validated finite state and action indexing.**
  - Reject duplicate indexes and transitions to unindexed states.
  - Cache model and policy validation only after proving equivalence with per-state closure.
- [ ] **P3.2 Add finite-horizon dynamic programming.**
  - Match the exact reference evaluator on finite examples.
  - Report objective, iteration count, and residual where applicable.
- [ ] **P3.3 Add discounted Bellman fixed-point solvers.**
  - Require `ContractionDiscount`.
  - Clamp terminal values to terminal payoffs.
  - State the norm, tolerance, residual, and stopping bound.

P3 gate: compiled and direct exact evaluation agree on every finite reference fixture.

## P4: Learning

- [ ] **P4.1 Specify tabular Q-learning before implementation.**
  - Define Q-table key, learning-rate schedule, exploration schedule, discount, episode limit, and per-episode step limit.
  - Define terminal targets and generator ownership.
- [ ] **P4.2 Implement one pure Q-update.**
  - No partial action maximum.
  - No update from an unavailable action.
  - Acceptance: deterministic algebraic tests cover terminal and continuing targets.
- [ ] **P4.3 Add seeded episodic Q-learning.**
  - Use the validated MDP and sampling interpreter.
  - Return structured traces and final generator state.
  - Acceptance: equal seeds produce equal updates and tables.

P4 gate: learning behavior is explicit, bounded, seeded, and compared with scripted fixtures.

## P5: POMDPs

- [ ] **P5.1 Define observation timing and exact finite beliefs.**
  - Default to observing after transition.
  - Reject zero-evidence conditioning with a structured error.
- [ ] **P5.2 Add exact finite filtering.**
  - Test prediction, conditioning, normalization, and impossible observations.
- [ ] **P5.3 Add belief-state planning only after filtering laws pass.**

## P6: Compiler and accelerated backends

- [ ] **P6.1 Define a typed finite categorical IR.**
  - State source and target categories and preservation laws.
  - Cover shared-draw counterexamples before adding rewrites.
- [ ] **P6.2 Add CPU array lowering.**
  - Define indexing, shapes, precision, sparse/dense policy, and RNG semantics.
- [ ] **P6.3 Add GPU and neural packages outside the semantic core.**
  - Require CPU/GPU differential tests and transfer-inclusive benchmarks.
  - Define approximation, normalization, gradient, and estimator assumptions.

## Admission gates

- Continuous kernels require a concrete use case plus measurability and integration contracts.
- Recursion schemes require explicit recursive syntax and termination or productivity evidence.
- Codensity, Cayley, normalization-by-evaluation, and Kan-extension work require laws and benchmarks.
- New package dependencies require an owned use case, maintenance review, bounds, and CI evidence.
