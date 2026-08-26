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
  - Local CUDA evidence on NVIDIA GB10: CPU/GPU differential error `0.000e0`; transfer-inclusive 256-by-256 mean `295.110287 ms` over 20 runs.

P6 gate: `DONE`. Exact IR laws, shared-draw counterexamples, dense CPU differential tests, actual CUDA differential execution, transfer-inclusive benchmarking, and neural normalization and gradient contracts pass.

## Admission gates

- Continuous kernels require a concrete use case plus measurability and integration contracts.
- Recursion schemes require explicit recursive syntax and termination or productivity evidence.
- Codensity, Cayley, normalization-by-evaluation, and Kan-extension work require laws and benchmarks.
- New package dependencies require an owned use case, maintenance review, bounds, and CI evidence.
