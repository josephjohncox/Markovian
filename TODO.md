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
- [x] **Documentation stack.**
  - Added a pinned mdBook user and contributor guide for the complete public semantic stack.
  - Added repository-owned link, include, math-delimiter, local-MathJax digest, source-to-HTML equation-count, version, and HTML build checks.
  - Added foundation chapters for algebra, category theory, measure theory, categorical probability, information theory, and categorical learning.
  - Added an equation-level law catalogue, executable fixture links, derived mathematical insights, guided reading routes, and an annotated bibliography.
  - Added browser-startup recovery and validation for TeX altered by Markdown emphasis or smart punctuation.
  - Added the book check to CI, Pages, and the source-distribution manifest.
- [x] **GitHub Pages publication.** GitHub Actions publishes successful `main` documentation builds at <https://josephjohncox.github.io/Markovian/>.

## P2: Bounded interpreters

Risks: reward timing, horizon boundaries, discount placement, and generator ownership can silently diverge between interpreters.

- [x] **P2.2 Add seeded finite-support sampling.**
  - Receive an explicit seed or generator.
  - Define generator ownership and returned generator state.
  - Sample only positive support exposed by `FiniteDist`.
  - Preserve the exact evaluator's terminal, horizon, reward, and discount semantics.
  - Required evidence: equal seeds produce equal traces and returns.
  - Required evidence: support-membership tests replace frequency thresholds.
  - Required evidence: terminal, horizon-zero, weighted, and self-loop examples pass.
- [x] **P2.3 Add structured traces and errors.**
  - Include action ID, transition reward, successor state, and stop reason.
  - Keep model, policy, objective, conditioning, and sampling errors distinct.
  - Required evidence: no interpreter uses partial indexing, partial maxima, global randomness, or unchecked normalization.
  - Required evidence: exact expectation of bounded trace observables matches direct evaluation.

P2 status: `DONE`. Exact and sampling interpreters implement the same terminal-before-horizon, reward-timing, and discount semantics.

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
- [x] **P3.4 Add policy-free exact compilation and exact control.**
  - Compile all model-available actions and preserve joint outcomes.
  - Close a compiled policy once before policy evaluation.
  - Add bounded rational value iteration with residual and policy bounds.
  - Add bounded deterministic policy iteration with rational linear solves.

P3 status: `DONE`. Exact tests cover policy-free compilation, control bounds, deterministic ties, permutations, and agreement between policy and value iteration.

## P4: Learning

- [x] **P4.1 Specify tabular Q-learning before implementation.**
  - Define Q-table key, learning-rate schedule, exploration schedule, discount, episode limit, and per-episode step limit.
  - Define terminal targets and generator ownership.
- [x] **P4.2 Implement one pure Q-update.**
  - No partial action maximum.
  - No update from an unavailable action.
  - Required evidence: deterministic algebraic tests cover terminal and continuing targets.
- [x] **P4.3 Add seeded episodic Q-learning.**
  - Use the validated MDP and sampled-step interpreter.
  - Return structured traces and final generator state.
  - Required evidence: equal seeds produce equal updates and tables.
- [x] **P4.4 Add shared tabular learning foundations.**
  - Share Q-values, V-values, observations, rates, schedules, and epsilon-greedy behavior.
  - Sample each joint reward-successor outcome through one explicit-generator step.
- [x] **P4.5 Add TD(0), SARSA, and Expected SARSA.**
  - Keep terminal bootstraps separate from continuing bootstraps.
  - Preserve on-policy carried-action and expected-distribution semantics.
  - Add bounded resumable episodic runners for all four tabular methods.

P4 status: `DONE`. Deterministic tests cover distinct continuing targets, common terminal timing, seeded behavior, carried actions, bounds, failures, and split-run equality.

## P5: POMDPs

- [x] **P5.1 Define observation timing and exact finite beliefs.**
  - Default to observing after transition.
  - Reject zero-evidence conditioning with a structured error.
- [x] **P5.2 Add exact finite filtering.**
  - Test prediction, conditioning, normalization, and impossible observations.
- [x] **P5.3 Add belief-state planning only after filtering laws pass.**

P5 status: `DONE`. Prediction, post-transition conditioning, impossible observations, normalization, and bounded belief planning pass exact deterministic fixtures.

## P6: Compiler and accelerated backends

- [x] **P6.1 Define a typed finite categorical IR.**
  - State source and target categories and preservation laws.
  - Cover shared-draw counterexamples before adding rewrites.
- [x] **P6.2 Add CPU array lowering.**
  - Define indexing, shapes, precision, sparse/dense policy, and RNG semantics.
- [x] **P6.3 Add GPU and neural packages outside the semantic core.**
  - Require CPU/GPU differential tests and transfer-inclusive benchmarks.
  - Define approximation, normalization, gradient, and estimator assumptions.
  - The neural package now includes checked dense networks, approximate categorical information quantities and gradients, sized structural action masks, linear policy-gradient updates, replay, target networks, and one DQN batch update.
  - A separate bridge checks exact global action layouts against actual neural heads and compiles exact availability order without numeric masking or fabricated terminal masks.
  - Neural evidence uses hand calculations, finite differences, and deterministic differential fixtures. It makes no training-performance claim.
  - The 2026-08-26 sample-bearing run used one excluded warmup and 20 measured runs on an NVIDIA GB10.
  - The transfer-inclusive mean was `267.843920400 ms`, and sample standard deviation was `3.025869898 ms`.
  - The [evidence record](docs/evidence/CUDA-2026-08-26.md) retains raw samples and older mean-only execution records.
  - These local measurements are not general performance claims.
  - Historical pre-bridge three-package evidence: <https://github.com/josephjohncox/Markovian/actions/runs/32998596001>.

P6 status: `DONE`. Exact IR, CUDA, neural numerical, policy-gradient, replay, target-network, and DQN fixtures are implemented. Current local validation status belongs in `docs/CONTEXT.md`.

## Exact semantic tower roadmap

D-038 defines this roadmap in dependency order. A later stage cannot weaken the exact-law boundary established by an earlier stage.

- [x] **S1 Algebraic foundation.** Add duplicate-free finite sets, nonempty finite objects, lawful scalar classes, exact nonnegative rationals, opaque raw matrices, stochastic and deterministic refinements, and exact convex enrichment. Each finite-witness module exports `sameFiniteLayout` and retains its descriptive alias. Nominal roles protect proof refinements, and convex validation uses a fixed exact sum. Tests cover the canonical layout API, representative scalar fixtures, nontrivial and noncommutative dagger fixtures, biproduct decomposition, compact and trace laws, stochastic closure, deterministic subcategory and embedding laws, convex laws, and the transpose-normalization counterexample.
- [x] **S2 Exact Bayesian layer.** Added normalized priors, positive support restriction, exact pushforward and joints, structured conditioning, prior-indexed Bayesian inversion, almost-sure equality, checked Bayesian-channel composition, and differential POMDP integration. Exact tests cover joint balance, identity, composition reversal, tensor, double inversion, zero-evidence behavior, and almost-sure uniqueness.
- [x] **S3 Purity-indexed stochastic circuits.** Added recursive deterministic and stochastic syntax, structural folds, exact matrix and kernel interpretation, explicit sharing and fanout, deterministic-only copy optimization, exact convex choice, dense CPU lowering, an approximation boundary, differential legacy-IR tests, and purity compile-fail evidence.
- [x] **S4 Deterministic categorical compiler.** Added the supported first-order fragment with identity, composition, products, pairing, projections, and finite quoted tables. Compilation tests cover composition, tensor, pairing, projections, and independent finite-table denotation.
- [x] **S5 Structured-cospan open syntax.** Added finite typed interfaces, directed labelled hypergraphs, total structure maps, explicit quotient pushouts and cocone factors, structured-cospan gluing, disjoint-union tensor, boundary reversal, and commuting open-system cells with vertical, horizontal, tensor, and interchange evidence.
- [x] **S6 Partial acyclic open interpretation.** Added opaque boundary-functional DAG validation, stable topological schedules, cycle witnesses, and named finite assignments. Added signature-keyed local circuits, aggregate purity, and exact live-frontier matrix interpretation. Bounded differential tests compare this interpretation with the complete-valuation equation and direct matrix paths. Tests distinguish stored sharing, independent execution, and discard. `OpenCircuit` remains the unchanged global-decoration API. Arbitrary cyclic graphs, feedback, continuous-time black-boxing, and unrestricted MDP black-boxing remain deferred.

S6 status: `DONE`.

## Integration status

D-053 through D-060 are `DONE` and `Accepted`. Both supported compilers, `--prefer-oldest`, warning-free Haddock, formatting, all four source archives and unpacked tests, every compile-fail boundary, all four inventory benchmarks, and deterministic semantic-report stability passed locally and in hosted CI run `33467147313` on revision `993508f`.

## S7: Approximation and categorical-learning case studies

- [x] **S7.1 Add information-theory, categorical-learning, polarity, and game-semantic foundations.**
  - Separate entropy, divergence, reverse derivatives, linear adjoints, adjoint functors, Bayesian inversion, and optimizer dynamics.
  - Separate state pushforward, payoff pullback, tangent push, cotangent pull, logical polarity, and Player-Opponent polarity.
  - Add checked categorical entropy, cross entropy, KL divergence, mutual information, and analytic logit gradients outside the exact core.
  - Ground the game-semantic material in arena, strategy, focusing, optic, and open-game references without claiming an implementation.
  - Require decomposition, product, invariance, pairing, and finite-difference evidence for future executable interpreters.
- [x] **S7.2 Add reproducible inventory-control benchmarks.**
  - [x] Add a bounded synthetic two-echelon serial fixture with explicit one-period supplier delay, event timing, successor costs, conditioned geometric demand, and truncation-mass reporting.
  - [x] Compare period-specific bounded base-stock schedules with exact finite-horizon backward induction, retain opaque solution provenance, validate order-cap and target-set widening, and report exact regret, model size, solver status, and stability diagnostics.
  - [x] Add a deterministic semantic report and a one-warm-up, twenty-sample reproducible benchmark with raw timing samples and sample statistics.
  - [x] Add the source-crosswalked Clark--Scarf (1960), Section III, finite lattice specialization with exact equations (14), (15), (20), (21), and (26), zero-regret policy evaluation, explicit layouts and budgets, retained-mass reporting, and widened-cap diagnostics.
  - [x] Encode Doğru's one-warehouse multi-retailer balance assumption as a named relaxation, not as the physical transition model.
  - [x] Compare the relaxed policy with a bounded physical allocation model and report value error, policy regret, state count, runtime, and truncation mass.
  - [x] Add the fixed-batch `(R,nQ)` newsvendor equations as a separate benchmark with discrete-demand inequalities.
  - [x] Pin every published parameter table or state clearly; otherwise state that a fixture is synthetic.
- [x] **S7.3 Add categorical cost, payoff, and rewrite interpreters.**
  - [x] Add a checked exact rational finite payoff, payoff pullback through exact stochastic matrices, and exact state-payoff pairing evidence.
  - [x] Fold circuit syntax into bounded primitive, table, owner-work, structural-operation, represented-layout, and matrix-cell reports.
  - [x] Add opaque identity, reassociation, and deterministic fanout-to-share candidates; keep stochastic syntax excluded at construction.
  - [x] Issue checked witnesses only after bounded exact endpoint-layout and row-major matrix checks. Keep the checked-`Double` softmax/cross-entropy fusion separate and certificate-free.
  - [x] Produce deterministic cost/rewrite reports. Defer timing until a named optimizer and workload make a performance claim.
- [x] **S7.4 Add typed parametric reverse circuits.**
  - [x] Add a small framework-independent neural-backend interpreter with explicit nested-pair parameter products, distinct primal and cotangent types, cotangent-module witnesses, captured primitive pullbacks, input and parameter diagonal accumulation, exact module/VJP laws, and finite-difference fixtures.
  - [x] Add D-058's finite acyclic syntax over caller-owned primitives, structural parameter ownership, checked finite primal and cotangent layouts, bounded preparation, and opaque stored or recomputed tapes.
  - [x] Check exact `Rational` composition, tensor, and diagonal fixtures, deterministic reports, compile-fail boundaries, and every input and parameter coordinate of a heterogeneous `Double` program under both tape policies.
  - Keep optimizer state separate from differentiated circuits. General autodiff lowering, recursion, cycles, tensors, and checkpoint scheduling remain out of scope. Do not identify reverse differentiation with Bayesian inversion or matrix dagger.
- [x] **S7.5 Add finite typed interaction protocols.**
  - [x] Add finite reachable acyclic arenas with explicit move ownership, alternation, opaque legal-history replay, and separate labelled and represented-layout equality.
  - [x] Add bounded prefix-closed strategies with exhaustive Opponent receptivity, one total Player response, copycat, partial synchronized hidden-middle composition, and exact external-prefix observational equality.
  - [x] Add ownership, illegal-history, prefix-closure, budget, layout, representative identity/associativity, congruence, deterministic-report, and compile-fail evidence.
  - Keep the result scoped to finite alternating protocols. It does not establish Hyland--Ong or AJM semantics, open games, equilibria, chance, or multi-agent stochastic games.

- [x] **S7.6 Add owner-refined finite open games.**
  - [x] Add bounded total finite functions, concrete optics, owner-disjoint strategy products, play, coplay, sequential and tensor composition, and context-indexed best responses.
  - [x] Add exact `Rational` maximizing decisions, pure contextual equilibrium enumeration with all ties, owner-preserving observational equality, deterministic reports, and explicit layout diagnostics.
  - [x] Differential-test every represented two-player `2 x 2` payoff table over `{0,1}` and retain matching-pennies and non-credible-threat counterexamples.
  - Keep arena histories separate. Do not claim mixed, correlated, repeated, stochastic, Bayesian, continuous, subgame-perfect, or equilibrium-existence results.

S7 status: `DONE` for the bounded scopes defined by D-048 through D-060. The research boundaries listed below remain out of scope.

## Requirements for new work

- Continuous kernels require a concrete use case plus measurability and integration contracts.
- Recursion schemes require explicit recursive syntax and termination or productivity evidence.
- Codensity, Cayley, normalization-by-evaluation, and Kan-extension work require laws and benchmarks.
- New package dependencies require an owned use case, maintenance review, bounds, and CI evidence.
