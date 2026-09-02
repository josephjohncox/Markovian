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
  - Exact distributions satisfy functor laws and admitted checked-bind identity and associativity literally; the checked resource API is not a `Monad`.
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

## Checked finite feedback

- [ ] **S8 Explicit bounded feedback fragments (Partial).** Restricted source, operation accounting, phase-specific rational maxima, exact and one-below limits, discarded-intermediate counterexamples, and focused evidence exist under proposed D-069. Complete archive, lower-bound, hosted, and immutable-revision evidence remains open. Universal trace, arbitrary cyclic circuits/open systems, and rewardful cyclic finite-support claims remain blocked.

## Optional continuous probability

- [ ] **S9 Restricted continuous probability (Partial).** Two optional packages contain restricted exact and numerical paths under proposed D-070 and D-071. The bounded bivariate algorithm now has cumulative symbolic and rational accounting, independent exact oracles, and all-coordinate fixture differentials. These fixtures do not establish general sampler correctness, certified floating bounds, or multidimensional cubature. Fresh archive, lower-bound, supported-compiler, and hosted gates remain open. Arbitrary measurable callbacks, point conditioning, continuous disintegration, continuous MDP execution, and the `hmatrix` bridge remain blocked.

## Optional host tensor runtime

- [ ] **S10 Checked host F64 tensors (Partial).** Optional `markovian-tensor` has a restricted host F64 path under proposed D-072. Shape admission, primitive differentials, deterministic allocation/cleanup fault injection, atomic runtime-shape batches, and report evidence are present. Full supported-compiler, integration, archive, and hosted evidence remains open. General tensor semantics, arbitrary strides, broadcasting, mutation, and performance claims remain blocked.

## Optional CUDA device fragment

- [ ] **S11 Checked matrix/VJP CUDA executor (Blocked).** Proposed D-074 has an owned dynamic CUDA 13 driver table, strict ABI checks, explicit pre-launch fallback boundaries, and a digest-pinned no-GPU compile workflow. Device execution remains blocked until archive-only enabled compilation, the hosted compile-only receipt, protected UUID-bound hardware, all four Compute Sanitizer tools, and complete CI evidence pass together. Local fault fixtures do not establish general device correctness. Generic tensor graphs, generic reverse programs, other dtypes, arbitrary devices, bitwise reproducibility, and GPU advantage remain blocked.

- [ ] **S11.1 SafeTensors profile (Partial).** `markovian-safetensors` implements only the pinned bounded metadata-free F64 profile. Focused canonical encoding, duplicate-preserving parsing, malformed corpus, raw IEEE, exact-limit, opacity, region, paired-archive, and package-local preferred-oldest evidence passes on GHC 9.8.4, and the ordinary focused suites pass on GHC 9.4.8. Complete-graph preferred-oldest, hosted, and release gates remain open; D-073 remains `Proposed`.

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

D-053 through D-060 are `DONE` and `Accepted`. Both supported compilers, `--prefer-oldest`, warning-free Haddock, formatting, all four historical source archives and unpacked tests, every then-applicable compile-fail boundary, all four inventory benchmarks, and deterministic semantic-report stability passed locally and in hosted CI run `33467147313` on revision `993508f`.

The current 16-package integration overlay is an uncommitted repair tree, not release evidence. No new frontier is promoted: games, autodiff, feedback, continuous probability, host tensors, and the bounded SafeTensors profile are Partial; CUDA/device execution is Blocked. D-061 through D-076 remain Proposed. Complete declaration Haddock, full-graph and independently repeated archives, device fault evidence, protected hardware, Compute Sanitizer, complete supported-compiler/lower-bound jobs, and hosted CI remain open. Release-tooling repairs add hostile-input validation, independently validated SPDX, complete-bundle checksums/provenance, and no-replace finalization, but a clean immutable revision has not run the complete preparation command.

## S8.6: Complete first-release preparation

- [x] Add a versioned preparation manifest and exposed-module snapshots.
- [x] Check PVP versions, full sibling bounds, package metadata, package README files, and changelogs.
- [x] Add bounded archive validation, checked extraction, SHA-256 sums, deterministic manifests, and SPDX 2.3 source SBOMs.
- [x] Add atomic clean-revision preparation and archive-only package and consumer builds.
- [x] Add migration, install, release-note, checklist, license, rollback, and provenance documentation.
- [x] Add a least-privilege manual preparation and attestation workflow without Hackage credentials.
- [ ] Complete D-061 and regenerate the final package and API manifests.
- [ ] Complete exposed declaration Haddock and every D-075 compiler and hardware gate.
- [ ] Run full preparation on one clean immutable revision and verify hosted attestations.
- [ ] Get explicit user approval before any external candidate or publication action.

S8.6 status: `BLOCKED` by D-061, D-067, D-073, D-074, and D-075.

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
  - Keep arena histories separate. The generic open-game callback still has no mixed lifting, repetition, continuous strategy space, subgame-perfect solver, or equilibrium-existence result.

- [ ] **S8.1 Accept exact mixed, stochastic, and Bayesian candidate semantics.**
  - [x] Add checked owner products, literal rational simplexes, complete normal games, independent mixed profiles, exact expectation, and every-pure-deviation Nash checks.
  - [x] Keep correlation devices separate; add unconditional CE and constant-deviation CCE checks with null-recommendation reports.
  - [x] Add joint reward/successor outcome laws, finite-horizon public-state Markov evaluation, and local continuation-game Markov-perfect checks.
  - [x] Add correlated common type priors, positive-type and ex-ante Bayes-Nash checks, null-type reports, bounded strategic-normal conversion, and a checked closed-context open-game adapter.
  - [x] Add exhaustive binary mixed-Nash differential enumeration, correlation, degeneracy, irrational-equilibrium, timing, null-type, deterministic-report, compile-fail, and benchmark evidence.
  - [ ] Pass GHC 9.4.8, lower-bound, full Haddock, formatting, source-archive-only, and hosted CI gates on one final revision.
  - Defer exact support/LP/LCP solvers to an optional package. Singular systems must be classified, not skipped. Do not claim equilibrium existence or complete real-equilibrium enumeration.

## S8.2: Accept bounded autodiff lowering

- [x] Add an optional closed typed polynomial and `tanh` language.
- [x] Add explicit shapes, associated products, owner trees, input fanout, and parameter sharing.
- [x] Add exact formal-polynomial and checked-Double compilation to opaque reverse tapes.
- [x] Add exact JVP/VJP pairing, all-coordinate finite differences, counterexamples, reports, and compile-fail boundaries.
- [x] Extract the pure and effect-capable reverse foundation to `markovian-reverse` under D-067; keep the bounded host adapter in `markovian-tensor-reverse`.
- [x] Add a private bounded scalar SSA with conservative exact identity rewrites and a floating reassociation counterexample.
- [x] Compare a bounded two-layer `2 -> 2 tanh -> 2` fixture with manual neural execution and independent central finite differences for every primal, input, weight, and bias coordinate under both tape policies.
- [x] Pass focused GHC 9.4.8, Haddock, formatting, and source-archive-only gates.
- [ ] Pass the full 16-package, 18-suite, 11-benchmark CI and hosted gates.

Do not claim arbitrary-Haskell autodiff, differentiation through sampling, tensor lowering, device support, or release readiness.

S7 status: `DONE` for the bounded scopes defined by D-048 through D-060. The research boundaries listed below remain out of scope.

## Requirements for new work

- Continuous kernels require a concrete use case plus measurability and integration contracts.
- Recursion schemes require explicit recursive syntax and termination or productivity evidence.
- Codensity, Cayley, normalization-by-evaluation, and Kan-extension work require laws and benchmarks.
- New package dependencies require an owned use case, maintenance review, bounds, and CI evidence.
