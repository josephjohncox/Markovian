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

- [x] **S8 Explicit bounded feedback fragments.** Restricted source, operation accounting, phase-specific rational maxima, exact and one-below limits, discarded-intermediate counterexamples, and complete release evidence passed under accepted D-069. Universal trace, arbitrary cyclic circuits/open systems, and rewardful cyclic finite-support claims remain blocked.

## Optional continuous probability

- [x] **S9 Restricted continuous probability.** Two optional packages contain restricted exact and numerical paths under accepted D-070 and D-071. The bounded bivariate algorithm has cumulative symbolic and rational accounting, independent exact oracles, all-coordinate fixture differentials, and complete release evidence. These results do not establish general sampler correctness, certified floating bounds, or multidimensional cubature. Arbitrary measurable callbacks, point conditioning, continuous disintegration, continuous MDP execution, and the `hmatrix` bridge remain blocked.

## Optional host tensor runtime

- [x] **S10 Checked host F64 tensors.** Optional `markovian-tensor` has a restricted host F64 path under accepted D-072. Shape admission, primitive differentials, deterministic allocation/cleanup fault injection, atomic runtime-shape batches, report evidence, and complete release gates passed. General tensor semantics, arbitrary strides, broadcasting, mutation, and performance claims remain blocked.

## Optional CUDA device fragment

- [x] **S11 Checked matrix/VJP CUDA executor.** Accepted D-074 has an owned dynamic CUDA 13 driver table, strict ABI checks, explicit pre-launch fallback boundaries, and a digest-pinned no-GPU compile workflow. The release record states that archive, hosted, protected hardware, sanitizer, CI, and benchmark gates passed. The retained repository records cannot reconstruct those hardware assertions as one D-077 same-session receipt set. Generic tensor graphs, generic reverse programs, other dtypes, arbitrary devices, bitwise reproducibility, and GPU advantage remain blocked.

- [x] **S11.1 SafeTensors profile.** `markovian-safetensors` implements only the pinned bounded metadata-free F64 profile. Canonical encoding, duplicate-preserving parsing, malformed corpus, raw IEEE, exact-limit, opacity, region, archive, supported-compiler, hosted, and release gates passed before D-073 acceptance.

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

The `2026.9.3.0` release record lists complete documentation, archives, device faults, protected hardware, four sanitizer tools, compilers, hosted CI, checksums, SPDX, and provenance. The repository cannot reconstruct its hardware assertions under D-077. D-061 through D-076 are `Accepted` only for their stated bounded scopes. General equilibrium solving, arbitrary-Haskell autodiff, unrestricted feedback or disintegration, general tensor semantics, and general device correctness remain out of scope.

## S8.6: Complete first-release preparation

- [x] Add a versioned preparation manifest and exposed-module snapshots.
- [x] Check PVP versions, full sibling bounds, package metadata, package README files, and changelogs.
- [x] Add bounded archive validation, checked extraction, SHA-256 sums, deterministic manifests, and SPDX 2.3 source SBOMs.
- [x] Add atomic clean-revision preparation and archive-only package and consumer builds.
- [x] Add migration, install, release-note, checklist, license, rollback, and provenance documentation.
- [x] Add a least-privilege manual preparation and attestation workflow without Hackage credentials.
- [x] Complete D-061 and regenerate the final package and API manifests.
- [x] Complete exposed declaration Haddock and every D-075 compiler and hardware gate.
- [x] Run full preparation on one clean immutable revision and verify hosted attestations.
- [x] Record explicit user approval before publication.

S8.6 status: `DONE` for release `2026.9.3.0`.

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

- [x] **S8.1 Accept exact mixed, stochastic, and Bayesian candidate semantics.**
  - [x] Add checked owner products, literal rational simplexes, complete normal games, independent mixed profiles, exact expectation, and every-pure-deviation Nash checks.
  - [x] Keep correlation devices separate; add unconditional CE and constant-deviation CCE checks with null-recommendation reports.
  - [x] Add joint reward/successor outcome laws, finite-horizon public-state Markov evaluation, and local continuation-game Markov-perfect checks.
  - [x] Add correlated common type priors, positive-type and ex-ante Bayes-Nash checks, null-type reports, bounded strategic-normal conversion, and a checked closed-context open-game adapter.
  - [x] Add exhaustive binary mixed-Nash differential enumeration, correlation, degeneracy, irrational-equilibrium, timing, null-type, deterministic-report, compile-fail, and benchmark evidence.
  - [x] Pass GHC 9.4.8, lower-bound, full Haddock, formatting, source-archive-only, and hosted CI gates on one final revision.
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
- [x] Pass the full 16-package, 18-suite, 11-benchmark CI and hosted gates.

Do not claim arbitrary-Haskell autodiff, differentiation through sampling, generic tensor lowering, or generic device support.

S7 status: `DONE` for the bounded scopes defined by D-048 through D-060. The research boundaries listed below remain out of scope.

## Executable learning and bounded extensions

- [x] Execute [the durable implementation checklist](docs/plans/EXECUTABLE-LEARNING-TODO.md), covering documentation truth, compiled teaching examples, the law laboratory, paired-difference reports, exact feedback sensitivity, checked state aggregation, and the D-085 resource contract. Completed with independent reviews and a parent audit; see [integration evidence](docs/evidence/EXECUTABLE-LEARNING-INTEGRATION.md).

The user authorized this program after [the 2026-09-05 review](docs/evidence/LEARNING-REVIEW-2026-09-05.md). Follow its checkboxes and execution log. Freeze and review each semantic contract before implementation. The program's historical status records remain unchanged. The separate 2026-09-08 acceptance covers only D-077 and original D-078; D-079 through D-085 and EL-04 remain `Proposed`, and released history stays immutable.

## Post-release roadmap

This roadmap schedules governance review before acceptance. D-077 and original D-078 are `Accepted` for the bounded scopes recorded below; D-079 through D-085 remain `Proposed`. New implementation work is `BLOCKED` until the applicable decision freezes exact exported signatures, failure precedence, package topology, ownership, limits, and evidence. Existing proposal-stage implementation evidence remains unaccepted until it is reviewed against that frozen contract.

- [x] **R0 (`DONE`) Review the 2026-09-08 deployment-scoped evidence amendment.** Policy frozen at `a3ac9db635574d1d97ae74b18f816cb74e06f574`; independent review and parent checks are recorded below.
- [x] **R1 (`DONE`) Review D-077 GPU profile and evidence truth.** Readiness PASS supports bounded acceptance of the existing profile/receipt and exact-dyadic versus CPU/CUDA repair plus deployment-scoped policy. Hardware evidence binds only `3e850085fa96c4e48a80270b9e49e9f55fe0f757`; the seven-file governance delta to `c86b4e0241debe0a9ea51b6e9f962d89ea8293df` was reviewed separately. No descendant is labelled hardware-tested. D-074 remains historical. Finite retention is allowed; deployment still requires complete evidence/signatures bound to its deployed revision.
- [x] **R2 (`DONE`) Review D-078 strict-discount affine feedback.** Readiness PASS supports acceptance only of original opaque nominal `A`/`K` coefficients and four Rational equations under the frozen normalized-event, strict-discount and cumulative-accounting contract. It remains unreleased. EL-04's separate implementation PASS does not accept its JVP contract; probability derivatives, universal trace and cyclic open adapters remain excluded.
- [ ] **R3 (`NEXT`) Review D-079 exact joint affine kernels.** Gate A implements one owner table, duplicate-before-filter validation, same-scope partial renaming, complete-map scope transitions, real-Borel materialization, signed support extrema, complete renaming preflight, frozen failure precedence, and bounded reports. The [Gate B left-successor substitution contract](docs/plans/D079-LEFT-SUCCESSOR-SUBSTITUTION.md) passed freeze review `8f9b416a-ce2d-4033-8d6a-a3350d107111` and was committed at `a8fe21e12b2d036e8ec22f10e25c4d07a5374f6f`. Implementation is underway in the isolated D-079 worktree; it is not complete or accepted. Keep callbacks, RCP, point conditioning, and multi-step control excluded; D-079 remains `Proposed`.
- [ ] **R4 (`READY`) Review D-080 bounded first-order quotation.** The existing declaration and failure schedule is now recorded in D-080, including hidden nominal paths, construction-time scope continuations versus callback-free stored `let`, saturating preflight, and independent exact primal/JVP recursion. Review the separate target account and bounded evidence gaps before any repair. Keep arbitrary-Haskell autodiff excluded.
- [ ] **R5 (`READY`) Review D-081 immutable host-F64 affine views.** The prerequisite evidence repair now compares the existing transpose fragment directly with `contiguousCopy`-first execution for every closed primitive primal and pullback, checks all logical coordinates by finite differences, and includes rectangular matrix dimension/stride reversal. It does not define a transpose-to-base pullback. Before implementing a wider view, freeze admitted affine maps, ownership, materialization, pullbacks, limits, and failure precedence. Keep general dtype, mutation, broadcast pullback, and persistent devices excluded.
- [ ] **R6 (`BLOCKED`) Review D-082 CUDA multiply-chain graphs.** Start only after D-077 and D-081 are accepted with complete evidence. Limit the graph to matrix inputs, admitted affine views, multiplication, and declared VJPs. Make no generic lowering or speedup claim.
- [ ] **R7 (`READY`) Review D-083 exact CE and CCE one-witness solvers.** Freeze deterministic streaming active sets, exact elimination, one-witness return, and cumulative accounting. Approve package placement explicitly before adding a solver package or edge.
- [ ] **R8 (`READY`) Review D-084 the reference DQN trainer.** Freeze fuel, replay and checkpoint ownership, event timing, resumption, generator handling, and failure atomicity. Keep production, convergence, and distributed claims excluded.
- [ ] **R9 (`READY`) Review D-085 interpreter-relative exact tabulation.** Limit tabulation to the named acyclic retained-circuit consumer. Freeze cache identity and compare retained and uncached execution with measured receipts. Make no universal NBE or optimization claim.

For every roadmap item, one cumulative bounded ledger covers the complete operation. Preflight precedes dense allocation. Exact and one-below tests are mandatory. Proof records and empirical receipts remain separate. Evidence for `v2026.9.3.0` does not validate post-release work. No roadmap item authorizes publication or a workflow launch.

Post-release roadmap status: R0–R2 review is complete. Final status-edit review `973f051f-ae7f-4bfd-8e79-2a1711a96670` passed; parent committed bounded D-077/D-078 acceptance at `3bfd8de50d64ea7d38008e5c5e1d3c6b595ff801`. `NEXT` is D-079 review. D-079–D-085 remain Proposed; D-082 remains `BLOCKED` on D-081. No new device feature or external action is authorized.

### All-nine execution checklist

The user requested continuation through all nine proposals on 2026-09-08.
This checklist schedules that work. It does not claim implementation or acceptance is complete.
EL-00 through EL-07 are complete for their bounded program. Do not reopen their historical execution log.

Keep all 16 versions, package edges, public APIs, and ownership boundaries unchanged during this documentation stage.
Add no packages. This stage freezes no new public signatures.
Later changes must follow each reviewed proposal contract and preserve unrelated APIs and ownership boundaries.
The parent owns status edits, commits, pushes, and separately authorized external operations.
No checklist item authorizes GitHub, runner, environment, authentication, release, publication, or network mutations.

Complete each stage in order: reviewed contract, implementation, tests, fresh independent review, then a separate parent status edit.
For existing code, review the implementation against its contract rather than treating its presence as acceptance.
Record exact revisions, changed paths, commands, exit results, log locations, and review identifiers beside completed checkboxes.
Record accepted scope and the parent status-edit revision separately.
Leave blocked or failed items unchecked. Do not check a box from plans or another stage's evidence.
Tests must cover independent oracles, opacity/nominal roles, cumulative limits, exact/one-below boundaries, and atomic failure where applicable.
Preflight must precede dense allocation. Keep proof results separate from execution and benchmark evidence.

#### R0 — Policy review

- [x] Review [section 8.2](docs/WORKFLOWS.md#82-deployment-scoped-gpu-evidence-amendment--2026-09-08) and its active projections.
- [x] Check full deployment-time evidence, signatures, issuer/source/run/transparency checks, finite retention, compact records, and deployed-revision binding.
- [x] Confirm historical receipts, failed attempts, released assets, tensor immutability, and action pins remain truthful and unchanged in meaning.
- [x] Run capability release verification, release-policy, book, and diff checks. Record failures without weakening checks.
- [x] Obtain fresh independent policy review. Review `ed4a64ae-6c19-4b53-9185-096ad6c927ad` passed with no findings; this was source/log inspection, not independent command execution.

Local documentation validation on 2026-09-08 used the uncommitted seven-file delta from `80949faed2da16d603d7eebd0586ea9d94b325f9`.
Changed paths: `TODO.md`, `RELEASE-CHECKLIST.md`, `docs/WORKFLOWS.md`, `docs/DECISIONS.md`, `docs/CONTEXT.md`, `docs/evidence/CUDA-D077-RECEIPTS.md`, and `backends/markovian-gpu/CHANGELOG.md`.
All commands ran from `/home/josephcox/dev/Markovian-proposals` and returned exit 0:

- `python3 scripts/check-capabilities --verify-release`: nine records checked. Log: `/tmp/deployment-policy-capabilities.log`.
- `bash scripts/check-release-policy`: revision flow, provenance scope, hostile-input rejection, and action pins passed. Log: `/tmp/deployment-policy-release-policy.log`.
- `bash scripts/check-book`: 77 static fences, 52 linked book files, and 422 display-math blocks checked. Log: `/tmp/deployment-policy-book.log`.
- `git diff --check`: passed. Log: `/tmp/deployment-policy-diff.log`.

These checks do not rerun Haskell teaching programs, hardware tests, or attestation verification. Parent separately reran capability, release-policy and book gates, generated all 16 source archives, and verified that all seven changed files match their archive bytes. Logs: `/tmp/proposals-parent-policy-{capabilities,policy,book,sdist}.log`. Parent also verified the seven-Markdown-file scope, unchanged Haskell signature fences, and unchanged pre-D-077 decision text before committing `a3ac9db635574d1d97ae74b18f816cb74e06f574`. Independent policy review is complete; no proposal status is accepted by this record.

#### R1 — D-077 profile and evidence governance

Existing evidence: [D-077 receipt record](docs/evidence/CUDA-D077-RECEIPTS.md) and the current repair boundary in [D-077](docs/DECISIONS.md#d-077-govern-gpu-profiles-and-evidence-truth).

- [x] Review the existing profile/receipt contract, signatures, failure order, ownership, numeric policies, and deployment-scoped evidence plan.
- [x] Review the existing repair against that contract. No implementation gaps were found in the bounded readiness review.
- [x] Check authority drift, dyadic/CPU/CUDA fixtures, bounded preflight, malformed receipts, and the exact tested run's verification records.
- [x] Obtain fresh independent D-077 review. Readiness PASS `a6b05ca2-3b1f-4202-b01c-378a1353957b` (`d077-acceptance-review.md`) inspected tested `3e850085fa96c4e48a80270b9e49e9f55fe0f757` and the seven-Markdown-file delta to `c86b4e0241debe0a9ea51b6e9f962d89ea8293df` listed in the decision and receipt acceptance notes.
- [x] Complete final independent review and parent commit of this separate authorized acceptance-status edit. Review `973f051f-ae7f-4bfd-8e79-2a1711a96670` passed; parent commit `3bfd8de50d64ea7d38008e5c5e1d3c6b595ff801`. Deployment still needs evidence binding its deployed revision.

**Acceptance evidence — 2026-09-08:** D-077 is Accepted only for its existing bounded repair and deployment-scoped policy. Run `34181389307` / attempt `1`, profile `5fbed61193cf483a2ff5642c7487ad052add4ed52da1a83a110da4711c7480dd`, remains bound to tested `3e850085fa96c4e48a80270b9e49e9f55fe0f757`. The reviewer inspected source/logs, not fresh execution. Parent's 21 profile tests, policy and book checks passed: `/tmp/proposals-parent-c86-{profile,policy,book}.log`. The governance delta manifest/patch are `/tmp/proposals-d077-governance-delta.json` and `.patch`. No hardware rerun, permission change, infrastructure gate, deployment or release is claimed.

#### R2 — D-078 strict-discount affine feedback

Existing evidence: the frozen contract and current implementation boundary in D-078.

- [x] Review the existing exact signatures, root placement, nominal coefficients, failure table, four equations, and cumulative ledger.
- [x] Review the existing implementation against that contract. No implementation gaps were found in the bounded readiness review.
- [x] Run literal-equation, explicit-horizon finite-oracle, normalization, timing, layout, rational-growth, and exact/one-below tests. Parent's exact-snapshot root suite and feedback boundary passed as recorded below.
- [x] Obtain fresh independent review. Readiness PASS `172bb833-8189-4b78-acf1-4df2455d3752` (`d078-acceptance-review.md`) reviewed the contract, implementation and supplied evidence; the exact-snapshot integration results below supply its remaining evidence attachment.
- [x] Complete final independent review and parent commit of this separate authorized acceptance-status edit. Review `973f051f-ae7f-4bfd-8e79-2a1711a96670` passed; parent commit `3bfd8de50d64ea7d38008e5c5e1d3c6b595ff801`. Keep universal trace and cyclic open adapters excluded.

**Acceptance evidence — 2026-09-08:** D-078 is Accepted only for original strict-discount affine coefficients and their four Rational equations, not EL-04's separately reviewed JVP contract. Parent executed `cabal test Markovian-test --project-file=cabal.project.ci -f-cuda --ghc-options=-Werror --test-show-details=direct` and `bash scripts/check-feedback-boundary` at exact `c86b4e0241debe0a9ea51b6e9f962d89ea8293df` under GHC 9.8.4; both exited 0. Logs: `/tmp/proposals-parent-c86-root-test.log` and `/tmp/proposals-parent-c86-feedback-boundary.log`. All Haskell bytes are unchanged through status-edit base `049372690908f179a095bb170ec7e80034b04d2e`. These exact-snapshot results are carried forward, not labelled fresh status-edit or hardware runs. Capability availability remains `unreleased` with `implementation-fixtures`; frozen release membership is unchanged.

**Status-edit validation — 2026-09-08:** Commands below ran in `/home/josephcox/dev/Markovian-proposals` on base `049372690908f179a095bb170ec7e80034b04d2e` plus the uncommitted status/test delta, not a new immutable revision. All exited 0. Logs are under `/tmp/proposals-d077-d078-status/`.

- `python3 scripts/check-capabilities --write` regenerated the book table; `python3 scripts/check-capabilities --verify-release` checked nine records and unchanged immutable membership (`capabilities-write.log`, `capabilities-release.log`).
- `python3 scripts/test_capabilities.py`: 33 tests, including Accepted + unreleased not implying released membership (`capabilities-tests.log`). `python3 scripts/test_release_tool.py`: 32 tests (`release-tests.log`).
- `bash scripts/check-release-metadata` and `bash scripts/check-release-policy`: 16-package metadata and frozen release gate/policy passed (`release-metadata.log`, `release-policy.log`). No release-tool implementation changed.
- `bash scripts/check-book`: 77 static Haskell fences, 52 book files, 422 display-math blocks, pinned mdBook/local MathJax passed (`book.log`). Teaching fingerprint remained valid; no teaching execution or hash refresh was needed or claimed.
- `cabal sdist all --project-file=cabal.project.ci --output-directory=/tmp/proposals-d077-d078-status/archives` generated 16 local validation archives (`sdist.log`). `python3 /tmp/proposals-d077-d078-status/validate-scope-archives.py` checked bounded paths/statuses, unchanged Haskell declaration fences and c86-to-049 Haskell bytes, an empty index, all 16 archive validators, and byte membership of all 17 changed files (`scope-archives.log`). These are not released assets.

Only current prose/metadata and the targeted capability regression changed. Historical EL/release records and changelogs remain intact; dated notes distinguish their point-in-time statuses. Final independent edit review and parent commit were pending when these validation records were written; their completion is recorded above. No commits, staging, publication, workflow/runner/environment/authentication actions, hardware execution, or cryptographic reruns were performed by this edit.

**Status-header correction — 2026-09-08:** Independent status-edit review `2d1cd7ac-de73-4c56-85e9-cda0be1e04b8` blocked the initial edit because the active D-077 receipt header still said Proposed. Parent added `test_cuda_receipt_matches_accepted_decision`; it failed on that header as expected (`/tmp/proposals-status-header-regression-before.log`), then passed after the header correction. All 34 capability tests, 32 release-tool tests, capability/release metadata/policy/book gates, and 16 archive validators passed. All 17 changed files matched separate corrected-byte archives. Logs and archives: `/tmp/proposals-status-final/`; validator wrapper: `/tmp/proposals-status-final-archives.py`. Original pre-correction records were not overwritten. Fresh correction review `973f051f-ae7f-4bfd-8e79-2a1711a96670` passed with no findings (source/log inspection, not an independent rerun); parent committed the corrected edit at `3bfd8de50d64ea7d38008e5c5e1d3c6b595ff801`.

#### R3 — D-079 joint affine kernels

Existing evidence: the frozen Gate A contract and implementation. The [Gate B left-successor substitution plan](docs/plans/D079-LEFT-SUCCESSOR-SUBSTITUTION.md) records the parent-approved reviewed design with the required correction: both projections retain `substitutionMaximumRationalBits` for stored admission and meter seeds, including discarded left reward and canceled intermediates. Separate projection work does not reset historical bits. Gate B implementation and explicit fresh-owner evidence remain absent. The plan records its drafting baseline separately from current decision status.

- [x] Record the complete Gate B contract, corrected projection accounting/precedence and three-bit/four-bit `9/4 -> 1/4` fixtures. This checks only the documentation action, not freeze review, implementation, or acceptance.
- [x] Independently review this repository freeze and Gate A compatibility: exact signatures/roles, sealed non-chainable right-reward/right-successor semantics, actual-operand-bound witnesses, full zero-inclusive manifests, both-source freshness, canonical order, total failure schedule, cumulative substitution ledger, and historical-bit projections. Review `8f9b416a-ce2d-4033-8d6a-a3350d107111` passed for the contract only. Parent resolved its teaching-binding follow-up by executing all seven fixtures; outputs were byte-identical and the receipt source digest was refreshed (`/tmp/d079-parent-contract/teaching-write.log`, `teaching-result.json`).
- [ ] Implement only the separately authorized reviewed contract, including the explicitly FUTURE supersession of retained-only Gate A renaming semantics by declaration-complete membership, collisions, scope coverage, and work. Preserve public Gate A signatures, real-Borel outputs, nominal ownership, and historical records.
- [ ] Run all plan fixtures: independent symbolic/corner oracles, shared/fresh joint moments, lost left reward, zero-row renaming/collisions, private manifest/witness invariants, positive clients and named negative boundary diagnostics, exact/one-below limits, cancellation, projection historical bits, and insufficient-work precedence.
- [ ] Obtain fresh independent review of Gate A and the completed Gate B implementation with current evidence.
- [ ] Parent records any full-proposal acceptance in a separate reviewed status edit. Gate A alone cannot complete D-079.

#### R4 — D-080 first-order quotation

Existing evidence: the proposal-stage quotation implementation and current evidence boundary in D-080.

- [x] Record the exact existing signature schedule, constructors/eliminators, path roles, call-by-value order, and total failure table in [the D-080 freeze](docs/DECISIONS.md#d-080-existing-api-freeze--declaration-reference). This checks only the documentation action, not contract review or acceptance.
- [x] Independently review the completed [prospective cumulative-compilation repair contract](docs/plans/D080-CUMULATIVE-COMPILATION.md), preserving the historical separate-account characterization. Review `eeb4e4ba-4417-442e-b5bd-cb28ca4f4556` passed for the contract only. The approved scalar-product output fixture and full parameter tree have external baseline execution evidence; primitive ownership-layout equality has its own named coupon and exact product-owner budget oracles. Teaching receipts were refreshed through actual execution. Planner/accounting/private-probe specifications are not implemented; baseline fixture evidence is not budget-repair evidence or acceptance.
- [ ] Review the existing implementation against that schedule. Repair only reviewed gaps.
- [ ] Run forged/escaped-path compile failures, used/unused lets, shadowing, saturation, preflight, and independent primal/JVP/reverse coordinate comparisons.
- [ ] Obtain fresh independent contract and implementation review with current test evidence.
- [ ] Parent records any acceptance in a separate reviewed status edit. Keep arbitrary-Haskell and higher-order autodiff excluded.

D-080 freeze validation used `/home/josephcox/dev/Markovian-proposals`, branch `feat/deployment-scoped-proposals`, with base HEAD `c86b4e0241debe0a9ea51b6e9f962d89ea8293df` plus the uncommitted `docs/DECISIONS.md` and `TODO.md` documentation delta. This is current-worktree evidence, not evidence for a new immutable revision or for `v2026.9.3.0`. GHC 9.8.4 / Cabal 3.16.1.0 compiled the unchanged autodiff source and ran its focused suite. The package and boundary commands ran after the decision edit; subsequent TODO edits record those results only. Final documentation checks cover both changed files.

- `cabal test markovian-autodiff-test --project-file=cabal.project.ci --test-show-details=direct`: exit 0; all focused tests passed. Log: `/tmp/d080-contract-freeze/package.log`.
- `bash packages/markovian-autodiff/scripts/check-autodiff-boundary`: exit 0; all expected public-boundary rejections passed. Log: `/tmp/d080-contract-freeze/boundary.log`.
- `bash scripts/check-book`: exit 0; 77 existing Haskell fences, 52 book files, 422 display-math blocks, pinned mdBook and local MathJax checked. Log: `/tmp/d080-contract-freeze/final-book.log`.
- `python3 scripts/check-learning`: exit 0; static references/output receipt only, not teaching execution. No new Haskell fence or executable example was added. Log: `/tmp/d080-contract-freeze/final-learning.log`.
- `python3 scripts/check-capabilities --verify-release` and `bash scripts/check-release-policy`: exit 0. Logs: `/tmp/d080-contract-freeze/final-capabilities.log` and `/tmp/d080-contract-freeze/final-release-policy.log`.
- `git diff --check`: exit 0. Log: `/tmp/d080-contract-freeze/final-diff-check.log`.

No Haskell, Python, workflow, package/version, release-history, topology, or teaching-metadata file changed. Both Markdown files already belong to the root source distribution. No tests were added. The independent review, exhaustive quotation coordinate/failure matrix, second supported compiler, and full integration/release gates are not established by these focused commands. R4 review/acceptance boxes remain unchecked; D-077 through D-085 remain `Proposed`.

**Status update — 2026-09-08:** The preceding D-080 freeze validation is a point-in-time record. The separate bounded D-077/D-078 acceptance does not change D-080's Proposed status, separate compile-ledger contract, evidence gaps, or unchecked review/acceptance boxes.

#### R5 — D-081 immutable host-F64 affine views

Existing evidence: [transpose reverse-equivalence repair](docs/evidence/D081-TRANSPOSE-REVERSE-EQUIVALENCE.md), not a wider affine-view implementation.

- [ ] Review prerequisite coordinate and finite-difference evidence. Freeze and review affine maps, signatures, ownership, pullbacks, materialization, limits, and failure order.
- [ ] Implement only the reviewed affine-view contract after the prerequisite review. Preserve immutable storage and distinct owner/storage/region evidence.
- [ ] Test empty/scalar shapes, signed strides, offsets, transpose/reversal/slicing/composition, bounds, overlap rejection, region escape, and direct/materialized pullbacks.
- [ ] Obtain fresh independent review with all-coordinate oracles, finite differences, allocation/preflight checks, and exact/one-below evidence.
- [ ] Parent records any acceptance in a separate reviewed status edit. The existing transpose evidence alone cannot complete D-081.

#### R6 — D-082 CUDA multiply-chain graphs

**Still blocked on separate reviewed acceptance of D-081. D-077's bounded acceptance is recorded above; it does not accept the graph or supply deployed-revision evidence.**

- [ ] Verify both prerequisite acceptance records. Freeze and review graph signatures, admitted nodes/views, ownership, schedule, cleanup, ledgers, and failure precedence.
- [ ] Implement only the reviewed closed matrix-input/view/multiply DAG and its declared VJPs.
- [ ] Test graph/transfer/payload/work/launch limits, cleanup, and every forward/VJP coordinate against independent dyadic and CPU references.
- [ ] Obtain fresh independent review with same-session hardware/sanitizer evidence under section 8.2. Obtain separate authorization before hardware workflows.
- [ ] Parent records any acceptance in a separate reviewed status edit. Bind actual GPU deployment evidence to its deployed revision.

#### R7 — D-083 CE and CCE one-witness solvers

Package placement needs explicit approval. This checklist authorizes no new package or edge.

- [ ] Obtain placement approval within the unchanged topology. Freeze and review signatures, streamed active-set order, normalization, reports, ledgers, and failure precedence.
- [ ] Implement separate bounded exact CE and CCE one-witness solvers under the reviewed contract.
- [ ] Test independent witness inequalities, small exhaustive fixtures, degeneracy, rank deficiency, inconsistency, no-witness traversal, and exact/one-below atomic failures.
- [ ] Obtain fresh independent review of placement, solver behavior, accounting, and current evidence.
- [ ] Parent records any acceptance in a separate reviewed status edit. Keep Nash and unrestricted solver claims excluded.

#### R8 — D-084 reference DQN trainer

- [ ] Freeze and review trainer signatures, fuel, replay/checkpoint owners, event order, terminal/truncation rules, RNG advancement, resumption, and failures.
- [ ] Implement only the reviewed resumable reference trainer around the existing checked batch update.
- [ ] Test split-run equality, masks, replay warming/eviction/order, target schedules, standard/Double-DQN, callback failures, atomic updates, and exact/one-below limits.
- [ ] Obtain fresh independent review with semantic step evidence and separately scoped timing receipts.
- [ ] Parent records any acceptance in a separate reviewed status edit. Keep production, convergence, device, and distributed claims excluded.

#### R9 — D-085 retained-circuit trace cache

Existing evidence: [EL-06 resource policy](docs/plans/EL-06-RESOURCE-ADMISSION.md) and its completed lesson, not the concrete cache.

- [ ] Freeze and review concrete table/cache/interpreter/trace signatures, key identity, ordered validation replay, dual ledgers, and failure precedence.
- [ ] Implement only the named retained-circuit consumer under source-semantic admission. Preserve full source charges and separately bounded executor costs.
- [ ] Test discarded-intermediate checks, source failure order, key separation, changed-limit misses, no partial entries, exact uncached equality, and exact/one-below limits.
- [ ] Obtain fresh independent review with same-workload raw time/allocation/hit/table-size evidence. Do not substitute EL-06's hypothetical hit cost.
- [ ] Parent records any acceptance in a separate reviewed status edit. Keep universal normalization and optimization claims excluded.

## Requirements for new work

- Continuous kernels require a concrete use case plus measurability and integration contracts.
- Recursion schemes require explicit recursive syntax and termination or productivity evidence.
- Codensity, Cayley, normalization-by-evaluation, and Kan-extension work require laws and benchmarks.
- New package dependencies require an owned use case, maintenance review, bounds, and CI evidence.
