# Repository context

Read this file, `TODO.md`, and the relevant architecture and decision sections before a semantic change.

## Current state

Markovian is a greenfield experimental Haskell package for finite stochastic kernels and decision models. It has no compatibility commitment and no external users. Incorrect interfaces are removed rather than preserved.

The core library exposes validated floating and exact probability, reward, objective, kernel, MRP, MDP, policy, sampling, trace, compilation, tabular-learning, exact POMDP, categorical IR, dense CPU, and bounded inventory-benchmark modules. Policy-free exact compilation stores every available action and joint outcome. A separate operation closes a compiled model under a policy. Exact interpreters provide bounded expectation, trace enumeration, finite-horizon policy evaluation, contraction Bellman policy evaluation, bounded value iteration, deterministic greedy extraction, rational policy iteration, filtering, and bounded belief planning. Inventory modules include the synthetic serial fixture, the finite Clark--Scarf specialization, the finite Doğru physical-versus-relaxed adaptation, and a bounded two-stage fixed-batch execution with separate stationary newsvendor calculations. Each reports only validated bounded evidence.

The sampled and tabular layers share one explicit-generator joint MDP step. They provide pure TD(0), SARSA, Expected SARSA, and Q-learning updates. Each method has a bounded resumable episodic runner. The neural package provides checked dense networks, typed parametric reverse circuits, finite owned reverse programs with explicit tapes, stable categorical operations, sized structural action masks, approximate entropy, cross entropy, KL divergence, mutual information, and logit gradients, linear REINFORCE and actor-critic updates, FIFO replay, target-network synchronization, and one standard or Double-DQN batch update. It has no tensor, general-autodiff, checkpoint-scheduler, device, environment-runner, or complete-trainer dependency. The separate `markovian-neural-bridge` package checks exact global action layout and head width, preserves exact per-state availability order, and represents terminals without empty masks.

The exact semantic foundation also exposes duplicate-free finite sets, nonempty finite objects, law-bearing scalar capabilities, nonnegative rational scalars, opaque semiring matrices, normalized stochastic matrices, proof-carrying deterministic matrices, exact convex mixtures, checked signed rational finite payoffs, exact payoff pullback and state-payoff pairing, normalized priors, positive supports, conditioning, prior-indexed Bayesian inversion, checked Bayesian channels, raw purity-indexed stochastic-circuit syntax, first-order quoted-table compilation, finite typed hypergraphs, structured cospans, commuting open-system cells, and a separately validated boundary-functional finite DAG interpreter. Both finite-witness modules export the canonical `sameFiniteLayout` operation. They retain `sameFiniteSetLayout` and `sameFiniteObjectLayout` as descriptive aliases.

The package does not contain the former branch-weight process, recursive `MDPF`, or defective prototype Q-learning implementation. `app/Sample/Main.hs` demonstrates the exact finite-horizon evaluator. `docs/book` contains the pinned mdBook user and contributor guide. The book covers the full public semantic stack and includes the architecture, decisions, and workflows as project references.

The separate `Pages` workflow publishes the checked book from `main` at <https://josephjohncox.github.io/Markovian/>. Book display equations use mdBook's required double-backslash delimiters. A pinned local MathJax 3.2.2 SVG bundle removes the runtime CDN dependency. A pre-typesetting pass restores TeX subscripts and ASCII primes when Markdown emits emphasis tags or smart punctuation. The book check verifies the bundle digest and local loader, rejects the external loader, malformed delimiters, ambiguous bare TeX stars, unsupported generated math markup, and source-to-generated block mismatches. Foundation chapters connect algebra, category theory, measure theory, categorical probability, information theory, reverse derivatives, parameter sharing, optimization, polarity, state-payoff push-pull duality, and game semantics to the APIs, law catalogue, executable fixtures, derived insights, and direct primary-source reading routes. [Pages run 33126170927](https://github.com/josephjohncox/Markovian/actions/runs/33126170927) built and deployed merge commit `1268191a025c22fd9b995a1025d9ca810ff43451`. The repository uses the GitHub Actions Pages source, HTTPS enforcement, and a `main`-only `github-pages` environment policy.

The root test aggregator includes focused `ExactControl`, `TabularLearning`, `PushPullExact`, `InventoryBenchmark`, `ClarkScarf1960`, `DogruInventoryBenchmark`, and `FixedBatchRnQ` modules. Their deterministic fixtures cover exact compilation and control, exact state-payoff push-pull, checked inventory provenance and bounds, tabular targets, terminal timing, seeded behavior, bounded loops, and resumable runs. The neural test aggregator includes `ActionMask` and `ParametricReverse` plus dense finite differences, categorical information and policy gradients, REINFORCE, actor-critic, replay, target networks, and DQN fixtures. The bridge suite owns exact-layout, terminal, permutation, policy-gradient, and DQN support-mask differentials.

On the current worktree, GHC 9.8.4 passed `cabal build all` and `cabal test all` for the four-package project after the integration-gate reconciliation. All root, reverse-program, and exact-support compile-fail scripts passed. All four inventory benchmarks completed one excluded warm-up and twenty measured runs with an identical exact semantic report. All four package metadata checks, warning-free full Haddock, repository HLint, Fourmolu, cabal-fmt, `git diff --check`, boundary-script syntax, CI YAML parsing, and the complete book check passed. Four source archives were generated with the required integration files, and all five root compile-fail scripts passed from an unpacked root archive. These results do not complete integration acceptance. Lower-bound, GHC 9.4.8, hosted, enabled GPU, complete unpacked package tests, unpacked reverse and exact-support boundaries, and all four unpacked inventory benchmarks remain to be run on one final worktree.

D-053 through D-060 are `Accepted`. The complete local integration matrix and hosted CI run `33467147313` passed for revision `993508f`, including all four packages, all four inventory benchmarks, and the game, reverse-program, and exact-support boundaries.

The [S6 evidence record](evidence/S6-ACYCLIC-OPEN-2026-08-27.md) remains historical evidence for the earlier semantic-tower revision. Do not apply its test counts or pass claims to the current uncommitted worktree.

The 2026-08-26 CUDA evidence used a final-correction worktree based on `2efb1c6`. The enabled test passed on an NVIDIA GB10 with driver 580.173.02 and compute capability 12.1. After one excluded warmup, 20 transfer-inclusive samples had a `267.843920400 ms` mean and `3.025869898 ms` sample standard deviation. The range was `263.519087000 ms` to `276.777522000 ms`, and maximum error was `0.000e0`. CUDA 13.0 `nvcc` V13.0.88 reproduced the committed PTX files. The [complete evidence record](evidence/CUDA-2026-08-26.md) retains raw samples and labels older mean-only results as historical execution records.

Feature commit `d99b2c011100d200934dce9e5993a7d070398b39` passed matching [push](https://github.com/josephjohncox/Markovian/actions/runs/33124880789) and [pull-request](https://github.com/josephjohncox/Markovian/actions/runs/33124883088) workflows. Both runs passed GHC 9.4.8, GHC 9.8.4, lower-bounds, and source-check jobs. PR #1 merged as `1268191a025c22fd9b995a1025d9ca810ff43451`.

## Resolved defects

| ID | Resolution |
| --- | --- |
| K-001 to K-004 | Removed the ambiguous branch-weight `Action`, recursive `MDPF`, self-recursing pseudo-Mendler algebras, and unchecked probabilistic evaluators. |
| K-005 to K-009 | Removed the defective Q-learning implementation, hidden constants, unbounded negative recursion, partial vector operations, incorrect reward timing, and unstable action-name keys. |
| K-010 | Removed compatibility characterization tests with the deleted API and replaced them with semantic contracts. |
| K-011 | The library, sample, and tests now depend only on `base` and the local package. Obsolete lower-bound workarounds were removed. |
| K-012 | Package metadata, source distribution, and direct component dependencies are verified by CI checks. |
| K-013 | Replaced generated changelog text with an unreleased factual entry. |
| K-014 | Removed the Q-learning example and rewrote the sample against the exact semantic API. |
| K-015 | Floating normalization removes every mass that rounds to zero and exposes positive support only. |
| K-016 | Removed the third-party dependency plan that required `mwc-random` and `unix-compat` constraints. |
| K-017 | Split HLint and cabal-fmt into separate Cabal installation plans after hosted CI exposed an unsatisfiable combined plan. |
| K-018 | Added unbiased rational categorical selection so fixed-width unit-interval sampling cannot make positive exposed support unreachable. |
| K-019 | Added exhaustive finite compilation so duplicate indexes and transitions to unindexed successors fail before iterative solvers run. |
| K-020 | Replaced hidden, partial, unbounded learning behavior with validated Q-values, pure terminal-aware updates, bounded episodes, and explicit generator state. |
| K-021 | Made observation timing explicit and added structured zero-evidence and mixed-belief errors instead of unchecked belief normalization. |
| K-022 | Added typed categorical syntax and explicit copy semantics so compiler rewrites cannot equate one shared draw with two independent draws. |
| K-023 | Added dense CPU, actual CUDA, and neural contract packages outside the semantic core with differential and normalization evidence. |
| K-024 | Changed copy from a diagonal target subobject to the full tensor square with diagonal denotation. Added standard probability, category, arrow, symmetry, coherence, and fanout combinators. |
| K-025 | Separated empty finite sets from nonempty probability objects and added exact scalar, raw matrix, stochastic, deterministic, compact, trace, and convex contracts without assigning stochastic meaning to transpose or trace. |
| K-026 | Closed representational-coercion and overridable-convex-validation holes. Added nontrivial dagger, representative scalar, biproduct, normalization, deterministic-subcategory, embedding, and reordered trace evidence. |
| K-027 | Added support-restricted exact Bayesian inversion and moved generic exact prediction and conditioning behind the Bayesian module without changing POMDP timing, support order, posterior values, impossible-observation errors, or bounded planning. |
| K-028 | Added raw purity-indexed circuit syntax, an exact structural fold, shared dense CPU lowering, deterministic-only copy optimization, explicit approximation contracts, and first-order finite-table compilation. |
| K-029 | Added finite typed interfaces and hypergraphs, explicit quotient pushouts, structured-cospan gluing, commuting open-system cells, and directed circuit decorations without graph black-boxing claims. |
| K-030 | Restored the binding finite-witness API. Both public modules now export lawful `sameFiniteLayout` operations and keep their descriptive aliases. |
| K-031 | Reconciled the CUDA record with labeled historical and current local measurements. Verified enabled execution and reproducible PTX generation. |
| K-032 | Added sample-bearing CUDA benchmark output, dispersion, a warmup policy, and a durable raw evidence record. Corrected stale hosted and contract-count documentation. |
| K-033 | Added opaque unique-production and acyclicity validation, exact finite DAG interpretation through local purity-indexed circuits and matrices, and compile-fail boundaries that exclude raw, cyclic, reversed, and purity-strengthened use. |
| K-034 | Replaced policy-bound exact compilation with policy-free exhaustive MDP compilation and separate compiled policy closure. |
| K-035 | Added exact discounted value iteration, deterministic greedy extraction, and rational deterministic policy iteration. |
| K-036 | Added shared sampled-step and tabular TD architecture plus framework-independent neural update references. |
| K-037 | Added a total checked finite payoff representation, exact pullback through normalized matrices, and state-payoff pairing without treating pullback as Bayesian inversion. |
| K-038 | Added a synthetic bounded two-echelon serial fixture, exact oracle and base-stock comparison, widened-bound diagnostics, deterministic report, and reproducible timing executable without assigning unverified published provenance. |
| K-039 | Added bounded circuit cost accounting and opaque deterministic rewrite candidates whose checked witnesses require exact endpoint layouts and exact row-major matrix denotation. |
| K-040 | Replaced unsized neural action-index lists with sized structural masks and added a one-way exact-layout bridge without coupling the root and neural libraries. |
| K-041 | Reconciled the four-package CI and source-archive inventory: all named inventory benchmarks and applicable compile-fail boundaries are explicit, including the finite-game boundary. Decision acceptance remains blocked until those gates execute on one final revision. |

## Semantic vocabulary

- **Finite distribution**: a validated, nonempty finite support with positive exposed mass and normalized total mass.
- **Stochastic kernel**: a function from one input to a finite distribution over outputs.
- **Action ID**: the stable identity of an agent choice.
- **Transition outcome**: one transition reward paired with its successor state.
- **Policy**: a stochastic kernel from state to available action ID.
- **Objective**: the explicit horizon, discount, and return rule supplied to an interpreter.
- **Interpreter**: code that evaluates, samples, solves, learns, or lowers a model.
- **Bellman operator**: the value transformation for one objective step.
- **Model syntax**: an explicit recursive program representation, if one is introduced later. It is not the state graph.

## Non-negotiable invariants

| ID | Invariant | Enforcement |
| --- | --- | --- |
| INV-ACTION | Action IDs and stochastic outcomes are different types. | Module boundaries and opaque constructors |
| INV-DIST | Finite distributions reject empty, negative, non-finite, and zero-total input. | Smart constructors |
| INV-REWARD | Rewards and terminal payoffs are finite. | Smart constructors |
| INV-AVAILABLE | Continuing MDP states have nonempty unique action IDs. | Structured model errors |
| INV-POLICY | Policy support is unique and contained in available actions. | Validation at closure and evaluation |
| INV-OBJECTIVE | Evaluation receives an explicit named objective. | Required argument |
| INV-TERMINAL | No interpreter requests policy or transition data at a terminal state. | Control flow and sentinels in tests |
| INV-HORIZON | Bounded evaluation decreases the transition horizon on every recursive call. | `Natural` horizon and self-loop tests |
| INV-RNG | Reproducible stochastic execution receives and returns explicit generator state. | Sampled interpreters and episodic runners |
| INV-CONTROL | Exact control compiles every model action and preserves each joint reward-successor distribution. | Policy-free compiled MDP and exact-control tests |
| INV-INVENTORY | Exact inventory values apply only to the stated finite or conditional bounded-demand model; reachable successors are not clamped, truncation mass is not a value-error bound, and unrelated or non-widened solutions cannot produce stable evidence. Fixed-batch finite-horizon and stationary outputs remain separate. | Opaque fixture and solution, support closure, checked widening witness, separate newsvendor module, and report tests |
| INV-LEARNING | Terminal updates use `r + gamma * g` without dummy actions or terminal policy queries. | Pure tabular and neural update tests |
| INV-MASK | Neural masks have a checked complete width and ordered nonempty active indices; exact compilation checks global layout, preserves availability order, and keeps terminals separate. | Opaque mask/layout constructors, bridge tests, gather-before-consumer implementation |
| INV-CORE | The semantic core has no tensor, GPU, autodiff, neural, or sampling-framework dependency. | Cabal dependency check |
| INV-REVERSE | Independent parameters form explicit pairs; cotangent witnesses provide zero, addition, scalar action, and equality; primitive pullbacks are additive and homogeneous; reverse diagonals use witness addition. | Opaque reverse interpreter, exact module/VJP laws, and nonlinear finite differences |
| INV-COPY | Copy targets the full tensor square and assigns mass only to diagonal pairs. | Opaque IR constructors and exact law tests |
| INV-MATRIX | Matrix semantics compare labelled entries and supports, not storage layout or exact-distribution representation. | Opaque matrices and `matrixEquivalent` laws |
| INV-NORMALIZED | Stochastic matrix rows sum exactly to one; transpose and trace are raw-matrix operations only. | Opaque normalized refinement and counterexample test |
| INV-PAYOFF | Exact payoffs are total on their finite object; pullback is conditional expectation and state pairing checks the represented singleton state source and common object. | Opaque payoff tables, structured mismatch errors, and exact pairing fixtures |
| INV-DETERMINISTIC | Copy-naturality reasoning requires proof-carrying deterministic provenance. | Opaque deterministic refinement and total forgetful embedding |
| INV-BAYES | Bayesian inversion is prior-indexed and defined only from positive output support to positive input support. | Opaque inverse, structured zero evidence, and exact joint laws |
| INV-CIRCUIT-PURITY | Copy-naturality rewrites require deterministic construction provenance. | Purity index, one-way weakening, and compile-fail checks |
| INV-CIRCUIT-SHARING | One stochastic execution followed by copy differs from duplicated execution. | Distinct share and fanout nodes with differential laws |
| INV-CIRCUIT-COST | Cost reports are bounded static accounting with caller-owned primitive charges; layout width is not runtime or heap liveness. | Raw-node-bounded fold, `Natural` limits, deterministic reports |
| INV-CIRCUIT-REWRITE | A deterministic candidate becomes checked evidence only after bounded exact endpoint-layout and row-major matrix checks. | Opaque candidates and checked witnesses, exact differential tests, compile-fail purity boundary |
| INV-OPEN-REVERSAL | Boundary reversal swaps cospan legs and does not reverse dynamics or circuit state parameters. | Same-oriented state types and a separate view with no reverse-denotation observer |
| INV-OPEN-CELLS | Higher cells preserve types, labels, ordered incidence, and both boundary squares. | Opaque validated `OpenSystemCell` |
| INV-OPEN-DAG | Every interpreted apex vertex has exactly one producer and represented edges are acyclic. | Opaque `AcyclicOpenSystem`, stable cycle diagnostics, and compile-fail checks |
| INV-OPEN-EXECUTION | One edge occurrence executes once; multiple consumers read its stored value, while separate occurrences execute independently. | Named assignments, edge-local circuit interpretation, and exact sharing fixtures |

## Current decisions

- D-001 through D-007 define the semantic foundation and objective boundaries.
- D-011 defines transition rewards and terminal payoffs.
- D-015 preserves joint reward and successor outcomes during policy closure.
- D-016 defines terminal-aware Bellman equations.
- D-017 requires overflow-safe floating normalization.
- D-023 and D-024 define the reproducible toolchain and pinned CI matrix.
- D-026 through D-029 define exact values, exact Kleisli laws, validated policy closure, and exact finite expectation.
- D-030 supersedes compatibility and migration decisions and defines immediate removal of defective experimental APIs.
- D-031 through D-037 define sampling, compilation, learning, POMDP, backend, and finite Markov-category contracts.
- D-038 defines the staged exact matrix, Bayesian, circuit, deterministic compiler, and syntax-only open-system tower while separating its three reversal operations.
- D-039 defines support-restricted prior-indexed Bayesian inversion, almost-sure equality, and checked Bayesian-channel prior flow.
- D-040 defines recursive purity-indexed circuits, exact structural folds, approximation boundaries, and the supported first-order deterministic compiler fragment.
- D-041 defines finite typed structured cospans, explicit pushout witnesses, commuting open-system cells, and the narrow directed circuit decoration denotation.
- D-042 defines the separately validated boundary-functional finite DAG fragment, named finite assignments, local label-circuit resolution, and exact topological semantics.
- D-043 defines the pinned mdBook stack, validation check, SHA-pinned Pages publication policy, and book maintenance contract.
- D-044 defines policy-free exact compilation and exact discounted control.
- D-045 defines the shared sampled-step and tabular TD architecture.
- D-046 defines optional framework-independent neural numerical and update references.
- D-047 defines self-hosted MathJax, Markdown-to-TeX recovery, and foundation-source publication evidence.
- D-048 separates information observables, reverse derivatives, adjunctions, Bayesian inversion, and optimizer dynamics and defines the inventory benchmark requirements.
- D-049 separates state pushforward, payoff pullback, tangent-cotangent duality, logical polarity, and strategic interaction.
- D-050 defines checked signed rational finite payoffs, exact stochastic pullback, and state-payoff pairing while keeping Bayesian inversion separate.
- D-051 defines bounded conditional-model exactness, synthetic serial-inventory provenance, finite-horizon backward induction, opaque solution provenance, checked widened-bound diagnostics, and reproducible timing.
- D-052 defines the small neural-backend parametric reverse interpreter, explicit parameter products, cotangent-module witnesses, captured primitive pullbacks, diagonal addition, and fixture-specific numerical equality.
- D-053 defines bounded static circuit costs, opaque deterministic rewrite candidates, exact checked witnesses, and the separate floating-fusion evidence boundary.
- D-054 defines sized structural neural masks and the one-way exact-action-layout bridge.
- D-055 defines the source-crosswalked Clark--Scarf (1960), Section III, exact finite-lattice specialization and its three-path oracle.
- D-056 defines Doğru's two-retailer finite-horizon adaptation with separate physical and bounded signed-relaxed actions, exact policy comparisons, and two-dimensional widening evidence.
- D-057 defines bounded two-stage fixed-batch execution, exact finite-horizon policy comparison, and separate stationary subsystem costs and discrete newsvendor inequalities.
- D-058 defines finite owned reverse syntax, checked finite primal and cotangent layouts, structural parameter ownership, and opaque stored or recomputed tapes.
- D-059 defines finite reachable acyclic alternating arenas, opaque replay histories, receptive deterministic strategies, bounded synchronized composition, and exact external-prefix observational equality.
- D-060 defines bounded concrete finite optics and an owner-refined finite open-game fragment with exact rational decisions, pure contextual equilibrium enumeration, and best-response-sensitive observational equality.

## Next task

S7.1 information, categorical-learning, polarity, and game-semantic foundations are implemented. S7.3 now includes exact payoff pullback, bounded static circuit costs, and exact deterministic rewrite certificates. The neural softmax/cross-entropy fusion has separate checked-`Double` differential evidence and no exact certificate. S7.2's source-bounded inventory slices remain implemented within their stated limits. D-058 extends the small typed parametric reverse interpreter with a finite acyclic syntax over caller-owned primitives, structural ownership, finite primal and cotangent layouts, bounded structural preparation, and opaque stored or recomputed tapes. Recomputation is a distinct typed owner operation. It is confined to `markovian-neural` and interprets supplied VJPs; it has no optimizer, tensor runtime, general autodiff, recursion, cycle, or checkpoint-scheduling semantics. Exact laws, structural cycle/depth rejection, deterministic reports, callback failures, repeated tapes, and all-coordinate finite differences under both tape policies pass locally. D-054's accepted exact-availability-to-structural-neural-mask bridge has bounded traversal and nominal-role protection across the explicit bridge package. D-055's Clark--Scarf (1960), Section III, finite lattice specialization preserves the source state and timing, compares three exact equation paths, and reports finite cap widening without an unbounded claim. D-056's Doğru adaptation keeps physical and signed-relaxed actions distinct and compares exact bounded oracles with balanced and physicalized policies. D-057's fixed-batch implementation keeps exact finite-horizon execution visibly separate from stationary newsvendor evidence on explicit finite Cartesian `R1`/`R2` layouts, retains and checks stationary provenance, and reports truncation and widening only as diagnostics. D-059's accepted finite alternating protocol is implemented in the root exact core. Arena construction and equality are bounded; strategy construction uses one conservative operation-wide work account; composition is explicitly partial after hiding. Copycat, representative successful associativity, checked equality, layout, rejection, and compile-fail fixtures pass locally. This is not Hyland--Ong or AJM semantics and supplies no closure, category-law, justification, or innocence result. D-060's separate finite open-game fragment is accepted within its stated finite scope. It checks structural owner-disjoint strategy products, pre-allocation product bounds, canonical continuations, strict performed counts, incumbent-sensitive sequential and tensor composition, bounded pure equilibrium enumeration, and exhaustive best-response-sensitive observational equality. Exact rational examples, all represented binary payoff tables, matching-pennies, and non-credible-threat counterexamples pass locally. It has no equilibrium-existence, mixed, stochastic, Bayesian, continuous, repeated, or subgame-perfect claim. The bounded S7 roadmap is complete. Arbitrary cyclic graphs, feedback, continuous-time open Markov black-boxing, unrestricted MDP black-boxing, tensor frameworks, general autodiff, devices, and production trainers remain out of scope.

## Instructions for future agents

1. Read this file, `TODO.md`, `docs/DECISIONS.md`, and the relevant architecture section.
2. Follow `docs/WORKFLOWS.md` and its evidence rules.
3. Do not preserve an incorrect API for compatibility.
4. Do not add hidden defaults, global randomness, partial functions, or unchecked numeric boundaries.
5. Do not claim a command passed without output from the current revision.
6. Update architecture, decisions, context, TODO, and README when their contracts change.
