# Repository context

Read this file, `TODO.md`, and the relevant architecture and decision sections before a semantic change.

## Current state

Markovian `2026.9.3.0` is the first coordinated 16-package release. D-053 through D-076 are `Accepted` only for their stated bounded, represented scopes. D-077's bounded evidence repair/policy and D-078's original strict-discount affine coefficients are post-release `Accepted` decisions as of 2026-09-08; they remain unreleased. D-079 Gate A plus frozen Gate B left-successor substitution and D-080 closed quotation plus frozen cumulative compilation are separately `Accepted` within their bounded, unreleased scopes. EL-03/EL-04/EL-05 and D-081 through D-085 remain `Proposed`. Acceptance authorizes no package edge, workflow launch, deployment, or publication. The root remains `base`-only and exact. Numerical, sampling, learning, continuous, reverse, autodiff, tensor, SafeTensors, neural, and CUDA execution remain in optional packages.

The release record states that the acceptance candidate passed the compiler, source, documentation, archive, checksum, SPDX, and attestation gates. It also records CUDA compile-only, protected GB10, benchmark, fault, and four-tool sanitizer passes. The repository does not retain enough raw same-session material to reconstruct the protected hardware assertions under D-077.

Release preparation produced 16 source archives, 16 SPDX 2.3 SBOMs, 29 component logs, complete SHA-256 checksums, and a 37-subject SLSA v1 provenance statement. The statement was verified against the repository, signer workflow, and source revision. Preparation remains non-publishing; publication requires a separate human request.

The exact semantic foundation exposes finite sets and objects, law-bearing scalar capabilities, nonnegative rationals, semiring matrices, stochastic and deterministic refinements, exact convex mixtures, payoffs, priors, support-restricted Bayesian inversion, purity-indexed circuits, deterministic compilation, structured cospans, bounded acyclic open denotation, finite games, three released accepted checked feedback fragments, and accepted but unreleased strict-discount affine feedback coefficients. Matrix dagger, Bayesian inversion, payoff pullback, reverse differentiation, feedback, boundary reversal, strategic duality, and disintegration remain distinct.

`markovian-reverse` owns bounded pure and effect-capable reverse programs. `markovian-autodiff` differentiates only its closed polynomial and `tanh` language. `markovian-tensor` owns host F64 buffers and primitive VJP tapes. `markovian-tensor-reverse` provides only the bounded host adapter. `markovian-safetensors` implements only the pinned metadata-free F64 profile. `markovian-gpu` admits only positive-size F64 matrix multiplication and its declared matrix VJP under one digest-addressed `sm_121` profile. Admission is not hardware correctness evidence.

The historical D-074 CUDA record does not establish general device correctness, arbitrary devices or dtypes, generic tensor graphs, generic reverse-program lowering, bitwise cross-device reproducibility, or GPU advantage. It cannot satisfy D-077 without the missing same-session receipts. The continuous packages do not establish arbitrary measurable callbacks, point conditioning, general disintegration, certified numerical bounds, or continuous MDP execution. The game packages verify bounded candidates but do not establish equilibrium existence or unrestricted solving. Universal feedback, arbitrary cyclic open black-boxing, arbitrary-Haskell autodiff, and production training remain out of scope.

The pinned mdBook at <https://josephjohncox.github.io/Markovian/> covers the current development API, explicitly separating released fragments from unreleased proposals, together with laws, counterexamples, mathematical foundations, inventory fixtures, and evidence boundaries. The [checked capability inventory](book/src/capabilities.md) separates availability, decision status and evidence scope; its selected released membership is pinned to the immutable release source, not the current snapshots. Display equations use the vendored MathJax 3.2.2 bundle and checked `\[` and `\]` delimiters.

The [S6 evidence record](evidence/S6-ACYCLIC-OPEN-2026-08-27.md) remains historical evidence for the earlier semantic-tower revision. Do not substitute its test counts for the complete release evidence.

The 2026-08-26 CUDA evidence used a final-correction worktree based on `2efb1c6`. The enabled test passed on an NVIDIA GB10 with driver 580.173.02 and compute capability 12.1. After one excluded warmup, 20 transfer-inclusive samples had a `267.843920400 ms` mean and `3.025869898 ms` sample standard deviation. The range was `263.519087000 ms` to `276.777522000 ms`, and maximum error was `0.000e0`. CUDA 13.0 `nvcc` V13.0.88 reproduced the committed PTX files. The [complete evidence record](evidence/CUDA-2026-08-26.md) retains raw samples and labels older mean-only results as historical execution records.

Feature commit `d99b2c011100d200934dce9e5993a7d070398b39` passed matching [push](https://github.com/josephjohncox/Markovian/actions/runs/33124880789) and [pull-request](https://github.com/josephjohncox/Markovian/actions/runs/33124883088) workflows. Both runs passed GHC 9.4.8, GHC 9.8.4, lower-bounds, and source-check jobs. PR #1 merged as `1268191a025c22fd9b995a1025d9ca810ff43451`.

## D-079 implementation integration

Parent authorized Gate B implementation in the sole-writer worktree `/home/josephcox/dev/Markovian-d079-contract`, branch `feat/d079-contract-freeze`, after contract review `8f9b416a-ce2d-4033-8d6a-a3350d107111` and freeze `a8fe21e12b2d036e8ec22f10e25c4d07a5374f6f`. That source branch predates D-077/D-078 acceptance. Integration preserves their Accepted statuses in this proposals branch; the source branch's earlier statuses are not current integration policy.

`Kernel/JointAffine/Exact.hs` now implements the frozen complete-declaration representation, declaration-complete Gate A renamers, and sealed left-successor substitution. Shared/fresh requests are revalidated against actual operands, freshness checks both full manifests, left reward is admitted but lost, and both projections preserve complete historical rational bits. The public suite uses independent symbolic/multinomial and witnessed-source corner oracles, exact/one-below and competing-failure fixtures. The boundary script runs a positive installed client, named compile failures, and private source-context probes of actual zero declarations, reservations, captured witnesses, machine counts, and rational order without installing private accessors. Historical retained-only Gate A records remain historical; the explicit Gate B supersession is not retroactive.

The reviewed implementation is committed at `080fe0e209af26bcd11d64befe392510860835f5`. Corrective readiness review `e4bfb40c-1cd5-4389-b986-e2c3a18e8e24` passed after the complete semantic review, both supported-compiler boundary suites, and warning-enabled Haddock checks. Final source-bound evidence is retained in `/tmp/d079-correction-validation-NnuNuzNF`. Those were implementation-stage results, not acceptance. Subsequent combined source/guard PASS `b4c2d0e8`, committed-integration PASS `ddb3d314` and decision-specific PASS `b4c30c96-f3ef-40ba-8ff0-bdf038e9e4df` support the separate parent-authorized bounded, unreleased acceptance at reviewed `12f3794`. Separate 13-file status-delta review `57601418` passed with P2, addressed by the changelog clarifications. Follow-up validation/review must precede the parent-owned commit; see [current acceptance evidence](DECISIONS.md#d-079d-080-acceptance-evidence-and-status-edit-boundary).

## Resolved defects

| ID | Resolution |
| --- | --- |
| K-001 to K-004 | Removed the ambiguous branch-weight `Action`, recursive `MDPF`, self-recursing pseudo-Mendler algebras, and unchecked probabilistic evaluators. |
| K-005 to K-009 | Removed the defective Q-learning implementation, hidden constants, unbounded negative recursion, partial vector operations, incorrect reward timing, and unstable action-name keys. |
| K-010 | Removed compatibility characterization tests with the deleted API and replaced them with semantic contracts. |
| K-011 | The library, sample, and tests now depend only on `base` and the local package. Obsolete lower-bound workarounds were removed. |
| K-012 | Package metadata, source distribution, and direct component dependencies are verified by CI checks. |
| K-013 | Replaced generated changelog text with a factual release entry. |
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
| K-041 | Reconciled the CI and source-archive inventory: all named inventory benchmarks and applicable compile-fail boundaries are explicit, including the finite-game boundary. |

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
| INV-TENSOR | Tensor parameter ownership, cotangent algebra, and physical storage identity remain separate; numerical primitives accept finite F64 values only. | Opaque owners/storage, finite refinement, nominal roles, tensor tests |
| INV-DEVICE | A CUDA executor owns one private context, module, and stream; calls synchronize before copy-back and retain cleanup failures. | Opaque serialized executor, safe FFI transaction, structured status, enabled tests |
| INV-FALLBACK | Automatic CPU fallback can occur only when configured and before the first user-kernel launch. | Committed-launch status and dispatch tests |
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
- D-068 defines the closed polynomial and `tanh` language, compiler-owned VJPs, exact polynomial execution, checked-Double execution, and bounded reverse lowering.
- D-059 defines finite reachable acyclic alternating arenas, opaque replay histories, receptive deterministic strategies, bounded synchronized composition, and exact external-prefix observational equality.
- D-060 defines bounded concrete finite optics and an owner-refined finite open-game fragment with exact rational decisions, pure contextual equilibrium enumeration, and best-response-sensitive observational equality.
- D-064 through D-066 define exact owned mixtures and CE/CCE, joint-outcome finite-horizon stochastic games, correlated-prior Harsanyi checks, bounded strategic-normal conversion, and the checked closed-context open-game bridge.
- D-072 defines the checked host F64 tensor runtime; D-073 admits only the pinned bounded metadata-free F64 SafeTensors profile while broader compatibility claims remain blocked; D-074 defines the prepared matrix/VJP CUDA fragment, admission, ownership, synchronization, cleanup, and fallback boundary.
- D-077 accepts bounded GPU evidence governance and the existing profile/receipt repair; hardware evidence remains bound only to `3e850085fa96c4e48a80270b9e49e9f55fe0f757`, not current HEAD. D-078 accepts only original strict-discount affine coefficients and four exact equations, not EL-04's JVP contract. Both dated acceptance records are in `docs/DECISIONS.md`.
- D-079 accepts only bounded, unreleased exact compact affine-Uniform joint kernels and frozen left-successor substitution: complete zero-inclusive declarations, actual-operand-checked all-shared/all-fresh injective requests, sealed non-chainable right reward/successor, admitted but unaccumulated left reward, cumulative ledgers and historical-bit projections. No temporal/general composition, category, mixed routing, disintegration, device or performance claim follows.
- D-080 accepts only bounded, unreleased closed exact-polynomial quotation and frozen cumulative compilation: nominal lexical environments/paths, generative construction tokens, callback-free stored lets, call-by-value unused bounds, independent Rational primal/JVP and distinct owned reverse lowering, numeric planning and logical reservation before strict admission and target construction. Capacity/failure/report contracts are accepted, not arbitrary-Haskell/higher-order AD, control flow, effects, sampling, tensors, devices, nested differentiation, vector packing, physical allocation/peak memory or performance.
- D-081 through D-085 still propose affine tensor views, CUDA multiply chains, CE/CCE witnesses, a reference DQN trainer, and interpreter-relative tabulation. Read D-081's [reviewed r4 materialization addendum](plans/D081-MATERIALIZATION-ADDENDUM.md) with its unchanged [full v3 plan](plans/D081-AFFINE-VIEWS.md): r4 supersedes only reviewed resource/producer/failure/fixture seams. Parent has adopted the r4 contract after design/documentation PASS and the pre-closeout audit; the [freeze record](evidence/D081-MATERIALIZATION/FREEZE-RECORD.md) binds that decision. The next gate is separate runtime-continuation authority; final closeout review and parent byte audit/staging/commit remain. D-081 remains Proposed and runtime continuation held. D-082 remains blocked on D-081 acceptance; the other proposals' signatures, failure precedence, ownership, topology, limits and evidence remain under review.

## Executable-learning implementation status

EL-01 contracts and EL-02 teaching passed independent review; the supplied EL-03
implementation review passed with a non-blocking opacity-probe defect, now repaired
and mutation-tested. EL-04's fixed-topology event-reward JVP is implemented in the
exact root under the frozen contract, with one cumulative base-plus-derivative
meter, exact layouts, nominal ownership, independent determinant/dual-unrolling
tests and an executed retry lesson. Its probabilities and discount are fixed;
the lesson's symbolic probability derivative is explicitly outside the API.
EL-04 passed the supplied independent implementation review. EL-05 now implements
supplied-partition fixed-policy aggregation in the exact root: an opaque owned
quotient or bounded distinguishing witness preserves joint reward/next-block laws,
declared observations and terminal/payoff timing under one cumulative meter.
Independent finite-value/trace fixtures and an executed success/witness lesson
provide unreleased evidence. EL-00 through EL-07 are complete for the bounded local program.
The [integration record](evidence/EXECUTABLE-LEARNING-INTEGRATION.md) records independent reviews and the parent audit.
The [durable checklist](plans/EXECUTABLE-LEARNING-TODO.md) retains commands and residual limits.
The deployment-scoped policy passed independent review and parent validation. Separate 2026-09-08 bounded D-077/D-078 and D-079/D-080 acceptance records follow decision-specific readiness PASS reviews and parent authorization. The current queue starts with D-081 physical repository-freeze review in the [all-nine checklist](../TODO.md#all-nine-execution-checklist), not runtime implementation. Separate 13-file D079/D080 status-delta review `57601418` passed with P2, addressed by the changelog clarifications; final review `11ddbb94` passed and parent committed the acceptance record at `78669c3613302249c499eba99a959f75c4c59edc`.
Apply the [2026-09-08 deployment-scoped evidence amendment](WORKFLOWS.md#82-deployment-scoped-gpu-evidence-amendment--2026-09-08).
The historical executable-learning program did not itself accept these decisions. The separate D-077/D-078 and D-079/D-080 acceptances change no package versions, topology, or immutable released evidence. EL-03/EL-04/EL-05 remain Proposed despite their implementation evidence.

## Post-release boundaries

S7.1 information, categorical-learning, polarity, and game-semantic foundations are implemented. S7.3 now includes exact payoff pullback, bounded static circuit costs, and exact deterministic rewrite certificates. The neural softmax/cross-entropy fusion has separate checked-`Double` differential evidence and no exact certificate. S7.2's source-bounded inventory slices remain implemented within their stated limits. D-058 extends the small typed parametric reverse interpreter with a finite acyclic syntax over caller-owned primitives, structural ownership, finite primal and cotangent layouts, bounded structural preparation, and opaque stored or recomputed tapes. Recomputation is a distinct typed owner operation. Its pure implementation is extracted to `markovian-reverse` and interprets supplied VJPs; it has no optimizer, tensor runtime, general autodiff, recursion, cycle, or checkpoint-scheduling semantics. Exact laws, structural cycle/depth rejection, deterministic reports, callback failures, repeated tapes, and all-coordinate finite differences under both tape policies pass locally. D-054's accepted exact-availability-to-structural-neural-mask bridge has bounded traversal and nominal-role protection across the explicit bridge package. D-055's Clark--Scarf (1960), Section III, finite lattice specialization preserves the source state and timing, compares three exact equation paths, and reports finite cap widening without an unbounded claim. D-056's Doğru adaptation keeps physical and signed-relaxed actions distinct and compares exact bounded oracles with balanced and physicalized policies. D-057's fixed-batch implementation keeps exact finite-horizon execution visibly separate from stationary newsvendor evidence on explicit finite Cartesian `R1`/`R2` layouts, retains and checks stationary provenance, and reports truncation and widening only as diagnostics. D-059's accepted finite alternating protocol is implemented in the root exact core. Arena construction and equality are bounded; strategy construction uses one conservative operation-wide work account; composition is explicitly partial after hiding. Copycat, representative successful associativity, checked equality, layout, rejection, and compile-fail fixtures pass locally. This is not Hyland--Ong or AJM semantics and supplies no closure, category-law, justification, or innocence result. D-060's separate finite open-game fragment is accepted within its stated finite scope. It checks structural owner-disjoint strategy products, pre-allocation product bounds, canonical continuations, strict performed counts, incumbent-sensitive sequential and tensor composition, bounded pure equilibrium enumeration, and exhaustive best-response-sensitive observational equality. D-064 through D-066 add a separate exact normal-form and one-shot dynamic/incomplete-information layer; they do not make the generic open-game callback mixed. The closed-context bridge requires explicit owner-local replacement evidence and an exact differential check. No equilibrium-existence, unrestricted solving, repeated-game, private-history, or continuous-game claim follows. The bounded S7 roadmap is complete. Arbitrary cyclic graphs, universal feedback, continuous-time open Markov black-boxing, unrestricted MDP black-boxing, general tensor frameworks, generic reverse-program device lowering, general autodiff, general device correctness, and production trainers remain out of scope.

## D-080 cumulative compilation implementation handoff

**Historical handoff:** This section preserves the original implementation/modernization sequence and its then-pending gates and statuses. The current completion and bounded acceptance below supersede those dispositions, not the original failures or execution attribution.

The implementation-stage repair on `feat/d080-contract-repair`, based on
`30276390d9fce1433ab7eca543efdda8e3b13a4b`, adds a private numeric planner and
compiler coupons in `Quote.hs`. One syntax/planner/reservation ledger precedes
a strict witness, target construction, and unchanged standalone compilation.
Derived capacities retain caller rational bits. No public exports or package
edges change. Both historical freezes remain unchanged; D-080 remains Proposed.

### Historical pre-review-repair handoff

The following records the initial implementation, not current source-check or
mutation coverage. Its formatter/89-hint failures and seven-mutant count were
superseded by the review repairs below.

`QuoteCompilationBudgets` checks literal product-owner k=1/2/4/128 budgets,
small quotations, embedded projections, cumulative owner-list association,
aggregate extents, caller bits and primitive cases. `QuoteCompilationFixture`
ports the approved associated 79-unit/81-product tree and all 72 direction /
432 retained-tape seed cases. The current-unit boundary also compiles the real
local autodiff/reverse sources with CPP-only events and rejects seven mutations.
The public pure tape is the Identity specialization of `Program/Effect.hs`, so
actual runtime tape sites there are instrumented separately from preparation.
An initial probe that instrumented only Internal runtime constructors failed
its positive tape control and was corrected; that failure is not success evidence.

Independent implementation review, complete frozen-matrix audit and broader
compiler/archive gates remained parent-owned. Fourmolu then could not parse the
function-like CPP instrumentation macros, and normal HLint preprocessing reports
redundant identity wrappers (89 hints). These source-check failures are not
waived or presented as compiler failures. The normal preprocessed quotation
contains no probe sink, hook reference or event labels. All 18 local GHC 9.8.4
`-Werror` suites passed; the final additional shared-owner budget test also
passed the focused suite. Production-path probes and seven mutation controls
passed, including an archive-only focused run. Detailed command
logs, including initial failures and recovery, are in
`dist-newstyle/d080-implementation-logs/` in the implementation worktree.

### Reviewed source handoff (D-080 remains Proposed)

Instrumentation now uses object-like CPP prefixes/events whose normal expansion
is empty. All five production inputs erase hooks normally; exact repository
Fourmolu and HLint checks are clean. The private production-path boundary retains
installed-current-unit opacity, isolated package DBs and frozen home-source flags,
and rejects **ten compiled runtime mutants** plus a separate preprocessing-only
normal-hook leakage control. The exact intermediate-total, duplicate-error,
let-order and k=1/2/4/128 policy controls are implemented. Prior review closed
those source-check/accounting/probe findings; it did not accept D-080.

The remaining source-layout review repair explicitly selects the package's own
consumer project and matching versioned sibling reverse archive, or proves the
actual repository package location before selecting repository sources. An
incidental grandparent project cannot select checkout reverse sources. Missing
or mismatched intended reverse sources reject without fallback. Earlier extracted
**repository-shaped** archive passes in `dist-newstyle/d080-review-repairs-logs/`
remain valid only for that layout, not the existing standalone CI consumer.
The separate sibling-extraction regression and final-byte validation evidence is
in `dist-newstyle/d080-layout-repair-logs/`; original failures remain preserved.

Independent layout/implementation review `6e4750c9` passed for the bounded
repair, committed at `6c050d4563ef45e7728b5bf9cddd5c7602b3176d`.
Parent integrated it with the partial modernization into
`e56981a536e12d01be552a8a94f17ea077b61fbe`. Incoming Haskell bytes match
that D-080 commit except the modern compiler's typed `ownedParameterValue`
accessor repair in `addParameters`; its ownership boundary is not waived.
The merge retained the pre-integration teaching digest as stale, not as a refresh.

The current combined validation campaign starts from e56981a. The approved
modern-only policy in `DECISIONS.md` supersedes legacy two-compiler obligations:
all project gates use GHC 9.14.1/Cabal 3.18.1.0 and new-bound prefer-oldest plans.
Only standalone HLint/cabal-fmt construction uses the separately pinned 9.8.4
bootstrap compiler, through the same installer in bootstrap and CI, without
changing global selection. Old source checks, archives and executions above
remain attributed to their original bytes/toolchains, not combined validation.

Current commands, failures, source inventories and both baseline-relative diffs
are retained in `/tmp/markovian-modern-combined-e56981a/`. Combined readiness
requires current all-package tests/benchmarks/boundaries, actual teaching receipt,
all-16-package warning-enabled installed Haddock package logs/interfaces, coverage,
fresh archive membership and standalone outside-checkout consumers. The final
implementation report records executed gates and any remaining gaps; this
handoff does not predeclare PASS. Actual modern package-log inspection found
unresolved private Haddock links despite a clean parent installation log.
The corrective pass in `/tmp/markovian-doc-ci-repair-fpmt26od/` preserves that
failure evidence. Feedback.Value.Exact now renders its private prose name as
text. Autodiff.Quote uses Haddock 2.33's supported `redact-type-synonyms` and
spells the EnvironmentShape equations through the original public aliases.
Redaction alone still warns on the family equations. The runtime datatype,
fully saturated aliases, signatures, constructor opacity and nominal roles
remain unchanged; interface fingerprints change, so reinstallation is required.
CI now captures a fresh installation and checks all 16 public plus one private
library unit logs, bound to source archives, manifest, compiler and actual plan,
and all 16 installed interfaces. Coverage suppression is not this warning gate.

The old installed HLS binary's ABI failure remains retained evidence; it is not
the new project's launcher. Final integration uses the scratch-proven, byte-pinned
official source installer/seal/full-ABI guard through `scripts/project-hls.py` in
bootstrap and `.envrc`. Its complete absolute project-local build/store must be
preserved; failed construction or runtime identity drift fails closed, without
resealing, generic-wrapper fallback or global selection changes. The original
source/recipe proof remains in `/tmp/markovian-hls-official-recipe-b9026de3/`;
new commands, source/archive bindings and actual disposition are in
`/tmp/markovian-final-integration-b9026de3/`. Construction, reused construction,
ABI checking and actual project LSP operation are distinct evidence categories.
The raw upstream targets omit `ghc-check`, so the mandatory external full boot DB
and linked-runtime guard is the check actually executed, not an invented
in-process check. The approved tool-only bounds exception and upstream GHC 9.14
plugin limitations are recorded in `DECISIONS.md` and README. No standalone
formatter restores the unavailable integrated plugins. Read the final report
for completed gates and residual blockers; this prose does not predeclare PASS.
The subsequent P1 selector review found the historical guard authenticated absolute
tools but not the PATH selectors used by the Cabal cradle. The focused repair
captures the complete compiler/Cabal selector mapping during construction, binds it
to receipt/seal and validates it before guarded subprocesses. Current recipes use
canonical recipe-digest build roots and explicit installation, preserving the old
unsuffixed build unchanged. Repair evidence and reused-versus-fresh attribution
are in `/tmp/markovian-selector-repair/`; launch-time validation does not claim
protection against concurrent filesystem mutation. No upstream source/dependency
recipe or mathematical behavior changes are part of this repair.
Independent combined review remains required. D-077/D-078 stay Accepted;
D-079/D-080 stay Proposed and unreleased.

## Current modernization and acceptance completion — 2026-09-08

Modernization is locally complete at reviewed `12f3794942485b5e525def6ee470a4ea22a9f13e`; the historical handoff above is not the current blocker list. Combined source/guard PASS `b4c2d0e8`, committed-integration PASS `ddb3d314`, and `/tmp/markovian-modern-parent-final-owxkm3hw/result.json` bind the 579 reviewed source blobs/modes and retained evidence. Actual committed-checkout tests, D079/D080 boundaries and teaching executions are in `/tmp/markovian-proposals-committed-validation/`; `reuse-proof.json` binds the earlier current-byte archive/lower-bound/benchmark/documentation campaign without calling it a fresh execution.

GHC 9.14.1 / Cabal 3.18.1.0 / base 4.22 / bytestring 0.12.2 remain the core baseline; GHC 9.8.4 is ancillary tool construction only. The isolated official-source HLS recipe and selector/seal/full-ABI guard remain unchanged. Upstream GHC 9.14 integrated-plugin limits, absolute local build/store retention, no wrapper/global fallback and no protection against concurrent filesystem mutation remain explicit. Actual LSP operation is distinct from construction/reuse/ABI evidence; no unavailable plugin or broader HLS acceptance is claimed.

Parent separately authorizes bounded, unreleased D079/D080 acceptance after their respective technical audits `b4c30c96-f3ef-40ba-8ff0-bdf038e9e4df` and `98a7de79-1e4f-4539-aa61-80cf332a2ed7`. See the [decision evidence and status-delta boundary](DECISIONS.md#d-079d-080-acceptance-evidence-and-status-edit-boundary). Separate independent status-delta review `57601418` passed for the 13-file delta with P2. Dated continuous/autodiff changelog clarifications address that note while preserving the original entries and released sections. Final status-delta review `11ddbb94` passed after follow-up validation; parent committed that acceptance record at `78669c3613302249c499eba99a959f75c4c59edc`. Nothing is released; EL-03/EL-04/EL-05 and D-081–D-085 remain Proposed. No hosted-CI, hardware, attestation, deployment, topology, tooling or released-history change follows.

## Current D-081 adopted-contract handoff

Read the [r4 materialization addendum](plans/D081-MATERIALIZATION-ADDENDUM.md), then the unchanged [canonical31bf contract](plans/D081-AFFINE-VIEWS.md) and [durable proof/fixture/provenance bundle](evidence/D081-MATERIALIZATION/README.md). The original full v3 design approval and review `5ba479ba-964e-4da4-8f95-51831227309b` remain historical. Parent has adopted the r4 contract under `/tmp/d081-r4-freeze-closeout-authority.md`, after independent DESIGN PASS `8992df58-e052-4b23-9fd9-345febebf9d2`, documentation PASS `7f1a1fa6-ec54-4b26-a642-e33011ab9b74` on the 25-file prepared patch, and the pre-closeout parent audit of 7475 campaign entries, 597 proposal-source files and 16 archives. The [freeze record](evidence/D081-MATERIALIZATION/FREEZE-RECORD.md) binds the decision and distinguishes identity auditing from retained executions. The addendum supersedes only reviewed resource/producer/failure/fixture seams and consolidates the five clarifications; its preface overrides original scratch-stage framing without changing mathematical bytes. D-081 stays Proposed, D-082 blocked, D-083 placement unanswered, EL-03/04/05 and D-084/85 unchanged.

Held417337, historically executed a650c2e0 and scratch-r4 `8b0b50628bf10e260f6048e2fe96dcc10359de31c7958591fecfbc6aef98dee5` are different source identities. The held implementation's 12-file unstaged delta remains protected and unvalidated as a whole; scratch-r4 is not applied. Operative fixtures control over the parent's earlier mistaken historical-table selection. Logical slots and demand counters are not native allocation measurements; r2 scalar and v2 producer controls do not certify the r4 whole runtime. All helper failures and earlier-first-live behavior remain documented.

Lifetime A is unchanged: payload-dependent work must execute and complete within the callback with caller joins or cancel-and-joins on every exit. There is no automatic join, universal closed rejection, close/read synchronization, prompt reclamation or supported post-finalization use. This checkout adds no Affine exposure, module membership, dependency, version or release claim: 133 public modules and all 16 versions `2026.9.3.0` remain.

NEXT: obtain separate parent runtime-continuation authority for the adopted contract. Final independent review of this narrow status closeout and parent byte audit/staging/commit still remain; no Git commit or integration is claimed. Runtime continuation remains held; no full D081 runtime/mutation/fault/installed-unit/archive-consumer gate is discharged here. Original `/tmp/d081-repository-freeze-61823aec/` and `/tmp/d081-proof-assembly-QQfz0BTQ/` evidence remain historical and immutable. Fresh documentation validation uses a new guarded modern campaign bound to the clean proposals source plus this delta, not held/candidate runtime code. Existing tooling, all three absolute HLS stores and all old failures remain protected.

## Instructions for future agents

1. Read this file, `TODO.md`, `docs/DECISIONS.md`, and the relevant architecture section.
2. Follow `docs/WORKFLOWS.md` and its evidence rules.
3. Do not preserve an incorrect API for compatibility.
4. Do not add hidden defaults, global randomness, partial functions, or unchecked numeric boundaries.
5. Do not claim a command passed without output from the current revision.
6. Update architecture, decisions, context, TODO, and README when their contracts change.
