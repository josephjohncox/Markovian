# Repository context

Read this file, `TODO.md`, and the relevant architecture and decision sections before a semantic change.

## Current state

Markovian is a greenfield experimental Haskell package for finite stochastic kernels and decision models. It has no compatibility commitment and no external users. Incorrect interfaces are removed rather than preserved.

The core library exposes validated floating and exact probability, reward, objective, kernel, MRP, MDP, policy, sampling, trace, finite-compilation, tabular-learning, exact POMDP, categorical IR, and dense CPU modules. Its exact semantic foundation also exposes duplicate-free finite sets, nonempty finite objects, law-bearing scalar capabilities, nonnegative rational scalars, opaque semiring matrices, normalized stochastic matrices, proof-carrying deterministic matrices, exact convex mixtures, normalized priors, positive supports, conditioning, prior-indexed Bayesian inversion, checked Bayesian channels, raw purity-indexed stochastic-circuit syntax, first-order quoted-table compilation, finite typed hypergraphs, structured cospans, and commuting open-system cells. Both finite-witness modules export the canonical `sameFiniteLayout` operation. They retain `sameFiniteSetLayout` and `sameFiniteObjectLayout` as descriptive aliases. Exact interpreters provide direct bounded expectation, trace enumeration, finite-horizon dynamic programming, contraction Bellman policy evaluation, filtering, and bounded belief planning. Sampled evaluation and episodic Q-learning receive and return explicit generator state. Separate GPU and neural packages depend on no semantic-core runtime framework.

The package does not contain the former branch-weight process, recursive `MDPF`, or defective prototype Q-learning implementation. `app/Sample/Main.hs` demonstrates the exact finite-horizon evaluator.

`test/Main.hs`, `test/AlgebraicFoundation.hs`, `test/BayesianExact.hs`, `test/StochasticCircuit.hs`, and `test/OpenSystems.hs` contain fifty-six deterministic contracts. The named `canonical finite-layout API aliases` contract imports and tests `sameFiniteLayout` from both finite-witness modules. Separate GPU and neural package tests cover disabled-backend behavior, actual CUDA differential execution, stable softmax, analytic gradients, and approximation error. The semantic-tower revision has local GHC 9.4.8 and 9.8.4 project-scoped `-Werror` build and test evidence. Source, package, compile-fail, unpacked source-archive, warning-free Haddock, CUDA-disabled, neural, and actual CUDA differential gates also pass locally.

The 2026-08-26 CUDA evidence used a final-correction worktree based on `2efb1c6`. The enabled test passed on an NVIDIA GB10 with driver 580.173.02 and compute capability 12.1. After one excluded warmup, 20 transfer-inclusive samples had a `267.843920400 ms` mean and `3.025869898 ms` sample standard deviation. The range was `263.519087000 ms` to `276.777522000 ms`, and maximum error was `0.000e0`. CUDA 13.0 `nvcc` V13.0.88 reproduced the committed PTX files. The [complete evidence record](evidence/CUDA-2026-08-26.md) retains raw samples and labels older mean-only results as historical execution records.

Commit `2efb1c690211fd7b925c64a212b331ea20d8459b` passed matching [push](https://github.com/josephjohncox/Markovian/actions/runs/33039698248) and [pull-request](https://github.com/josephjohncox/Markovian/actions/runs/33039701651) workflows. Both runs passed GHC 9.4.8, GHC 9.8.4, lower-bounds, and source-check jobs. PR #1 records hosted evidence for its current head.

## Resolved defects

| ID | Resolution |
| --- | --- |
| K-001 to K-004 | Removed the ambiguous branch-weight `Action`, recursive `MDPF`, self-recursing pseudo-Mendler algebras, and unchecked probabilistic evaluators. |
| K-005 to K-009 | Removed the defective Q-learning implementation, hidden constants, unbounded negative recursion, partial vector operations, incorrect reward timing, and unstable action-name keys. |
| K-010 | Removed compatibility characterization tests with the deleted API and replaced them with semantic contracts. |
| K-011 | The library, sample, and tests now depend only on `base` and the local package. Obsolete lower-bound workarounds were removed. |
| K-012 | Package metadata, source distribution, and direct component dependencies are verified by CI gates. |
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
| INV-RNG | Reproducible stochastic execution receives and returns explicit generator state. | Implemented by P2.2 |
| INV-CORE | The semantic core has no tensor, GPU, autodiff, neural, or sampling-framework dependency. | Cabal dependency gate |
| INV-COPY | Copy targets the full tensor square and assigns mass only to diagonal pairs. | Opaque IR constructors and exact law tests |
| INV-MATRIX | Matrix semantics compare labelled entries and supports, not storage layout or exact-distribution representation. | Opaque matrices and `matrixEquivalent` laws |
| INV-NORMALIZED | Stochastic matrix rows sum exactly to one; transpose and trace are raw-matrix operations only. | Opaque normalized refinement and counterexample test |
| INV-DETERMINISTIC | Copy-naturality reasoning requires proof-carrying deterministic provenance. | Opaque deterministic refinement and total forgetful embedding |
| INV-BAYES | Bayesian inversion is prior-indexed and defined only from positive output support to positive input support. | Opaque inverse, structured zero evidence, and exact joint laws |
| INV-CIRCUIT-PURITY | Copy-naturality rewrites require deterministic construction provenance. | Purity index, one-way weakening, and compile-fail checks |
| INV-CIRCUIT-SHARING | One stochastic execution followed by copy differs from duplicated execution. | Distinct share and fanout nodes with differential laws |
| INV-OPEN-REVERSAL | Boundary reversal swaps cospan legs and does not reverse dynamics or circuit state parameters. | Same-oriented state types and a separate view with no reverse-denotation observer |
| INV-OPEN-CELLS | Higher cells preserve types, labels, ordered incidence, and both boundary squares. | Opaque validated `OpenSystemCell` |

## Current decisions

- D-001 through D-007 define the semantic foundation and objective boundaries.
- D-011 defines transition rewards and terminal payoffs.
- D-015 preserves joint reward and successor outcomes during policy closure.
- D-016 defines terminal-aware Bellman equations.
- D-017 requires overflow-safe floating normalization.
- D-023 and D-024 define the reproducible toolchain and pinned CI matrix.
- D-026 through D-029 define exact values, exact Kleisli laws, validated policy closure, and exact finite expectation.
- D-030 supersedes compatibility and migration decisions and authorizes immediate removal of defective experimental APIs.
- D-031 through D-037 define sampling, compilation, learning, POMDP, backend, and finite Markov-category contracts.
- D-038 authorizes the staged exact matrix, Bayesian, circuit, deterministic compiler, and syntax-only open-system tower while separating its three reversal operations.
- D-039 defines support-restricted prior-indexed Bayesian inversion, almost-sure equality, and checked Bayesian-channel prior flow.
- D-040 defines recursive purity-indexed circuits, exact structural folds, approximation boundaries, and the supported first-order deterministic compiler fragment.
- D-041 defines finite typed structured cospans, explicit pushout witnesses, commuting open-system cells, and the narrow directed circuit decoration denotation.

## Next task

Exact semantic tower stages S1 through S5 are complete. S6, a partial interpreter for a separately validated acyclic boundary-functional hypergraph fragment, is the next ready stage. The current global circuit decoration is not graph black-boxing. Arbitrary open-system black-boxing and feedback remain deferred.

## Instructions for future agents

1. Read this file, `TODO.md`, `docs/DECISIONS.md`, and the relevant architecture section.
2. Follow `docs/WORKFLOWS.md` and its evidence rules.
3. Do not preserve an incorrect API for compatibility.
4. Do not add hidden defaults, global randomness, partial functions, or unchecked numeric boundaries.
5. Do not claim a command passed without output from the current revision.
6. Update architecture, decisions, context, TODO, and README when their contracts change.
