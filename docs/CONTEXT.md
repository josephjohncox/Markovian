# Repository context

Read this file, `TODO.md`, and the relevant architecture and decision sections before a semantic change.

## Current state

Markovian is a greenfield experimental Haskell package for finite stochastic kernels and decision models. It has no compatibility commitment and no external users. Incorrect interfaces are removed rather than preserved.

The core library exposes validated floating and exact probability, reward, objective, kernel, MRP, MDP, policy, sampling, trace, finite-compilation, tabular-learning, exact POMDP, categorical IR, and dense CPU modules. Exact interpreters provide direct bounded expectation, trace enumeration, finite-horizon dynamic programming, contraction Bellman policy evaluation, filtering, and bounded belief planning. Sampled evaluation and episodic Q-learning receive and return explicit generator state. Separate GPU and neural packages depend on no semantic-core runtime framework.

The package does not contain the former branch-weight process, recursive `MDPF`, or Q-learning implementation. `app/Sample/Main.hs` demonstrates the exact finite-horizon evaluator.

`test/Main.hs` contains thirty-two deterministic semantic, law, closure, sampling, compilation, solver, learning, POMDP, categorical, CPU-lowering, trace, and interpreter contracts. Separate GPU and neural package tests cover disabled-backend behavior, actual CUDA differential execution, stable softmax, analytic gradients, and approximation error. GHC 9.4.8 and 9.8.4 pass project-scoped `-Werror`, source checks, package checks, warning-free Haddock, lower-bound resolution, and unpacked source-distribution tests. The complete greenfield hosted run passed at <https://github.com/josephjohncox/Markovian/actions/runs/32920753099>.

## Resolved defects

| ID | Resolution |
| --- | --- |
| K-001 to K-004 | Removed the ambiguous branch-weight `Action`, recursive `MDPF`, self-recursing pseudo-Mendler algebras, and unchecked probabilistic evaluators. |
| K-005 to K-009 | Removed the defective Q-learning implementation, hidden constants, unbounded negative recursion, partial vector operations, incorrect reward timing, and unstable action-name keys. |
| K-010 | Removed compatibility characterization tests with the deleted API. Nineteen semantic contracts remain. |
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

## Current decisions

- D-001 through D-007 define the semantic foundation and objective boundaries.
- D-011 defines transition rewards and terminal payoffs.
- D-015 preserves joint reward and successor outcomes during policy closure.
- D-016 defines terminal-aware Bellman equations.
- D-017 requires overflow-safe floating normalization.
- D-023 and D-024 define the reproducible toolchain and pinned CI matrix.
- D-026 through D-029 define exact values, exact Kleisli laws, validated policy closure, and exact finite expectation.
- D-030 supersedes compatibility and migration decisions and authorizes immediate removal of defective experimental APIs.

## Next task

All roadmap phases P0 through P6 are `DONE`. Further work must enter through an admission gate with a concrete use case, law or approximation contract, and required benchmark evidence.

## Instructions for future agents

1. Read this file, `TODO.md`, `docs/DECISIONS.md`, and the relevant architecture section.
2. Follow `docs/WORKFLOWS.md` and its evidence rules.
3. Do not preserve an incorrect API for compatibility.
4. Do not add hidden defaults, global randomness, partial functions, or unchecked numeric boundaries.
5. Do not claim a command passed without output from the current revision.
6. Update architecture, decisions, context, TODO, and README when their contracts change.
