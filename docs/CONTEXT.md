# Repository context

Use this document to resume work. Read `docs/DECISIONS.md` and the relevant architecture section before a semantic change.

## Current state

Markovian is an early Haskell prototype. One Cabal library exposes the legacy `Markovian` and `QLearning` modules plus the additive `Markovian.*` core modules.

The source lazily embeds recursive process children in `MDPF`. Evaluators force that tree during sampling, expectation, and experimental Q-learning.

The examples remain legacy demonstrations. The legacy `Markovian` and `QLearning` semantics remain unchanged.

D-022 authorized the additive Foundation Kickoff slice. The core now defines opaque floating and separate exact rational probability, distribution, reward, and discount values. It also defines horizons, one-layer kernels, policies, and one-step MRP and MDP interfaces. Action IDs and stochastic transition outcomes are separate types. Terminal state inspection returns a terminal payoff without running a transition kernel. Floating normalization removes every mass that rounds to zero before it exposes support.

`test/Main.hs` contains seventeen named deterministic contracts, including four legacy characterization tests and exact functor and kernel laws. `Markovian.cabal` exposes the new modules and lists all durable documents in `extra-doc-files`.

D-023 defines the local toolchain. GHCup supplies GHC 9.8.4, Cabal 3.16.1.0, HLS 2.14.0.0, and Fourmolu 0.20.0.0. The project stores HLint 3.10 and cabal-fmt 0.1.12 under the ignored `.direnv/` directory. `toolchain.env`, `.envrc`, and `scripts/bootstrap-tools` reproduce this environment. `cabal.project` and `cabal.project.ci` define the local build.

P0, P1.1, and P1.2 are `DONE`. The core has separate exact and floating values plus a law-bearing exact kernel. Package checks, project-scoped `-Werror` builds, seventeen tests, warning-free Haddocks, format checks, normal and lower-bound plans, and unpacked source-distribution builds pass. GHC 9.4.8 and 9.8.4 are verified. The complete hosted run passed at <https://github.com/josephjohncox/Markovian/actions/runs/32784091295>.

`README.md` was already modified before this slice. `TODO.md` and the `docs/*.md` files were already untracked. The current writer retained and extended those changes.

## Known defects

| ID | Evidence | Defect and effect |
| --- | --- | --- |
| K-001 | `src/Markovian.hs:16-27` | `Action` combines an action name, a branch weight, and one successor. Evaluation treats the branch as an outcome. Q-learning treats it as a controllable action. |
| K-002 | `src/Markovian.hs:34-54,79-94` | `buildMDPF` lazily embeds a recursive `MDPF` child for each successor. Root construction need not diverge. An evaluator forces descendants and can diverge on a reachable cycle or infinite path. |
| K-003 | `src/Markovian.hs:63-70,78-94` | The code advertises Mendler recursion. The algebras call themselves and bypass the supplied recursive function. |
| K-004 | `src/Markovian.hs:80-96` | Empty supports and invalid weights reach partial indexing, `categorical`, or unchecked normalization. Negative weights, non-finite weights, zero totals, and overflowed totals remain possible. |
| K-005 | `src/QLearning.hs:16-23` | Learning rate, discount, and exploration rate are hidden constants. |
| K-006 | `src/QLearning.hs:26-30` | A negative episode count never reaches the zero case. |
| K-007 | `src/QLearning.hs:33-64` | Learning ignores transition weights and uses partial vector operations. Terminal rewards do not enter the Q target. |
| K-008 | `src/QLearning.hs:71-98` | The stateful path recurses before it updates. It differs from online Q-learning and bypasses the recursion argument. |
| K-009 | `src/QLearning.hs:13,38,91` | Q-table keys use state identity and action names. Duplicate action names collide. |
| K-010 | `test/Main.hs` | Resolved: seven core contracts and four legacy characterization tests pass on GHC 9.4.8 and 9.8.4. |
| K-011 | `Markovian.cabal` and `cabal.project.ci` | Resolved: direct imports are declared per component, normal and `--prefer-oldest` plans pass, and D-025 records required transitive corrections. |
| K-012 | `Markovian.cabal` | Resolved: package metadata passes `cabal check`, all durable files enter the source tarball, and the unpacked tarball builds and tests. |
| K-013 | `CHANGELOG.md:1-5` | The release date and entry are generated placeholders. No release evidence exists. |
| K-014 | `app/Sample/Main.hs:29-35` and `app/QLearning/Main.hs:31-37` | Both examples encode stochastic branches with the ambiguous legacy `Action`. |
| K-015 | `src/Markovian/Probability.hs` and `test/Main.hs` | Resolved: normalization removes masses that round to zero, and the passing regression checks that exposed outcomes have positive mass. |
| K-016 | `cabal.project.ci` | Workaround active: upstream lower bounds admit broken vty, mwc-random, and unix-compat plans. D-025 defines the smallest tested constraints. |

## Semantic vocabulary

Use one term for each concept.

- **Finite distribution**: a validated, nonempty finite support with nonnegative mass and total mass one.
- **Stochastic kernel**: a function from one input to a distribution over outputs.
- **Action ID**: the stable identity of an agent choice.
- **Transition outcome**: one transition reward paired with its successor state.
- **Transition outcome kernel**: a stochastic kernel from a state or state-action pair to a transition outcome.
- **Policy kernel**: the stochastic kernel from a state to an available action ID.
- **Reward model**: the finite reward assigned to a transition, plus an optional terminal payoff.
- **Objective**: the horizon, discount, and return rule used by an interpreter.
- **Interpreter**: code that evaluates, samples, learns, solves, or lowers a model.
- **Bellman operator**: the value transformation for one objective step.
- **Model syntax**: an explicit recursive program representation. It is not the state graph itself.

Use **MRP**, **MDP**, and **POMDP** only for the explicit interfaces in the target architecture. Call the current branch-weight model the **legacy process**.

## Non-negotiable invariants

| ID | Invariant | Target enforcement |
| --- | --- | --- |
| INV-ACTION | An action ID and a stochastic outcome are different types. | Module boundaries and opaque constructors |
| INV-DIST | A finite distribution is nonempty, finite, nonnegative, and normalized without floating overflow. | Opaque type, scaled normalization, and smart constructor |
| INV-REWARD | Every reward and terminal payoff is finite. | Opaque type and smart constructor |
| INV-AVAILABLE | A nonterminal MDP state has at least one available action ID. | `NonEmpty` result or structured model error |
| INV-POLICY | A policy assigns mass only to available action IDs. | Validation at policy closure |
| INV-OBJECTIVE | Every evaluation receives an explicit objective. | Required function argument |
| INV-TERMINAL | An interpreter does not request actions or transitions from a terminal state. | Control flow and tests |
| INV-HORIZON | Bounded evaluation stops at its finite horizon, including on self-loops. | Natural-number type and tests |
| INV-CYCLE | Cyclic models use bounded evaluation or a stated fixed-point solver. | API separation and tests |
| INV-QKEY | A Q-table key contains stable state identity and action ID. | Public key type and uniqueness checks |
| INV-RNG | A reproducible interpreter receives an explicit seed or generator. | Required configuration |
| INV-CORE | The semantic core has no tensor, GPU, autodiff, neural, or sampling-framework dependency. | Package dependency checks |

## Current decisions

- D-001 to D-007 define the accepted semantic foundation and boundaries.
- D-008 selects backend candidates without adding dependencies.
- D-009 defers advanced categorical optimization until a use case passes its proof gate.
- D-010 defines additive migration and two explicit legacy interpretations.
- D-011 defines the transition-reward and terminal-payoff convention.
- D-015 preserves joint reward and successor outcomes during policy closure.
- D-016 defines terminal-aware Bellman equations.
- D-017 requires overflow-safe floating normalization.
- D-022 authorizes the bounded Foundation Kickoff exception, selects the initial floating representation, and selects fail-fast construction errors.
- D-023 defines the reproducible GHCup and direnv development toolchain.
- D-024 defines the pinned two-compiler GitHub CI baseline.
- D-025 scopes warning failures and records lower-bound workarounds.
- D-026 separates exact reference values and objective domains.
- D-027 limits literal Kleisli laws to the exact kernel domain.
- D-012 and D-013 are superseded for the initial constructors. D-019 through D-021 remain proposed and block their named implementation phases.
- D-018 remains proposed. D-009 still defers categorical compiler work.

See `docs/DECISIONS.md` for rationale and consequences.

## Open questions

- What separate exact-reference value representation should accompany the floating runtime values?
- Should a later finite-distribution constructor combine equal support, and under which equality contract?
- Which policy representation best reports unavailable or duplicate action IDs?
- What public names should replace the legacy `Process`, `Action`, and `MDPF` names?
- Which test framework should the package adopt?
- Which additional GHC versions and lower dependency bounds pass CI beyond the verified GHC 9.8.4 development baseline?
- Does any initial POMDP use case justify a public belief-state type?
- Which continuous-kernel use case justifies a second package?

Do not resolve these questions only in code. Add or update a decision entry first.

## Instructions for future agents

1. Read this file, `TODO.md`, `docs/DECISIONS.md`, and the relevant architecture section.
2. Follow `docs/WORKFLOWS.md`.
3. Start P1.3, the task marked `NEXT` in `TODO.md`. Preserve every completed P0 through P1.2 gate.
4. Keep one writer for each branch and worktree.
5. Do not change legacy behavior without characterization tests and an accepted decision.
6. Do not claim a command passed without command output from the current revision.
7. Update all documents required by the change matrix.
8. Leave the worktree with a diff review and a clear evidence report.
