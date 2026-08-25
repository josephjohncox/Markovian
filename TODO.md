# Work plan

This file is the authority for priority, status, dependencies, risks, and acceptance criteria.

## Status legend

- `[ ] PLANNED`: the item is queued behind normal priority gates.
- `[-] ACTIVE`: one assigned writer owns the item.
- `[!] BLOCKED`: the next eligible item lacks a required external dependency or accepted decision.
- `[x] DONE`: the acceptance criteria have current, linked evidence.

Do not mark an item `DONE` from static inspection when its criteria require commands.

## Priority rules

P0 is the highest priority. Complete each priority gate before work starts on the next priority.

A user can authorize an exception. Record the exception in `docs/DECISIONS.md` and list its risk here.

## Foundation Kickoff exception

D-022 records the user's authorization to implement one bounded, additive semantic-foundation slice while the P0 toolchain gate was blocked.

- [x] **FK.1 Add validated floating values.**
  - Added opaque `Prob`, `Weight`, `FiniteDist`, and `Reward` values with fail-fast structured errors.
  - Added scaled normalization, removal of zero and rounded-zero masses, and labeled duplicate preservation.
  - Source evidence: `src/Markovian/Probability.hs` and `src/Markovian/Reward.hs`.
  - GHC 9.8.4 builds the modules, and the related contract tests pass.
  - Completion evidence: FK.4 passed the final warning-free verification run.
- [x] **FK.2 Add one-layer semantic interfaces.**
  - Added `Kernel`, `Policy`, typed terminal state and transition outcomes, and one-step MRP and MDP interfaces.
  - Action IDs and transition outcomes have separate types and operations.
  - Terminal steps do not run transition kernels. Model construction does not unfold successor states.
  - Source evidence: `src/Markovian/Kernel.hs`, `src/Markovian/MRP.hs`, `src/Markovian/MDP.hs`, and `src/Markovian/Policy.hs`.
  - GHC 9.8.4 builds the modules, and the related contract tests pass.
  - Completion evidence: FK.4 passed the final warning-free verification run.
- [x] **FK.3 Add deterministic contract tests and package declarations.**
  - Added tests for value validation, terminal reward timing, one-layer self-loops, action-versus-transition separation, overflow-safe normalization, rounded-zero mass removal, and empty distribution and action support.
  - Exposed the new modules and added durable documents to `extra-doc-files` in `Markovian.cabal`.
  - Command evidence: the GHC 9.8.4 build and seven deterministic contract tests pass.
  - Cabal package, Haddock, format, and source-distribution commands pass.
  - Completion evidence: FK.4 passed the final warning-free verification run.
- [x] **FK.4 Verify the Foundation Kickoff slice.**
  - The pinned direnv toolchain and local project files reproduce the environment.
  - `cabal check` reports no package warnings.
  - The GHC 9.8.4 `-Werror` build and seven deterministic tests pass.
  - Haddock completes without warning lines.
  - Fourmolu, cabal-fmt, and source-distribution commands pass.

Remaining after FK.4: exact-reference probability and law tests; `Discount` and `Horizon`; duplicate-action validation; policy support validation and closure; bounded evaluators; legacy adapters; and all later TODO priorities.

Reviewer revision: normalization removes entries whose normalized mass rounds to zero. The passing extreme finite-weight regression checks that `outcomes` contains only positive masses.

Risk: the implementation has evidence from one compiler only. CI and additional compiler versions can expose compatibility defects.

## P0: Trustworthy baseline

Dependencies: a supported GHC and Cabal toolchain.

Risks: the current package might not parse or compile. Characterization tests can expose undocumented legacy behavior.

- [x] **P0.1 Add the local project and CI baseline.**
  - D-023 defines the pinned development environment. D-024 defines the two-compiler CI baseline.
  - `.github/workflows/ci.yml` pins each action by commit SHA and tests GHC 9.4.8 and 9.8.4 on Ubuntu 22.04.
  - Hosted evidence: the Format, Lower bounds, GHC 9.4.8, and GHC 9.8.4 jobs passed at <https://github.com/josephjohncox/Markovian/actions/runs/32540102997> for commit `28296970083358438c93a11dda51c59d89e90a68`.
- [x] **P0.2 Replace the placeholder with legacy characterization tests.**
  - Added terminal value, deterministic chain, exact expected value `12.5`, and sample-support characterization tests against the legacy API.
  - Added zero-episode and terminal-initial-state Q-table identity tests.
  - Eleven tests pass on GHC 9.4.8 and 9.8.4 with `--test-show-details=direct`.
- [x] **P0.3 Audit package metadata and direct dependencies.**
  - Corrected package metadata, distributed every durable file, and verified `cabal check` and `cabal sdist`.
  - Removed unused direct component dependencies and retained PVP bounds for every dependency.
  - D-025 scopes `-Werror` to project code and records the tested transitive lower-bound corrections.
  - Normal and `--prefer-oldest` builds and all eleven tests pass.
- [x] **P0.4 Add format, Haddock, and source-distribution gates.**
  - Pinned Fourmolu and cabal-fmt checks pass.
  - Haddock completes with source links and no warning lines.
  - The source tarball unpacks, builds, and passes all eleven tests.
  - CI now runs every P0.4 gate.

P0 gate: all four baseline items are `DONE`. D-022 permits the additive Foundation Kickoff files but does not waive any baseline command or evidence requirement.

## P1: Valid finite semantic core

Dependencies: P0 and accepted decisions D-001 through D-007, D-010, D-011, D-015, D-017, and D-022.

D-022 supersedes D-012 and D-013 for the initial floating, fail-fast constructors. The Foundation Kickoff exception covers only FK.1 through FK.4. The rest of P1 remains behind P0 and FK.4.

Risks: a premature public API can lock in weak names or the wrong numeric representation.

- [x] **P1.1 Complete validated probability and objective values.**
  - Floating `Prob`, `Weight`, `FiniteDist`, and `Reward` values remain opaque and use D-017 normalization.
  - Added separate rational exact probability, distribution, reward, and discount modules.
  - Added closed finite-horizon discounts, strict contraction discounts, and unbounded natural-number horizons.
  - Floating constructors reject invalid values and canonicalize negative zero.
  - D-026 proves the scaled-total invariant and explains why the defensive non-finite branch has no feasible public test input.
  - Exact weights `1` and `3` normalize literally to `1/4` and `3/4`; floating maximum weights normalize to `0.5` and `0.5`.
  - Fifteen deterministic tests pass under GHC 9.4.8, GHC 9.8.4, and the tested lower-bound plan.
  - All constructors remain hidden.
- [x] **P1.2 Verify and complete stochastic kernels and one-layer MRP and MDP interfaces.**
  - Action IDs remain separate from outcomes, one model step returns one layer, empty action support returns `EmptyActionSupport`, and the self-loop contract terminates.
  - Added exact finite-distribution bind and exact left-to-right kernel composition.
  - D-027 limits literal Kleisli claims to the exact reference domain.
  - Exact functor identity and composition plus kernel left identity, right identity, and associativity pass literally.
  - Seventeen deterministic tests pass under GHC 9.4.8, GHC 9.8.4, the lower-bound plan, and the unpacked source distribution.
  - Hosted evidence: all four CI jobs passed at <https://github.com/josephjohncox/Markovian/actions/runs/32784091295> for commit `4050d20d6bc84c1eb93b6c44ea1b5d365d33e44d`.
- [x] **P1.3 Add explicit policy closure.**
  - Added per-state floating closure with separate model, duplicate-policy, unavailable-policy, and normalization errors.
  - Added exact support validation and joint-outcome closure for literal reference laws.
  - Terminal closure returns its payoff without evaluating the policy.
  - Distinct rewards to one successor remain distinct, and exact closed traces match direct policy-and-MDP traces.
  - Conditional reward queries return `ZeroMassTransition` for absent successor mass.
  - D-028 records why arbitrary state types require a fallible per-state closure interface.
- [x] **P1.4 Keep the new core additive.**
  - No legacy export was removed or changed.
  - New policy closure exports have Haddock contracts and opaque representations.
  - All nineteen P0 and P1 test groups pass under both compilers, the lower-bound plan, and the unpacked source distribution.
  - Hosted evidence: all four CI jobs passed at <https://github.com/josephjohncox/Markovian/actions/runs/32905919708> for commit `2875b2664aba670e9178a6a7012455875e34e955`.

P1 gate: `DONE`. The semantic core is additive, validated, documented, and tested.

## P2: Bounded interpreters

Dependencies: P1.

Risks: reward timing, horizon boundaries, and discount placement can silently disagree.

- [x] **P2.1 Add exact finite-support expectation.**
  - Added separate exact MDP status, transition outcome, decision, and model-error types.
  - Added exact policy kernels and named finite objectives.
  - The evaluator receives an exact model, policy, horizon, and discount with no hidden defaults.
  - Terminal payoff, horizon zero, deterministic reward timing, weighted support, discount zero, structured errors, and bounded self-loops have exact tests.
  - D-029 records bounded state recursion and the exponential reference-interpreter cost.
  - Twenty-three test groups pass under both compilers, the lower-bound plan, and the unpacked source distribution.
- [ ] **P2.2 `NEXT` Add seeded finite-support sampling.**
  - Receive an explicit generator or seed.
  - Never select zero-mass outcomes.
  - Acceptance: equal seeds produce equal traces and returns.
  - Acceptance: exact support tests replace statistical frequency gates.
- [ ] **P2.3 Add structured traces and errors.**
  - Make action IDs, outcomes, rewards, and stop reasons visible in trace values.
  - Acceptance: no interpreter uses partial vector indexing or partial maximum functions.
  - Acceptance: model, policy, and objective errors have separate constructors.

P2 gate: exact and sampling interpreters implement the same documented objective.

## P3: Migration and package cleanup

Dependencies: P2 and accepted D-010.

Risks: the legacy `Action` has two incompatible meanings. One generic adapter would hide data loss.

- [ ] **P3.1 Add explicit legacy adapters.**
  - Add `fromLegacyMarkovProcess` for branch weights as transition probabilities.
  - Add `fromLegacyDeterministicMDP` for each legacy branch as a deterministic action.
  - Do not add a generic `fromLegacy` function.
  - Acceptance: the evaluation adapter preserves expected value `12.5`.
  - Acceptance: the deterministic adapter creates two action IDs for the first sample state.
- [ ] **P3.2 Migrate the sampling application.**
  - Migrate `app/Sample/Main.hs` to explicit policy and objective inputs.
  - Acceptance: the sampling application compile fixture passes.
  - Dependency: P3.1.
- [ ] **P3.3 Define compatibility and release policy.**
  - Move old definitions to `Markovian.Legacy` during the 0.2 series.
  - Keep deprecated shims for at least 90 days after a verified 0.2 release.
  - Remove shims only in a PVP-major 0.3 release.
  - Acceptance: a migration guide maps each old export to its replacement.
- [ ] **P3.4 Replace generated package documents.**
  - Replace `CHANGELOG.md` placeholders only when release evidence exists.
  - Add installation, guarantees, limitations, and a compiled example to `README.md`.
  - Acceptance: README examples compile in `test/compile`.
  - Acceptance: the source distribution includes all durable documents.

P3 gate: users have tested adapters, deprecations, and a compiled sampling migration path.

## P4: Learning interpreters

Dependencies: P3, the MDP action identity contract from P1, and accepted D-019.

D-019 is still `Proposed`. P4 cannot start until reviewers accept it or accept a superseding decision.

Risks: statistical tests can be flaky. Hidden schedules can invalidate convergence claims.

- [ ] **P4.1 Add validated Q-learning configuration.**
  - Make learning rate, discount, exploration, episode count, and maximum episode steps explicit.
  - Use natural numbers for counts.
  - Acceptance: invalid rates and discounts return structured errors.
- [ ] **P4.2 Implement and test one pure Q update.**
  - Include the transition reward and terminal successor payoff in the target.
  - Key the table by stable state identity and action ID.
  - Acceptance: one update matches a hand-calculated numeric result.
  - Acceptance: zero episodes preserve the table.
  - Acceptance: a self-loop stops at the configured step limit.
- [ ] **P4.3 Add a seeded training interpreter.**
  - Sample the transition outcome kernel after the learner selects an action ID.
  - Acceptance: equal seeds produce equal Q-tables.
  - Acceptance: a scripted two-action model prefers the higher return.
  - Do not require statistical convergence in CI.
- [ ] **P4.4 Migrate the Q-learning application.**
  - Migrate `app/QLearning/Main.hs` to the validated learner and explicit configuration.
  - Acceptance: the Q-learning application compile fixture passes.
  - Acceptance: the application has no import of the legacy `QLearning` module.
  - Dependency: P4.1 through P4.3.

P4 gate: one coherent Q-learning algorithm replaces both experimental paths, and the learning application uses it.

## P5: Cyclic, POMDP, and continuous models

Dependencies: P4 and stable P1 and P2 semantics.

P5.1 depends on accepted D-016. P5.2 depends on accepted D-020. P5.3 depends on accepted D-021.

Risks: convergence assumptions and zero-evidence behavior can make APIs partial.

- [ ] **P5.1 Add discounted Bellman fixed-point solvers.**
  - Dependency: accepted D-016.
  - Require bounded rewards and a discount below one.
  - Report residual, tolerance, iteration count, and stop reason.
  - Acceptance: tests compare finite models with an exact linear reference.
  - Acceptance: a nonterminal transition into a nonzero terminal payoff matches the terminal-aware linear equation.
  - Acceptance: the result includes a documented residual-based error bound.
- [ ] **P5.2 Add finite POMDP interfaces.**
  - Dependency: accepted D-020.
  - Add latent state, observation, observation kernel, and belief update types.
  - Define impossible-observation behavior as a structured zero-evidence error.
  - Acceptance: normalization and zero-evidence tests pass.
- [ ] **P5.3 Add a continuous-kernel experiment outside the finite core.**
  - Dependency: accepted D-021.
  - Record measurability and integrability preconditions.
  - Use sampling or quadrature through an interpreter.
  - Acceptance: an accepted use-case decision names supported operations and error semantics.
- [ ] **P5.4 Evaluate other MDP variants.**
  - Assess average-reward, constrained, semi-Markov, and multi-agent variants separately.
  - Acceptance: each admitted variant has an objective, laws, and interpreter plan.

P5 gate: every unbounded or conditioned operation has explicit mathematical preconditions and errors.

## P6: Compiler, GPU, neural, and research backends

Dependencies: P5, stable core semantics, and a measured use case for each backend.

D-009 remains `Deferred`. Compiler or optimizer work requires the accepted superseding decisions named below.

Risks: performance abstractions can change stochastic sharing or floating results.

- [ ] **P6.1 Define the categorical compiler IR.**
  - Dependency: D-018 must be `Accepted` and supersede D-009 for compiler IR work.
  - Represent identity, composition, tensor, copy, discard, and explicit sample bindings.
  - Acceptance: law tests preserve structure and distinguish one shared draw from two draws.
- [ ] **P6.2 Admit recursion, Kan, Codensity, Cayley, or NBE only through their proof gates.**
  - Dependency: each feature needs an accepted feature-specific decision that supersedes D-009 for that feature.
  - Acceptance: each feature has the proof obligation listed in `docs/DECISIONS.md`.
  - Acceptance: each optimization has equivalence tests and a representative benchmark.
- [ ] **P6.3 Evaluate tensor and GPU backends outside the core.**
  - Dependency: D-007 and D-008, plus an accepted backend-specific admission decision.
  - Evaluate Hasktorch first for neural GPU work.
  - Evaluate Accelerate for batched finite array work.
  - Evaluate horde-ad only as a research autodiff backend.
  - Acceptance: CPU and backend differential tests meet a written numerical-error policy.
  - Acceptance: benchmarks include data transfer and compilation costs.
- [ ] **P6.4 Add neural model denotations before training APIs.**
  - Dependency: an accepted neural-backend admission decision.
  - Treat a deterministic network as a Dirac kernel.
  - Validate stochastic network logits and support.
  - Acceptance: the approximation relation, calibration metric, and gradient-estimator assumptions are documented and tested.
- [ ] **P6.5 Consider `monad-bayes` only as an optional sampling interpreter.**
  - Dependency: an accepted sampling-adapter admission decision.
  - Acceptance: the core dependency graph stays free of `monad-bayes`.
  - Acceptance: interpreter tests define seed and distributional reproducibility contracts.

P6 gate: no backend changes the public denotation or enters the semantic core.

## Repository file coverage

This table includes every file tracked at the start of the documentation phase.

| File | Required work | Priority |
| --- | --- | --- |
| `.gitignore` | Review entries after CI and formatter tools create artifacts. Keep local project files ignored. | P0 |
| `CHANGELOG.md` | Replace the generated release placeholder only with verified release facts. | P3 |
| `LICENSE` | Review the copyright year and owner before a release. Change only with owner approval. | P3 |
| `Markovian.cabal` | Fix metadata, component dependencies, bounds, exposed modules, and distributed documents. | P0-P3 |
| `README.md` | Keep the maturity warning current. Add only compiled examples and verified installation steps. | P0-P3 |
| `app/QLearning/Main.hs` | Migrate to the validated learner and explicit configuration. | P4 |
| `app/Sample/Main.hs` | Migrate to policy, objective, and seeded sampling inputs. | P3 |
| `src/Markovian.hs` | Characterize, deprecate, and move legacy semantics. Add the new core in separate modules. | P0-P3 |
| `src/QLearning.hs` | Characterize and replace the two inconsistent learning paths. | P0-P4 |
| `test/Main.hs` | Extend the deterministic core contracts with legacy characterization tests. | P0 |

Maintain `TODO.md`, `docs/CONTEXT.md`, `docs/ARCHITECTURE.md`, `docs/WORKFLOWS.md`, and `docs/DECISIONS.md` with every triggered change.

## Next-task marker

**NEXT BLOCKER RESOLUTION: provide a supported GHC and Cabal toolchain or an authorized CI execution environment. Complete the P0.1 project bootstrap, use its commands to verify FK.4, and fix only bounded-slice defects. Do not start another semantic slice.**
