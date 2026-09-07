# Executable learning and bounded extensions: implementation checklist

## Objective

Implement all recommendations in [the durable review](../evidence/LEARNING-REVIEW-2026-09-05.md). The user explicitly requested this checklist and execution against it. Do not stop after planning or documentation alone.

Baseline: `7edc04a2d9a0de6af6fed1ccccb43303442210f5`.
Branch: `frontier/executable-learning`.

Status: **Active**. EL-01 documentation, capability checks, and proposal contracts are implemented; independent contract review is still required before EL-03 through EL-05. Every completion needs artifact paths and validation evidence in the execution log below. Keep incomplete items unchecked. A blocked item needs a concrete reason and next action. A passing focused test does not complete final integration.

## Invariants and scope

- Preserve released tag `v2026.9.3.0`, its target, and historical release evidence. No publishing or tag changes.
- Keep all 16 package versions at the current development baseline unless the user separately authorizes release preparation.
- Keep D-077 through D-085 Proposed. New contracts remain proposal-stage implementation evidence, not implicit acceptance.
- Freeze signatures, topology, ownership, limits, and failure precedence before each new semantic implementation.
- Prefer existing package ownership and no new package edges. Paired reports belong in `markovian-continuous`. Exact feedback sensitivity and finite fixed-policy aggregation belong in the exact root if their contracts fit that boundary. Escalate any need for a new package or edge.
- Retain exact Rational versus floating separation, checked canonical layouts, opaque nominal witnesses, bounded traversals, cumulative accounting, and atomic failure.
- Preserve reward/successor correlation and explicit terminal, reward, horizon, observation, and event timing.
- Make no new research-novelty, general optimization, general autodiff, continuous-policy, GPU, or production-training claim.
- Use one writer per checkout. Reviewers are read-only. No nested delegation or external publication by implementation agents.
- Preserve unrelated edits. Never terminate tmux or the parent session.

## EL-00 — Durable handoff

- [x] Record baseline, all findings, prior art, confidence, feature boundaries, and deferred universal claims.
- [x] Create this checklist before implementation.
- [x] Link this checklist from root TODO and include both durable files in source distributions via `Markovian.cabal` extra-doc-files.

## EL-01 — Documentation truth and feature contracts

- [x] Resolve F01-F08 in the review, including the autodiff readiness wording and Kleisli composition order.
- [x] Audit nearby current-guidance statements for the same contradictions. Preserve properly labeled historical evidence.
- [x] Add checked capability records separating availability, decision status, and evidence scope.
- [x] Generate capability presentation and validate package/module references. Do not equate current exposed-module snapshots with released snapshots.
- [x] Add negative tests for stale capability output, invalid status combinations/references, and current-versus-release confusion.
- [x] Freeze a bounded paired-difference API, ledger, rational limits, and failure precedence before implementation.
- [x] Freeze a narrow exact feedback JVP API, admitted directions, nominal layout/ownership, ledger, and failure precedence before implementation.
- [x] Freeze supplied-partition fixed-policy aggregation, observations/terminal/payoff rules, witness/quotient types, ledger, and failure precedence before implementation.
- [ ] Obtain independent contract review and resolve concrete blockers before implementing EL-03 through EL-05.

## EL-02 — Executable teaching and learning routes

- [ ] Classify every Haskell fence as runnable, compiled-source fragment, or explicit pseudocode. Explain the convention.
- [ ] Add a manifest/checker that rejects unclassified, missing, stale, or invalid references. Test the checker with deliberate failures.
- [ ] Compile and execute runnable examples through existing components where practical. Fragments need compiled source context. Do not disguise runnable examples as pseudocode to bypass coverage.
- [ ] Generate stable displayed outputs from executed fixtures and reject stale outputs. Wire validation into source CI and source archives.
- [ ] Add three reading routes: MDP practice, probability/composition, differentiation/backends.
- [ ] Add prerequisites, learning outcomes, and next steps for the routes and new lessons.
- [ ] Add an orientation/notation guide: kernel and matrix orientation, composition, timing, exact/approximate/layout equality, and distinct reverse operations.
- [ ] Add a law-laboratory chapter with precise hypotheses, commands, checked results, hints, exercises, and checked solutions.
- [ ] Lab A: shared fair draw versus independent executions, plus deterministic copy-law contrast.
- [ ] Lab B: matrix transpose versus prior-indexed Bayesian inversion and zero-evidence boundary.
- [ ] Lab C: joint reward/successor law versus correlation loss. Explain which observables distinguish them.
- [ ] Lab D: terminal payoff versus horizon truncation and event timing.
- [ ] Lab E: exact equality versus floating reassociation, with a frozen operation order.
- [ ] Lab F: logical tensor coordinates versus storage identity. Distinguish primitive-on-view from view-to-base derivatives.
- [ ] Add a bounded resource-admission experiment with EL-06, reusing the checked-bind distinction rather than claiming a Monad instance.
- [ ] Review executable examples for circular oracles and claims that exceed their fixtures.

## EL-03 — Exact paired-difference report

Depends on the reviewed EL-01 contract.

- [ ] Implement the report in the existing continuous exact package with no numerical/runtime dependency.
- [ ] Return exact means, variances, covariance, expected difference, difference variance, independent-marginal variance baseline, and a signed comparison.
- [ ] Use one operation-wide preflight and cumulative ledger for all moments and derived rational arithmetic. Return no partial report on failure.
- [ ] Test positive, zero, and negative covariance, shared and independent owners, zero variance, signed coefficients, and reordered layouts where applicable.
- [ ] Check all reported identities against an independent exact polynomial/moment oracle.
- [ ] Test exact and one-below limits, rational growth, saturation, and deterministic failure precedence.
- [ ] Add the U versus U and U versus 1-U lessons. Check exact values 0, -1/12, 1/3, and 1/6 in their stated roles.
- [ ] Update public snapshots, changelog, book, capability record, and source-distribution membership without changing immutable published history.

## EL-04 — Exact feedback sensitivity

Depends on the reviewed EL-01 contract.

- [ ] Implement the narrow directional JVP, not general autodiff or a full Jacobian. Fixed-topology reward directions are an admissible first slice if named explicitly.
- [ ] Evaluate derivatives of real parameterized semantics at rational points. Retain strict discount and lawful direction requirements.
- [ ] Check differentiated fixed-point equations literally. Meter all arithmetic and solves with one ledger and atomic failure.
- [ ] Add an independent derivative oracle, such as direct dual-number finite unrolling or symbolic small fixtures. Do not reuse production derivative code.
- [ ] Teach retry value V=4/3 and dV/dp=8/9 at gamma=p=1/2. If probability directions are outside the first API, label that symbolic exercise separately from API coverage.
- [ ] Compare finite-unrolling derivatives with fixed-point sensitivity without claiming finite equality in general.
- [ ] Test zero and boundary cases within the declared domain, layout/role rejection, exact and one-below budgets, rational growth, and failure precedence.
- [ ] Update public snapshots, changelog, book, capability record, compile-fail gates where needed, and source archives.

## EL-05 — Checked supplied-partition state aggregation

Depends on the reviewed EL-01 contract.

- [ ] Implement a checker for a supplied finite partition of a fixed-policy model. Do not implement automatic partition search.
- [ ] Preserve terminal behavior, applicable terminal payoffs, declared observation labels, and the joint immediate-reward/successor-block distribution.
- [ ] Return an opaque checked quotient or a concrete distinguishing witness. Bound witness construction and validation.
- [ ] Preserve original finite layouts and explicit quotient mapping. Reject incomplete/duplicate/unknown partition entries.
- [ ] Compare original and lifted quotient finite-horizon evaluations and admitted reward/observation trace distributions with independent small fixtures.
- [ ] Add equal-expected-reward but different reward-law rejection, terminal mismatch, observation mismatch, and reward/successor correlation counterexamples.
- [ ] Test exact and one-below state/block/support/work/rational limits and atomic failure. Test ownership/opacity at compile time where needed.
- [ ] Add a runnable aggregation lesson with a successful quotient and a failed partition witness.
- [ ] Update current public snapshots, changelog, book, capability record, and source archives.

## EL-06 — Resource-aware optimization contract

This item resolves D-085's contract. It does not implement the full cache proposal.

- [ ] Distinguish denotation, admission, error precedence, and execution cost in an explicit contract.
- [ ] Choose and document how source semantic work differs from interpreter execution work, if both are reported.
- [ ] Resolve the 100-unit source versus 1-unit cache-hit example under a 10-unit budget. Do not promise identical failures while silently changing admission policy.
- [ ] Explain cache construction, hits, semantic-limit validation, and callback purity/failure assumptions. Keep arbitrary closure identity excluded.
- [ ] Add a checked worked experiment illustrating the selected relation and its boundary.
- [ ] Amend D-085's proposal consistently and retain its Proposed status.

## EL-07 — Integration and independent evidence

- [ ] Run focused tests and checker negative tests for each completed phase. Record actual outcomes, not planned commands.
- [ ] Run warning-enabled full build and all applicable test suites. Report current suite counts from actual output.
- [ ] Run Fourmolu, HLint, cabal-fmt, relevant Python/shell checks, package manifests, release metadata/policy, and new gates.
- [ ] Run relevant compile-fail boundaries using exact current inplace package units.
- [ ] Run book, local-link, MathJax semantic-rendering, capability, snippet, and generated-output checks.
- [ ] Check new public Haddock signatures for hidden implementation types. Use the repository's authoritative warning-enabled documentation procedure.
- [ ] Check source archives include all new code, fixtures, manifests, scripts, generated learning artifacts, and durable records. Validate archive-only examples where practical.
- [ ] Obtain independent correctness and teaching reviews. Fix concrete blockers and re-run affected gates.
- [ ] Audit each checkbox against concrete evidence. Keep unresolved work unchecked and document residual risks.
- [ ] Produce a final summary and clean intended commits. External PR, merge, or publication remains parent-controlled and separately reported.

## Execution log

### 2026-09-05 — Baseline capture

- Recorded the user's full request and reviewed findings before implementation.
- Confirmed clean baseline and created `frontier/executable-learning`.
- All three post-merge runs for PR #5 passed. No runtime feature evidence is inferred from those older checks.
- Next action: complete EL-00 linking, then EL-01 documentation truth and frozen contracts.

### 2026-09-05 — EL-00 verification and EL-01 implementation (independent review pending)

- Corrected F01–F08 in `README.md`, the API map, introduction, tensor, autodiff and kernel chapters. Nearby current guidance repairs include `MIGRATION.md`, `docs/CONTEXT.md`, `release/README.md`, and the feedback/evidence chapters. Historical release evidence and D-037/D-061 history were not rewritten.
- Added nine checked records in `docs/capabilities/current.json`, digest-pinned selected immutable module-membership evidence in `docs/capabilities/released-modules.json`, and generated `docs/book/src/capabilities.md`. `scripts/check-capabilities` checks current Cabal/snapshot references separately from immutable membership; `--verify-release` additionally checks the released Git source and tag target. These records are selected capability claims, not a complete signature inventory or automatic semantic proof.
- Added `scripts/test_capabilities.py` (20 tests, including stale output, reference/status errors, current-versus-release substitution and no-Git archive-layout validation). Wired ordinary capability checking into the book gate and CI metadata step, with negative tests in CI. Root source membership includes the checker, tests, records, presentation, and all new contracts.
- Froze full future declarations and bounded algorithms in [EL-03 paired difference](EL-03-PAIRED-DIFFERENCE.md), [EL-04 event-reward JVP](EL-04-REWARD-JVP.md), and [EL-05 aggregation](EL-05-AGGREGATION.md). No Haskell semantic implementation or public export changed. The JVP holds probabilities and discount fixed; dV/dp=8/9 is explicitly a separate symbolic exercise. Aggregation owns a checked quotient table instead of bypassing compiled-model opacity.
- Selected source-semantic admission with separate executor cost for D-085 in [the EL-06 policy](EL-06-RESOURCE-ADMISSION.md), amended the Proposed decision, and resolved the 100/1/10 example. EL-06's checked experiment and concrete cache API remain unimplemented; its checkboxes remain open.
- Commands/results: `python3 scripts/check-capabilities --write --verify-release` generated nine rows and verified immutable membership; `python3 scripts/test_capabilities.py` passed 20 tests; `python3 scripts/check-capabilities --verify-release` passed. Initial checker validation incorrectly required Python's case-sensitive order for current snapshots; corrected to compare sorted contents without modifying snapshots, then reran successfully.
- Commands/results: `bash scripts/check-package-manifest`, `bash scripts/check-release-metadata`, `bash scripts/check-release-policy`, and `python3 scripts/test_release_tool.py` passed: 16 packages, 18 Haskell suites, 11 benchmarks, two policy tests and 32 release-tool tests. Those suite counts are manifest checks, not a new Haskell-suite execution claim.
- Commands/results: `bash scripts/check-book` passed with mdBook 0.5.4, local MathJax 3.2.2, 45 Markdown files and 419 checked display-math blocks. Generated HTML: `docs/book/build/index.html` (ignored build artifact).
- Commands/results: `cabal check`, `cabal-fmt --check Markovian.cabal`, `ruff check scripts/check-capabilities scripts/test_capabilities.py`, `shellcheck -x -P SCRIPTDIR scripts/check-book`, `python3 -m py_compile scripts/check-capabilities scripts/test_capabilities.py`, and `git diff --check` passed. Initial Ruff failures (import order, unused import, executable bits) were corrected. Initial ShellCheck invocations without the script-relative include path failed SC1091; the explicit `-P SCRIPTDIR` invocation passed. An initial read-only decision-heading inspection used a wrong numeric slice, was corrected, and made no edits.
- Commands/results: `cabal sdist all --project-file=cabal.project.ci --output-directory=dist-newstyle/el01-sdist` produced all 16 development source archives (not release candidates). A Python `tarfile` membership assertion checked 12 required EL-00/EL-01 root paths and the existing root TODO link. Root archive: `dist-newstyle/el01-sdist/Markovian-2026.9.3.0.tar.gz`. Refreshed all archives after the final source edits. A separate Python run reused `release_tool.validate_archive` and `extract_archive` for all 16 archives, assembled their original package layout in a temporary directory without `.git`, and passed the archive-only capability gate plus all 20 negative tests. No Haskell archive compilation is inferred. A later `cabal sdist Markovian` refresh was rejected as a component target (Cabal-7151); the supported `cabal sdist all` command was used instead.
- An explicit Python/Git invariant check confirmed all 16 versions unchanged from durable-plan commit `69382b2`, D-077–D-085 still Proposed, released tag object `d746952084e09647e7bcd67b92dd6cef9d0e14c9` and target `fe6abb8db9b3def65ead6602168eef860a79527c` unchanged. Metadata checks also confirmed no package/dependency-edge drift.
- Structured stage evidence destination: `/home/josephcox/.pi/agent/sessions/--home-josephcox-dev-Markovian--/subagent-artifacts/outputs/c4bb5762-3f07-456b-bae3-7564338015d5/stages/truth-contracts-write.json`.
- Remaining work: independent EL-01 contract/correctness review (no implementation stages independently reviewed yet), EL-02 executable lessons, EL-03–EL-05 semantic implementations after review, the EL-06 checked experiment, and EL-07 full integration. No full Haskell build/test/Haddock/hardware campaign was run for this documentation/Python-only stage. All package versions, dependency edges, released tag/history and D-077–D-085 statuses remain unchanged.

### 2026-09-07 — Handoff protocol recovery

- Workflow `c4bb5762-3f07-456b-bae3-7564338015d5` stopped after `truth-contracts-write`. The child committed `2d3140e` but omitted the runtime-required `structured_output` call. Writing a JSON artifact did not satisfy that protocol.
- Parent verified branch `frontier/executable-learning`, the committed implementation, and a clean working tree. No rollback or duplicate implementation is needed.
- Parent reran `python3 scripts/test_capabilities.py` (20 passing tests), `python3 scripts/check-capabilities --verify-release` (nine checked records), and `git diff --check` (passed).
- Resume at independent EL-01 contract review. Later semantic implementation remains gated by that review. Recovery uses the same native subagent protocol, ordinary writer handoffs, and explicit JSON reviewer verdicts checked by the orchestration code. Malformed verdicts fail closed.
- No decision is accepted by this recovery. EL-02 through EL-07 remain incomplete.

## Completion standard

This program is complete only when EL-00 through EL-07 have evidence for every applicable item. Do not replace implementation with plans. Do not mark a failed or unexecuted gate as passed. Scope changes require explicit rationale and supervisor review. Unsupported universal features remain excluded rather than becoming hidden obligations.
