# Executable learning and bounded extensions: implementation checklist

## Objective

Implement all recommendations in [the durable review](../evidence/LEARNING-REVIEW-2026-09-05.md). The user explicitly requested this checklist and execution against it. Do not stop after planning or documentation alone.

Baseline: `7edc04a2d9a0de6af6fed1ccccb43303442210f5`.
Branch: `frontier/executable-learning`.

Status: **Active**. EL-01 documentation, capability checks, and proposal contracts passed independent contract review. EL-02 teaching is implemented except the explicitly later EL-06 resource experiment; independent teaching review and subsequent semantic phases remain outstanding. Every completion needs artifact paths and validation evidence in the execution log below. Keep incomplete items unchecked. A blocked item needs a concrete reason and next action. A passing focused test does not complete final integration.

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
- [x] Obtain independent contract review and resolve concrete blockers before implementing EL-03 through EL-05.

## EL-02 — Executable teaching and learning routes

- [x] Classify every Haskell fence as runnable, compiled-source fragment, or explicit pseudocode. Explain the convention.
- [x] Add a manifest/checker that rejects unclassified, missing, stale, or invalid references. Test the checker with deliberate failures.
- [x] Compile and execute runnable examples through existing components where practical. Fragments need compiled source context. Do not disguise runnable examples as pseudocode to bypass coverage.
- [x] Generate stable displayed outputs from executed fixtures and reject stale outputs. Wire validation into source CI and source archives.
- [x] Add three reading routes: MDP practice, probability/composition, differentiation/backends.
- [x] Add prerequisites, learning outcomes, and next steps for the routes and new lessons.
- [x] Add an orientation/notation guide: kernel and matrix orientation, composition, timing, exact/approximate/layout equality, and distinct reverse operations.
- [x] Add a law-laboratory chapter with precise hypotheses, commands, checked results, hints, exercises, and checked solutions.
- [x] Lab A: shared fair draw versus independent executions, plus deterministic copy-law contrast.
- [x] Lab B: matrix transpose versus prior-indexed Bayesian inversion and zero-evidence boundary.
- [x] Lab C: joint reward/successor law versus correlation loss. Explain which observables distinguish them.
- [x] Lab D: terminal payoff versus horizon truncation and event timing.
- [x] Lab E: exact equality versus floating reassociation, with a frozen operation order.
- [x] Lab F: logical tensor coordinates versus storage identity. Distinguish primitive-on-view from view-to-base derivatives.
- [ ] Add a bounded resource-admission experiment with EL-06, reusing the checked-bind distinction rather than claiming a Monad instance.
- [x] Review executable examples for circular oracles and claims that exceed their fixtures.

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

### 2026-09-07 — EL-01 independent review recorded; EL-02 executable teaching

- Received the prior read-only independent contract review, with no findings. Its exact JSON evidence is recorded below. The reviewer did not independently rerun commands; the recovery entry records the parent's 20-test and release-verification rerun. This closes only the EL-01 review gate, not later feature implementation or teaching review.

```json
{"verdict":"passed","summary":"No issues found. Reviewed F01–F08, nearby guidance, capability records/checker/generated output, all 20 test cases, immutable-membership and archive handling, and all three semantic contracts plus D085 admission policy against existing source. Mathematical conditions, ownership, timing, cumulative budgets, atomic failure, and independent-oracle requirements are sufficient for subsequent implementation. Later phases correctly remain incomplete. This was a read-only source/contract review; commands were not independently rerun. Confidence: high for the reviewed contracts.","findings":[]}
```

- Added `docs/learning/fences.json` and `scripts/check-learning`: 73 Haskell fences including the decisions Markdown include; three complete runnable contexts, 67 compiled fragments, and three explicitly reasoned, digest-pinned conceptual/historical sketches. The checker checks actual Cabal component/module/source-directory membership, source excerpts, includes/ranges, full runnable contexts and output receipts. Whitespace is normalized for fragment matching; compilation is of the complete registered source, not of a stand-alone excerpt. Static checks deliberately do not claim execution.
- Added five `*Contexts.hs` modules to existing test components (root context under `docs/learning`, others in dense-exact, learning, neural and GPU). Preserved the real calls rather than reclassifying them as pseudocode. Compiling exposed real error-type mismatches in the prior Bayesian push/evidence, dense lowering/run and neural forward/optimizer snippets; explicit checked IO conversion now joins those failures. The GPU preparation and execution snippets have separate compiled contexts, without a hardware-execution claim. Teaching wrappers infer constraints and retain named intermediate bindings; only missing-signature warnings and the specific HLint simplifications that would erase those fragments are suppressed.
- Added `test/LawLaboratory.hs` (A–D) and `packages/markovian-tensor/test/TensorLawLaboratory.hs` (E–F). Both run in their normal suites and via `--learning`; no package, component, public export or dependency edge was added. A checks actual shared/independent stochastic matrices and a deterministic witness. B checks transpose, supported inversion, posterior masses and `ZeroEvidence`. C checks a hand-enumerated cross moment, equal additive MDP values and distinct trace supports. D checks terminal-before-horizon values, stop reasons and realized transition counts. E executes separate F64 tensor additions and compares Rational arithmetic. F checks storage, logical order, a real multiply tape, explicit transpose pullback and independent six-coordinate quadratic differences. No automatic view-to-base tape is claimed.
- Added the three learner routes, orientation guide and law laboratory; linked them in navigation/introduction and first-MDP next steps. Each new route/lesson states prerequisites, outcomes and next steps. Exercises have hints and answers checked by the fixtures. The laboratory identifies independent reference tests and explains why materialize-first equivalence alone is not an independent oracle. Self-review found no production-oracle circularity; independent teaching review is still required.
- Generated three displayed stdout files and `docs/learning/output-receipt.json` by actual execution, including the first MDP's output. Receipts bind implementation/context sources, Cabal files, manifest/checker and output bytes. Wired the static gate into `check-book`, execution and 30 deliberate-failure tests into source CI, and archive-only checking into the source-distribution CI step. `scripts/check-learning-archive` validates all 16 archives, reconstructs the original package layout without Git, invokes archived tools, builds contexts, executes examples and runs checker tests. Cabal automatically includes the new test modules; root extra-doc/source membership includes manifest, receipt, outputs and scripts.
- Commands/results: a clean `cabal build all --project-file=cabal.project.ci` passed on GHC 9.8.4 with the project's warning-as-error configuration. `cabal test all --project-file=cabal.project.ci --test-show-details=direct` passed all 18 suites (276 `PASS:`-named checks, plus suites with their own output conventions and the new asserted laboratory outputs). These counts are actual execution, not manifest counts. Earlier default-cache builds failed with stale optimized/nonoptimized inplace interface/linker references; forcing recompilation did not repair the shared cache. Moving the ignored `dist-newstyle` cache to `/tmp/markovian-el02-old-dist-*` and rebuilding cleanly fixed the issue; no source workaround was used. Both affected context files subsequently received clean Haskell diagnostics. An isolated seven-component teaching build had already passed independently of that stale cache.
- Commands/results: `python3 scripts/check-learning --write` executed and generated the outputs; `python3 scripts/test_learning.py` passed 30 tests. The negatives cover missing/unclassified/stale fences, bad classes, unknown/escaping/missing sources, wrong package/component/module membership, includes/ranges, stale pseudocode, output and implementation receipts, missing execution and duplicate outputs. Three real temporary Cabal/GHC cases test rejected ill-typed context, runtime fixture failure and stale executed answer (followed by successful regeneration). Those deliberate compiler/runtime failures are expected passing test evidence, not hidden gate failures.
- Commands/results: `bash scripts/check-book` passed: mdBook 0.5.4, local MathJax 3.2.2, 48 Markdown files, 419 source-to-generated display-math blocks, local links/includes and static learning/capability checks. No new equations increased the previous math-block count. An additional direct Node attempt to load the browser MathJax bundle failed because `xmldom-sre` is not installed; no browser/MathJax-startup execution is claimed by this stage. The repository's existing HTML/MathJax semantic gate passed; full browser rendering remains an EL-07 integration gate.
- Commands/results: package manifest, release metadata and release policy checks passed with all 16 packages, 18 suites and 11 benchmarks unchanged; the 20 capability tests and `check-capabilities --verify-release` passed. `cabal check`, focused Fourmolu checks and HLint passed; executable-bit Ruff errors were fixed and the new Python files passed Ruff. `git diff --check` passed. No benchmark execution, Haddock campaign, GHC 9.4.8, CUDA execution or hosted CI run is claimed here.
- Commands/results: `cabal sdist all --project-file=cabal.project.ci --output-directory=dist-newstyle/el02-sdist` created 16 development archives. `python3 scripts/check-learning-archive dist-newstyle/el02-sdist` passed archive-only context compilation, all three executions/output comparisons and all 30 checker tests. Root archive is `dist-newstyle/el02-sdist/Markovian-2026.9.3.0.tar.gz`. Archives are refreshed after final documentation/evidence edits; immutable release archives/history are untouched.
- Remaining work: independent EL-02 correctness/teaching review; EL-03–EL-05 implementations against the reviewed contracts; EL-06 checked resource experiment; final EL-07 integration including warning-enabled public Haddock and actual browser rendering. The resource-experiment checkbox remains deliberately open. All versions and D-077–D-085 Proposed statuses remain unchanged. No push, PR, publication, merge, tag operation or nested delegation occurred.

### 2026-09-07 — EL-02 timeout recovery

- Workflow `4241ef7e-c057-4d85-b655-4069bbcd1247` passed independent EL-01 review, then its teaching writer `753db200-75a3-48e5-a0b7-2fd0a4085de6` reached the default 1,800-second deadline. This timeout is not a completed EL-02 review or integration gate.
- Parent inspected the partial tracked/untracked changes, checker, output receipt, execution log, and prior run status. No prior worker or Cabal/GHC build process remained active at inspection.
- Preserved the tracked binary diff and untracked files in `/tmp/markovian-el02-recovery-ot5Tcv/tracked.patch` and `/tmp/markovian-el02-recovery-ot5Tcv/untracked.tar.gz`. These are temporary recovery copies, not release artifacts.
- Parent independently reran `python3 scripts/check-learning` (73 statically classified fences and current output receipt) and `python3 scripts/test_learning.py` (30 passing tests, including deliberate compilation/runtime failures). Static checking is not a claim that the parent recompiled all real teaching contexts.
- Resume the partial implementation rather than overwrite it. Finish EL-02 validation and handoff, then require an independent teaching review. Use explicit per-stage deadlines instead of the accidental 30-minute default. Do not spend the delivery window rebuilding valid caches solely to silence inconclusive editor diagnostics.
- EL-03 through EL-07 remain incomplete. Later work continues only after the relevant stage review passes.

### 2026-09-07 — Native runner dependency recovery

- Workflow `6f0884e5-5616-4461-a8a6-b64193900aad` failed before its resumed child `d6cabe6e-dc71-4f93-a107-a98fa2f81340` became ready. No child session was persisted; the runtime observed process exit code 1. No implementation completion is inferred.
- The ten-second startup timeout masked `MODULE_NOT_FOUND` for `@earendil-works/pi-tui`, imported by the installed `pi-subagents` watchdog. Evidence: `/tmp/pi-subagents-uid-1000/async-subagent-runs/d6cabe6e-dc71-4f93-a107-a98fa2f81340/runner.stderr.log` and `process-terminal.json`.
- Parent verified the peer was missing from runner resolution but present in the installed Pi runtime. Added only `/home/josephcox/.pi/agent/npm/node_modules/pi-subagents/node_modules/@earendil-works/pi-tui`, a symlink to `/home/josephcox/.nvm/versions/node/v26.1.0/lib/node_modules/@earendil-works/pi-coding-agent/node_modules/@earendil-works/pi-tui`. No download, package manifest/settings change, or Markovian dependency change was made.
- A Node resolution/import check from the runner's package root now resolves that existing package and loads its `Text` export. This checks the missing dependency, not an entire workflow. Retry the native protocol with a fresh writer context and preserved working tree; independent review remains mandatory.

### 2026-09-07 — EL-02 resumed implementation and validation

- Inspected every tracked diff and untracked teaching artifact, the full durable review/checklist, and the recovered execution logs. Preserved the timed-out implementation rather than recreating it. `/tmp/el02-clean-build.log`, `/tmp/el02-all-tests.log`, `/tmp/el02-learning-write.log`, and `/tmp/el02-archive.log` substantiate the earlier build, 18-suite run (276 `PASS:` labels), teaching execution and archive-only compilation claims. `/tmp/el02-hlint.log` contains an earlier 45-hint failure, not passing evidence; the fresh HLint run below supplies the passing result.
- Strengthened two existing fixture assertions: laboratory B now checks both complete supported Bayesian-inverse rows `[[2/5,3/5],[0,1]]`, rather than only their count; laboratory E directly checks the rounded `b+c` intermediate used by its exercise answer. Regenerated outputs by execution and updated the matching lesson text. Added all three learning Python scripts to CI's explicit syntax-check list. No runnable fence was reclassified, and all six experiments still exercise real public APIs with literal independent expectations or base-coordinate differences.
- Fresh compiler evidence: `cabal build all --project-file=cabal.project.ci` and `cabal test all --project-file=cabal.project.ci --test-show-details=direct` passed after the final fixture edits, with 18 passing suites and 276 `PASS:` labels. Logs: `/tmp/el02-resume-build.log`, `/tmp/el02-resume-tests.log`. Used the existing cache; no cache rebuild or source workaround was needed. Editor analysis of the two edited Haskell files was unavailable, not clean; actual GHC 9.8.4 warning-as-error compilation supplies validation.
- Fresh teaching evidence: `python3 scripts/check-learning --write`, then `--run`, passed all context compilation and three complete output comparisons; 73 fences remain classified (three runnable, 67 fragments, three explicit pseudocode sketches). `python3 scripts/test_learning.py` passed all 30 tests, including the expected real compiler/runtime rejection fixtures. Logs: `/tmp/el02-resume-learning-write.log`, `/tmp/el02-resume-learning-run.log`, `/tmp/el02-resume-checker-tests.log`. `docs/learning/output-receipt.json` and the three displayed output files are current executed artifacts, not hand-edited answers.
- Fresh presentation/format evidence: `bash scripts/check-book` passed 48 book files, local links/includes, static capability/learning checks, and 419 source-to-generated MathJax display blocks (`/tmp/el02-resume-book.log`). Full repository HLint, including `docs/learning`, passed with no hints (`/tmp/el02-resume-hlint.log`); Fourmolu checked every `.hs` under the existing CI source roots plus `docs/learning`. Changed Cabal files passed `cabal-fmt --check`; the three learning Python scripts passed Ruff and `py_compile`; `scripts/check-book` passed `bash -n` and ShellCheck with `-x -P SCRIPTDIR`; `cabal check` and `git diff --check` passed.
- Fresh metadata evidence: package-manifest, release-metadata and release-policy checks passed (16 packages, 18 suites, 11 benchmarks, unchanged dependency edges; two policy tests). Capability release verification and all 20 capability tests passed; all 32 release-tool tests passed. An explicit Python/Git invariant check confirmed all 16 package versions unchanged, D-077–D-085 still Proposed, and the released tag object/target unchanged.
- Fresh archive evidence: `cabal sdist all --project-file=cabal.project.ci --output-directory=dist-newstyle/el02-sdist` produced all 16 development archives; `python3 scripts/check-learning-archive dist-newstyle/el02-sdist` passed archive-only component compilation, all three executions/freshness comparisons, and all 30 checker tests without Git or checkout symlinks. Logs: `/tmp/el02-resume-sdist.log`, `/tmp/el02-resume-archive.log`. Root artifact: `dist-newstyle/el02-sdist/Markovian-2026.9.3.0.tar.gz`. After the evidence-only checklist update, refreshed all 16 archives and verified byte-for-byte membership of all 46 changed/new distributed files (CI YAML is not a source-distribution input); archived executable sources and outputs are unchanged by that evidence edit.
- Independent EL-02 teaching/correctness review remains mandatory after this writer. EL-03 was not started. EL-06's resource experiment and EL-07's later integration/review gates remain unchecked; no browser-startup, Haddock, GHC 9.4.8, CUDA, benchmark, hosted-CI, release or publication result is inferred from this host-only stage.

## Completion standard

This program is complete only when EL-00 through EL-07 have evidence for every applicable item. Do not replace implementation with plans. Do not mark a failed or unexecuted gate as passed. Scope changes require explicit rationale and supervisor review. Unsupported universal features remain excluded rather than becoming hidden obligations.
