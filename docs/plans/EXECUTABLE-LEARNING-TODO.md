# Executable learning and bounded extensions: implementation checklist

## Objective

Implement all recommendations in [the durable review](../evidence/LEARNING-REVIEW-2026-09-05.md). The user explicitly requested this checklist and execution against it. Do not stop after planning or documentation alone.

Baseline: `7edc04a2d9a0de6af6fed1ccccb43303442210f5`.
Branch: `frontier/executable-learning`.

Status: **Active**. Start with EL-01. Every completion needs artifact paths and validation evidence in the execution log below. Keep incomplete items unchecked. A blocked item needs a concrete reason and next action. A passing focused test does not complete final integration.

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

- [ ] Resolve F01-F08 in the review, including the autodiff readiness wording and Kleisli composition order.
- [ ] Audit nearby current-guidance statements for the same contradictions. Preserve properly labeled historical evidence.
- [ ] Add checked capability records separating availability, decision status, and evidence scope.
- [ ] Generate capability presentation and validate package/module references. Do not equate current exposed-module snapshots with released snapshots.
- [ ] Add negative tests for stale capability output, invalid status combinations/references, and current-versus-release confusion.
- [ ] Freeze a bounded paired-difference API, ledger, rational limits, and failure precedence before implementation.
- [ ] Freeze a narrow exact feedback JVP API, admitted directions, nominal layout/ownership, ledger, and failure precedence before implementation.
- [ ] Freeze supplied-partition fixed-policy aggregation, observations/terminal/payoff rules, witness/quotient types, ledger, and failure precedence before implementation.
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

## Completion standard

This program is complete only when EL-00 through EL-07 have evidence for every applicable item. Do not replace implementation with plans. Do not mark a failed or unexecuted gate as passed. Scope changes require explicit rationale and supervisor review. Unsupported universal features remain excluded rather than becoming hidden obligations.
