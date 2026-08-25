# Workflows

This document defines the required execution workflow for future agents.

## 1. Start gate

Before any edit:

1. Read `README.md`.
2. Read `docs/CONTEXT.md`.
3. Read `TODO.md`.
4. Read `docs/DECISIONS.md`.
5. Read the relevant sections of `docs/ARCHITECTURE.md`.
6. Run `git status --short`.
7. List tracked and untracked files.
8. Confirm the `NEXT` task or record the user-authorized priority change.
9. Identify the one writer for the branch and worktree.

Stop if the worktree contains unexplained changes. Do not overwrite work owned by another writer.

## 2. One-writer policy

One agent owns writes in one branch and worktree at a time.

Advisors can read and report. They do not edit the assigned worktree.

Parallel agents can work only in separate worktrees and branches. Assign file ownership before they start.

The writer integrates changes in dependency order. The writer resolves conflicts and runs final checks.

Do not let two agents update `TODO.md`, `docs/CONTEXT.md`, or `docs/DECISIONS.md` in parallel.

## 3. Phase workflow

### Phase A: Inspect

1. Read every file affected by the task.
2. Read callers, tests, package declarations, and authoritative documents.
3. Record current file and line evidence.
4. Check tool availability.
5. State which checks cannot run.

Gate: the writer can explain current behavior, target behavior, affected invariants, and likely blast radius.

### Phase B: Decide

1. Classify the change as documentation, behavior, semantics, public API, dependency, backend, or release work.
2. Find the accepted decision that authorizes it.
3. Add a proposed decision if no accepted decision applies.
4. List proof obligations and acceptance criteria.
5. Stop when a required decision remains open.

Gate: an accepted decision covers every semantic or public contract change.

### Phase C: Plan

1. Select the smallest additive slice.
2. List files and assign one writer.
3. List required tests before implementation.
4. List required documentation updates.
5. List exact commands and expected evidence.
6. Record dependencies and rollback steps.

Gate: the plan maps each acceptance criterion to a file or command.

### Phase D: Implement

1. Add or update failing contract tests when the toolchain permits test execution.
2. Make the smallest source or documentation change.
3. Keep semantic core changes separate from backend and dependency changes.
4. Keep dependency updates separate from semantic changes.
5. Do not refactor unrelated code.
6. Review the diff after each coherent slice.

Gate: the diff contains no unexplained file and no unrelated behavior change.

### Phase E: Verify

Run the required commands for the change class. Capture the command, revision, exit code, and relevant output.

Gate: every acceptance criterion has current evidence or has an explicit blocked status.

### Phase F: Document

1. Update the documents required by the change matrix.
2. Record decision changes before status changes.
3. Update `docs/CONTEXT.md` with current defects and next resume facts.
4. Update `TODO.md` only when evidence supports the status.
5. Update `CHANGELOG.md` only for verified released behavior.
6. Recheck all links and file references.

Gate: source, tests, Cabal metadata, and documents describe the same contract.

### Phase G: Handoff

1. Run `git status --short`.
2. Review the complete diff.
3. List changed files.
4. List commands and results.
5. List checks not run and their reason.
6. List remaining risks and blockers.
7. State the exact next task.

Gate: another agent can resume from `docs/CONTEXT.md` and the `NEXT` marker.

## 4. Required commands

### 4.1 Documentation-only changes

Run these commands in Bash:

```bash
git status --short
git diff --check
git diff -- README.md TODO.md docs CHANGELOG.md Markovian.cabal

while IFS= read -r file; do
  git diff --no-index -- /dev/null "$file"
  rc=$?
  if [ "$rc" -ne 0 ] && [ "$rc" -ne 1 ]; then
    exit "$rc"
  fi

  git diff --no-index --check -- /dev/null "$file"
  rc=$?
  if [ "$rc" -ne 0 ] && [ "$rc" -ne 1 ]; then
    exit "$rc"
  fi
done < <(git ls-files --others --exclude-standard -- \
  README.md TODO.md docs CHANGELOG.md Markovian.cabal)

python3 /home/josephcox/.pi/agent/skills/ste-writing/scripts/ste-lint.py \
  README.md TODO.md docs/*.md
```

`git diff` omits untracked files. The loop prints each untracked durable file as a complete added-file diff.

The writer must read both outputs. A file list alone is not a content review.

Also verify links and cited paths with an available Markdown link checker. If none is installed, record that fact and inspect local links manually.

Do not claim a Haskell build result for documentation-only work unless the current revision produced it.

### 4.2 Every Haskell change

Run:

```sh
cabal check
cabal build all --project-file=cabal.project.ci
cabal test all --project-file=cabal.project.ci --test-show-details=direct
```

### 4.3 Format changes

Run pinned formatter versions:

```sh
fourmolu --mode check $(git ls-files '*.hs' '*.lhs')
cabal-fmt --check Markovian.cabal
```

### 4.4 Dependency or bounds changes

Also run:

```sh
cabal build all --prefer-oldest --project-file=cabal.project.ci
cabal test all --prefer-oldest --project-file=cabal.project.ci
```

Run the supported compiler matrix in CI. Record each compiler and package-plan result.

### 4.5 Public API changes

Also run:

```sh
cabal haddock all --project-file=cabal.project.ci \
  --enable-documentation --haddock-all --haddock-hyperlink-source
```

Compile all README and migration examples. Review the package version under the Haskell PVP.

### 4.6 Release changes

Run all prior gates. Then run:

```sh
cabal check
cabal sdist --output-directory=dist-sdist
```

Unpack the generated archive in a clean temporary directory. Build and test that unpacked tree.

Do not add a release date, tag, or success claim before this evidence exists.

### 4.7 Backend performance changes

Run reference differential tests and representative benchmarks. Include compilation, transfer, and setup costs.

Record hardware, software versions, precision, seed policy, model sizes, and complete benchmark commands.

## 5. Evidence rules

A completion claim needs evidence from the current revision.

Valid command evidence includes:

- The exact command.
- The tested revision or worktree state.
- Exit code zero.
- Relevant untruncated output or a durable CI URL.

Valid semantic evidence includes:

- A named deterministic test.
- A property test with stated generators and equality contract.
- A derivation linked from the decision log.
- A differential test against the named reference interpreter.

Valid performance evidence includes:

- A representative workload.
- Warmup and sample policy.
- Hardware and dependency versions.
- End-to-end timings.
- Variance or confidence information.

Static inspection can prove that text or code exists. It cannot prove compilation, runtime behavior, numerical accuracy, or convergence.

A missing tool produces `BLOCKED`, not `DONE`. Record the failed command and exit code when possible.

## 6. Documentation change matrix

| Change | Required document updates |
| --- | --- |
| Semantic or invariant change | `docs/ARCHITECTURE.md`, `docs/DECISIONS.md`, tests, `docs/CONTEXT.md` |
| Public API change | Architecture module map, Haddock, README, migration guide, Cabal review |
| Module or dependency change | Architecture boundaries, `Markovian.cabal`, dependency rationale |
| Priority or status change | `TODO.md` with evidence |
| New defect or resolved defect | `docs/CONTEXT.md` with file and line evidence |
| Workflow or gate change | `docs/WORKFLOWS.md` and related TODO acceptance criteria |
| Released user behavior | `CHANGELOG.md` and README when relevant |
| Decision reversal | New superseding entry in `docs/DECISIONS.md` |
| Backend admission | Architecture boundary, decision proof obligations, differential tests, benchmarks |

A task is incomplete when a required document update is missing.

Accepted decision history remains intact. Correct factual mistakes with a dated note or a superseding entry.

## 7. Anti-drift checks

Run these checks before handoff:

1. Review `git status`, the tracked diff, and each complete untracked-file diff.
2. Compare exposed modules in `Markovian.cabal` with the architecture module map.
3. Compare direct imports with each Cabal component dependency list.
4. Compare exported behavior with README and Haddock examples.
5. Compare test names with TODO acceptance criteria.
6. Compare current defects with `docs/CONTEXT.md` line references.
7. Compare accepted decisions with normative architecture statements.
8. Compare `TODO.md` statuses with linked evidence and known blockers.
9. Compare the `NEXT` marker with the highest eligible task or its blocker resolution.
10. Search for generated placeholders before release work.
11. Search for hidden horizons, discounts, seeds, tolerances, and learning schedules.
12. Confirm that core modules have no backend dependency.
13. Confirm that stochastic sharing remains explicit in compiler work.

Treat disagreement as a failed review. Do not resolve it through silent precedence.

## 8. Decision recording

Use the next decision ID in `docs/DECISIONS.md`.

Each decision contains:

- ID and title.
- Status.
- Decision or question.
- Rationale.
- Consequences.
- Alternatives when material.
- Proof obligations.
- Evidence links when accepted through tests or benchmarks.
- Superseded ID when applicable.

A proposed decision does not authorize implementation. The designated reviewer or user accepts it.

Do not use architecture text to hide an unresolved decision. Mark the uncertainty and link the proposed entry.

## 9. Failure handling

### 9.1 Missing tool

1. Record the missing executable.
2. Record the attempted command and exit code.
3. Continue only with checks that do not depend on the tool.
4. Mark affected criteria `BLOCKED`.
5. Do not infer success from static inspection.

### 9.2 Test or build failure

1. Preserve the first useful failure output.
2. Reduce the failure to the smallest reproducer.
3. Decide whether the change or baseline caused it.
4. Revert unrelated edits.
5. Fix within task scope or mark the task blocked.
6. Rerun the failed command and all dependent commands.

### 9.3 Semantic uncertainty

1. Stop source implementation.
2. Add a proposed decision with alternatives.
3. Add a focused characterization or model test when possible.
4. Request review.
5. Resume only after acceptance.

### 9.4 Flaky test

1. Do not rerun until it happens to pass.
2. Record the seed and output.
3. Replace frequency assertions with exact, seeded, or scripted checks.
4. Quarantine only with a named owner and removal criterion.

### 9.5 Performance regression

1. Confirm the benchmark environment and workload.
2. Compare end-to-end and kernel-only timings.
3. Check numerical and stochastic equivalence first.
4. Revert an optimization that changes semantics without authorization.

### 9.6 Documentation conflict

1. Identify the source, test, package, and document claims.
2. Find the accepted decision.
3. Correct all affected authorities in one change.
4. Add a decision when no accepted contract resolves the conflict.

## 10. Exact next workflow

P0 and P1 are `DONE`. D-023 through D-028 define the verified environment, exact laws, and validated policy closure. Nineteen tests and all completed local gates pass. P2.1 is `NEXT`.

The next writer must use this sequence:

1. Complete the start gate and read D-004, D-011, D-016, and the objective equations.
2. Record the exact evaluator's model, policy, objective, and error boundary before implementation.
3. Implement finite-horizon exact expectation without unfolding a recursive tree.
4. Apply each transition reward once and each reached terminal payoff once.
5. Make horizon zero, discount placement, and continuing-state cutoff explicit.
6. Add terminal, deterministic, weighted, discounted, horizon-zero, and self-loop tests.
7. Compare exact evaluator results with direct finite sums.
8. Run both compiler versions and every completed package gate.

Do not reuse the legacy evaluator or infer an objective from hidden defaults.
