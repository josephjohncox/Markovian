# Workflows

This document defines the required execution workflow for future agents.

## 1. Before editing

Before any edit:

1. Read `README.md`.
2. Read `docs/CONTEXT.md`.
3. Read `TODO.md`.
4. Read `docs/DECISIONS.md`.
5. Read the relevant sections of `docs/ARCHITECTURE.md`.
6. Run `git status --short`.
7. List tracked and untracked files.
8. Confirm the `NEXT` task or record the user-selected priority change.
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

Ready when: the writer can explain current behavior, target behavior, affected invariants, and likely blast radius.

### Phase B: Decide

1. Classify the change as documentation, behavior, semantics, public API, dependency, backend, or release work.
2. Find the recorded decision that defines its technical boundary.
3. Add a proposed decision if no recorded decision applies.
4. List the required evidence and success criteria.
5. Stop when a required design decision remains open.

Ready when: a recorded decision defines every semantic or public contract change.

### Phase C: Plan

1. Select the smallest coherent slice.
2. List files and assign one writer.
3. List required tests before implementation.
4. List required documentation updates.
5. List exact commands and expected evidence.
6. Record dependencies and rollback steps.

Ready when: the plan maps each success criterion to a file or command.

### Phase D: Implement

1. Add or update failing contract tests when the toolchain permits test execution.
2. Make the smallest source or documentation change.
3. Keep semantic core changes separate from backend and dependency changes.
4. Keep dependency updates separate from semantic changes.
5. Do not refactor unrelated code.
6. Review the diff after each coherent slice.

Ready when: the diff contains no unexplained file and no unrelated behavior change.

### Phase E: Verify

Run the required commands for the change class. Capture the command, revision, exit code, and relevant output.

Ready when: every success criterion has current evidence or has an explicit blocked status.

### Phase F: Document

1. Update the documents required by the change matrix.
2. Record decision changes before status changes.
3. Update `docs/CONTEXT.md` with current defects and next resume facts.
4. Update `TODO.md` only when evidence supports the status.
5. Update `CHANGELOG.md` only for verified released behavior.
6. Recheck all links and file references.

Ready when: source, tests, Cabal metadata, and documents describe the same contract.

### Phase G: Handoff

1. Run `git status --short`.
2. Review the complete diff.
3. List changed files.
4. List commands and results.
5. List checks not run and their reason.
6. List remaining risks and blockers.
7. State the exact next task.

Ready when: another agent can resume from `docs/CONTEXT.md` and the `NEXT` marker.

## 4. Required commands

### 4.1 Documentation-only changes

Run these commands in Bash:

```bash
git status --short
git diff --check
git diff -- README.md TODO.md docs CHANGELOG.md Markovian.cabal
scripts/check-book

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
  README.md TODO.md docs/*.md docs/book/src/*.md
```

`git diff` omits untracked files. The loop prints each untracked durable file as a complete added-file diff.

The writer must read both outputs. A file list alone is not a content review.

`scripts/check-book` verifies local chapter links, anchors, include targets, the pinned `mdbook` version, the vendored MathJax digest, local generated script tags, absence of the external loader, every source-to-generated display-math block, supported Markdown-to-TeX recovery markup, and the complete HTML build. Before publication, execute the generated pages through MathJax startup and require one rendered container per display block, no raw delimiters, and no MathJax errors.

Do not claim a Haskell build result for documentation-only work unless the current revision produced it.

#### 4.1.1 GitHub Pages publication

Enable Pages with GitHub Actions before the first deployment. Restrict the `github-pages` environment to `main`.

The `Pages` workflow runs on all pushes to `main`. A manual run must select `main`. The build job runs `scripts/check-book` and uploads `docs/book/build`. The deployment job uses only that checked artifact.

A local book gate is not hosted deployment evidence. Do not call the book published until the deployed revision has a successful hosted run and a reachable URL.

After a push to `main`, collect this evidence:

```sh
run_id="$(
  gh run list \
    --workflow Pages \
    --branch main \
    --event push \
    --limit 1 \
    --json databaseId \
    --jq '.[0].databaseId'
)"
gh run watch "$run_id" --exit-status
test "$(gh run view "$run_id" --json headSha --jq .headSha)" = \
  "$(git rev-parse HEAD)"
gh run view "$run_id" \
  --json conclusion,headSha,jobs,url \
  --jq '{conclusion,headSha,url,jobs:[.jobs[]|{name,conclusion}]}'
gh api repos/josephjohncox/Markovian/pages \
  --jq '{html_url,status,build_type,https_enforced}'
curl --fail --location --retry 10 --retry-delay 6 \
  https://josephjohncox.github.io/Markovian/ \
  >/dev/null
```

Record the run URL only after these commands pass. Record required package CI evidence separately. The Pages workflow does not run compiler or source-distribution gates.

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
hlint src
fourmolu --mode check $(git ls-files '*.hs')
cabal-fmt --check Markovian.cabal \
  backends/markovian-gpu/markovian-gpu.cabal \
  backends/markovian-neural/markovian-neural.cabal \
  backends/markovian-neural-bridge/markovian-neural-bridge.cabal
```

### 4.4 Dependency or bounds changes

Also run:

```sh
cabal build all --prefer-oldest --project-file=cabal.project.ci
cabal test all --prefer-oldest --project-file=cabal.project.ci
```

Run the supported compiler matrix in CI. Record each compiler and package-plan result.

### 4.5 Public API changes

Run the two-part documentation gate used by CI and `scripts/prepare-release`. First, a warning-enabled `cabal install --lib --enable-documentation` into a fresh store must emit no build or Haddock warnings and produce one interface per manifest package. In its scrubbed home, Cabal 3.16 emits a two-line missing-package-list advisory even with `active-repositories: :none`; the warning checker permits only that exact non-build advisory. Second, a serial `cabal haddock all` pass with `--haddock-options=--no-warnings` supplies declaration-coverage evidence only; `scripts/check-haddock-coverage` excludes declared private modules and requires exact public coverage. Never use the suppressed pass as warning evidence.

Compile all README examples. Review the exposed module list and package metadata. Before the first release, the package could remove incorrect interfaces without a compatibility phase.

### 4.6 Release changes

Run the static release checks during implementation:

```sh
bash scripts/check-release-metadata
bash scripts/check-release-policy
python3 scripts/test_release_tool.py
```

Run full preparation only from a clean immutable revision:

```sh
bash scripts/prepare-release \
  --revision "$(git rev-parse HEAD)" \
  --output ../markovian-release-artifacts
```

The script runs all package checks, tests, lower-bound resolution, boundaries, benchmarks, Haddock, and book checks. It creates every archive twice and compares bytes.

The script validates each archive before extraction. It builds the combined archive-only graph with all manifested suites, benchmarks, and required flags, checks the Cabal plan and deterministic receipts, and builds a fresh exact consumer.

It generates SPDX 2.3 source SBOMs with package verification codes and file license information, validates them with pinned independent tooling, and writes a deterministic artifact manifest. `SHA256SUMS` covers archives, SBOMs, the manifest, and `SOURCE-REVISION`. Linux atomic no-replace rename finalizes the temporary output only after all checks pass.

`release/packages.tsv` is the reviewed 16-package integration graph. D-061 is `Accepted` after the graph and every other acceptance gate passed; the manifest alone still does not authorize publication.

Run `.github/workflows/release-prepare.yml` only for an exact reviewed revision. Recheck each action commit against its current official release first. Verify provenance and all bundle checksums before archive extraction.

The preparation workflow has no Hackage credential. An unprivileged job validates revision input before privileged jobs can start, and every job independently validates the revision. The attestation job does not check out or execute repository code. It attests archives, SBOMs, the manifest, source revision, and the checksum file.

Stop after preparation and attestation. Do not add a release date, create a tag, upload a candidate, or publish without explicit user approval.

If a later publication stops after one package, record the published subset. Hackage publication is not atomic, and a published version cannot be replaced.

### 4.7 Backend performance changes

Run reference differential tests and representative benchmarks. Include compilation, transfer, and setup costs.

For CUDA changes, run the disabled contract first. On protected hardware, normalize and require the configured UUID and pass it to the executable for `DeviceByUUID` selection. Reproduce the PTX; run admission, independent CPU/CUDA matrix and VJP differentials, all-coordinate finite differences, scoped-fork ownership, and the device compile-fail boundary. Enable `cuda-fault-injection` only for validation and exercise allocation, transfer, launch, second launch, synchronization, copy-back, free, teardown, and combined primary/action/cleanup failures. Then run Compute Sanitizer and the transfer-inclusive benchmark. Never run untrusted pull-request code on the persistent GPU runner. Fallback tests must distinguish launch commitment from fallback permission and reject fallback after any cleanup failure. A missing selected device is a hardware-job failure, not a skip.

Record hardware, software versions, precision, seed policy, model sizes, kernel ABI and hash, complete benchmark commands, every raw sample, and whether timing includes admission, transfer, synchronization, copy-back, and cleanup. Do not infer speedup from the `-O0` list-based CPU reference.

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
| Public API change | Architecture module map, Haddock, README, Cabal review |
| Module or dependency change | Architecture boundaries, `Markovian.cabal`, dependency rationale |
| Priority or status change | `TODO.md` with evidence |
| New defect or resolved defect | `docs/CONTEXT.md` with file and line evidence |
| Workflow or check change | `docs/WORKFLOWS.md` and related TODO success criteria |
| Released user behavior | `CHANGELOG.md` and README when relevant |
| Decision reversal | New superseding entry in `docs/DECISIONS.md` |
| Backend compatibility change | Architecture boundary, required evidence, differential tests, benchmarks |

A task is incomplete when a required document update is missing.

Decision history remains intact. Correct factual mistakes with a dated note or a superseding entry.

## 7. Anti-drift checks

Run these checks before handoff:

1. Review `git status`, the tracked diff, and each complete untracked-file diff.
2. Compare exposed modules in `Markovian.cabal` with the architecture module map.
3. Compare direct imports with each Cabal component dependency list.
4. Compare exported behavior with README and Haddock examples.
5. Compare test names with TODO success criteria.
6. Compare current defects with `docs/CONTEXT.md` line references.
7. Compare recorded decisions with normative architecture statements.
8. Compare `TODO.md` statuses with linked evidence and known blockers.
9. Compare the `NEXT` marker with the highest `READY` task or its blocker resolution.
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
- Required evidence.
- Evidence links when tests or benchmarks determine the selected outcome.
- Superseded ID when applicable.

A proposed decision records an open design question. The designated reviewer or user records the selected outcome.

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
5. Resume only after the reviewer or user records the decision.

### 9.4 Flaky test

1. Do not rerun until it happens to pass.
2. Record the seed and output.
3. Replace frequency assertions with exact, seeded, or scripted checks.
4. Quarantine only with a named owner and removal criterion.

### 9.5 Performance regression

1. Confirm the benchmark environment and workload.
2. Compare end-to-end and kernel-only timings.
3. Check numerical and stochastic equivalence first.
4. Revert an optimization that changes semantics without a corresponding technical decision.

### 9.6 Documentation conflict

1. Identify the source, test, package, and document claims.
2. Find the recorded decision.
3. Correct all affected authorities in one change.
4. Add a decision when no recorded contract resolves the conflict.

## 10. Exact semantic tower workflow

D-038 work follows the S1 through S6 roadmap in `TODO.md`.

1. Preserve the direction `Algebra -> finite witnesses -> raw matrices -> normalized refinements -> Bayesian operations`. Matrix modules cannot import distributions, POMDPs, circuits, or backends.
2. Keep empty `FiniteSet` boundaries separate from nonempty `FiniteObject` states, priors, and probability supports. Permit a normalized empty-to-empty arrow only by vacuous row validation.
3. Use labelled extensional equality for matrix laws. Do not use storage equality or `ExactFiniteDist` equality. Ordinary witness `Eq` remains layout equality.
4. Run the focused `Markovian-test` suite after each stage. S1 evidence must include category, tensor, biproduct, dagger, compact, trace, normalization, deterministic, convex, and transpose-counterexample test names. S2 evidence must include joint balance, normalization, support restriction, identity, composition reversal, tensor, double inversion, almost-sure uniqueness, checked prior flow, and POMDP differential tests. S3 and S4 evidence must include purity compile failures, sharing versus fanout, structural-fold preservation, coherence, convex choice, legacy IR and dense CPU differentials, and first-order compilation equations. S5 evidence must include typed-map failures, source-row canonicalization, nominal quotient witnesses, canonical class order, layout-independent cocone factorization, both associator round trips, both unitor isomorphisms, tensor, boundary reversal without state reversal, extensional middle-map cell composition, interchange, and the narrow decorated-circuit denotation boundary. S6 evidence must include every unique-production and cycle rejection, actual stable cycle witnesses, assignment and signature validation, edge-context failures, topological versus complete-valuation agreement, sharing versus repeated execution, discard, conditional products, successful schedule and renaming independence, composition, tensor, units, associations, and the dedicated acyclic proof-boundary and purity scripts.
5. Run Fourmolu, HLint, `scripts/check-refinement-roles`, the stage-specific compile-fail scripts, and the project-scoped `-Werror` build before marking a stage complete.
6. Do not expose transpose, compact structure, trace, or raw addition through `StochasticMatrix`.
7. Permit copy-naturality rewrites only when a `DeterministicMatrix` or deterministic purity index supplies construction evidence. Assign nominal roles to each proof-carrying refinement.
8. Keep matrix conjugate transpose, prior-indexed Bayesian inversion, and structured-cospan boundary reversal as separately named operations with no common class. Do not add a Bayesian inversion placeholder before S2 supplies priors and support restriction.
9. Stop before arbitrary open-system black-boxing or universal feedback semantics. D-069 permits only explicit delayed execution, checked proper first exit, and nilpotent timed closure.

## 11. New-feature evidence workflow

P0 through P6, the greenfield cleanup, exact semantic-tower stages S1 through S6, and bounded implementation slices S7 through S11 are `DONE`. The coordinated release graph has 16 packages in `release/packages.tsv`; `ci/packages.tsv` remains the checked integration inventory. D-053 through D-076 are `Accepted` only for their recorded finite and bounded scopes. The complete GHC 9.4.8, GHC 9.8.4, lower-bound, source, Haddock, formatting, benchmark, compile-fail, package-boundary, archive-only, checksum, SPDX, and provenance gates passed before release. The digest-pinned no-GPU CUDA compile job and the UUID-bound protected GB10 job also passed, including enabled differentials, fault fixtures, PTX reproduction, the transfer-inclusive benchmark, and all four Compute Sanitizer tools. These results do not widen the contracts: arbitrary cyclic graph semantics, universal feedback, continuous-time black-boxing, unrestricted MDP black-boxing, general tensor semantics, generic reverse-program device lowering, general autodiff, and general device correctness remain deferred.

A new feature must use this sequence:

1. Complete the before-editing steps and identify its requirements in `TODO.md`.
2. Record the use case, denotation or approximation relation, and failure semantics.
3. State required laws, differential tests, or estimator assumptions before implementation.
4. Keep hardware, framework, and runtime dependencies outside the semantic core.
5. Add deterministic reference evidence before statistical or benchmark evidence.
6. Run both compiler versions, all 18 manifested test suites, source distributions, every applicable backend differential test, all compile-fail boundary scripts, and all 11 benchmarks in `release/components.tsv`. Repeat tests and applicable benchmarks from unpacked source archives. Run CUDA-enabled compilation in the pinned no-GPU job and device execution separately on protected hardware; portable source-distribution CI does not establish device correctness.
7. For exact control, record residual, bound, tie-order, terminal, and iteration-limit fixtures.
8. For sampled learning, record exact seeded generator states and split-run equality.
9. For neural derivatives, record the finite-difference tolerance and every checked parameter or input coordinate.
10. For replay or target updates, record ordering, eviction, successful-update count, and failed-update scheduling fixtures.
11. For information quantities, record logarithm base, support behavior, decomposition laws, product laws, and finite-difference evidence.
12. For a push-pull pair, state both transformer types, their pairing law, and whether the backward value is a payoff, cotangent, posterior, or utility. Test exact object alignment, layout reordering, identity, composition direction, and pairing where those laws apply. Do not present payoff pullback as prior-indexed Bayesian inversion.
13. For game semantics, define arenas, move ownership, legal plays, strategy composition, and observational equality before implementation.
14. For named inventory benchmarks, reproduce the source model's timing and assumptions, expose every relaxation, compare with a bounded exact oracle, and report truncation mass, value error, policy regret, state count, and runtime.
15. For categorical rewrites, name the law and evidence that authorizes the rewrite. Purity-indexed deterministic rewrites and approximate fusions require different equality contracts.

Do not widen the finite semantic core for speculative abstractions without recorded requirements and evidence.

Fourmolu 0.20 does not parse the repository's LaTeX-style literate Haskell files. Source-format checks therefore check all `.hs` files and explicitly exclude the three `.lhs` files from Fourmolu. This is a parser limitation, not a waiver from compiler or HLint diagnostics. GHC and HLint continue to check those files.

### Intentional terminology allowlist

Use `gate` only for a named executable CI, build, test, book, or compile-fail check. Preserve `stochastic gate` as a circuit term and `admissible` as a mathematical term.
