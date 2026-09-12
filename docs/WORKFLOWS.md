# Contributor workflows

## Before editing

Read the affected source, callers, tests, and contract. Use
[CONTEXT.md](CONTEXT.md) for invariants, [TODO.md](../TODO.md) for open work,
and [DECISIONS.md](DECISIONS.md) for the relevant design decision.

Check `git status --short` and inspect untracked files. Preserve unrelated
work. Use an isolated worktree when another writer owns the checkout; assign
separate files or worktrees before working in parallel.

For a semantic or public API change, agree on signatures, denotation or
approximation relation, equality, ownership, resource limits, and error order
before implementation. Add a Proposed decision when no existing contract
covers the change. A passing test does not itself accept a decision.

## Repository contents

Track library source, executable tests, maintained documentation, and scripts
that reproduce checks. Keep agent prompts, handoffs, execution journals,
worktree audits, and temporary command receipts in ignored `.agent-scratch/`
storage. Put review findings in the PR and unresolved work in `TODO.md`.

Retain mathematical derivations and benchmark data that explain a contract or
measurement. Prefer repository-relative inputs and reproducible commands.
List package documentation explicitly. Tests should verify behavior and
structured metadata, not hashes of mutable prose or a required review narrative.

## Validation

Use the versions in [`toolchain.env`](../toolchain.env). Project builds use
GHC 9.14.1; GHC 9.8.4 is only for ancillary tool installation. The current
[CI workflow](../.github/workflows/ci.yml) specifies the compiler, lower-bound,
source, archive, and benchmark checks.

### Documentation

```sh
git diff --check
scripts/check-book
```

Review both tracked and untracked content. If teaching examples or their
recorded sources change, run `python3 scripts/check-learning --run`. Refresh a
stale receipt with `--write`, which compiles and executes the fixtures; review
any output changes. Source archives must contain all referenced local files.

`scripts/check-book` checks capability status, teaching references and receipt
freshness, local links, includes, the pinned mdBook and MathJax, rendered math
markup, and the HTML build. Before publishing changed mathematics, run MathJax
in a browser and check every display block for errors or raw delimiters.

### Haskell

```sh
cabal check
cabal build all --project-file=cabal.project.ci
cabal test all --project-file=cabal.project.ci --test-show-details=direct
```

Run the affected compile-failure boundaries and reference differentials. For
new features, also run all manifested benchmarks, package checks, and source
archives, including the applicable tests and benchmarks from unpacked archives.
The manifests list 16 packages, 18 test suites, and 11 benchmarks.

Use the pinned Fourmolu, HLint, and cabal-fmt checks from CI. Fourmolu 0.20
cannot parse the three LaTeX-style `.lhs` files, so only `.hs` files enter that
formatter. GHC and HLint still check the literate sources.

Dependency or bounds changes also require lower-bound resolution:

```sh
cabal build all --prefer-oldest --project-file=cabal.project.ci
cabal test all --prefer-oldest --project-file=cabal.project.ci
```

### Public APIs and Haddock

Compile teaching examples and review exposed modules, package bounds, and
module snapshots. Run the two documentation checks used by CI:

1. `scripts/check_haddock_install.py` installs libraries with documentation into
   a fresh isolated store. It checks every install log and public interface;
   build and Haddock warnings fail validation.
2. A separate `cabal haddock all --haddock-options=--no-warnings` run feeds
   `scripts/check-haddock-coverage`. This establishes declaration coverage,
   including the declared private-module exclusions, but cannot establish
   warning freedom.

See [Haddock checks](book/src/release-preparation.md#haddock-checks) for the commands.

### Semantic tests

Name the laws, equality contract, reference implementation, and failure cases
that each test covers. Add deterministic checks before statistical or timing
experiments. Use the applicable requirements below:

| Change | Required checks |
| --- | --- |
| Exact control | Residual and value bounds, tie order, terminal behavior, iteration limits. |
| Sampled learning | Exact seeded generator states and split-run equality. |
| Neural derivatives | Finite-difference tolerance and every input/parameter coordinate. |
| Replay and target updates | Ordering, eviction, successful-update counts, failure scheduling. |
| Information quantities | Logarithm base, support behavior, decomposition/product laws, finite differences. |
| Pushforward and pullback | Both transformer types and pairing law; object alignment, reordered layouts, identity, composition direction. Distinguish payoffs, cotangents, posteriors, and utilities. |
| Games | Arenas, move ownership, legal plays, strategy composition, observational equality. |
| Inventory benchmarks | Source timing and assumptions, explicit relaxations, exact oracle comparison, truncation mass, value error, policy regret, state count, runtime. |
| Categorical rewrites | The specific law and evidence permitting the rewrite; deterministic provenance and approximation relations remain separate. |

Keep the dependency order algebra → finite witnesses → raw matrices →
normalized refinements → Bayesian operations. Matrix modules cannot import
distributions, POMDPs, circuits, or backends. Preserve empty finite sets and
nonempty probability objects as separate types; an empty-to-empty normalized
arrow is valid only by vacuous row validation.

Matrix laws use labelled extensional equality; witness `Eq` compares layouts.
Raw transpose, compact structure, trace, and addition do not become stochastic
operations. Copy-naturality rewrites require deterministic construction
provenance and nominal roles. Conjugate transpose, Bayesian inversion, and
cospan boundary reversal retain distinct operations. See
[ARCHITECTURE.md](ARCHITECTURE.md) and the relevant decision for detailed laws
and boundaries, including the admitted feedback fragments.

## Documentation ownership

| Information | Maintained location |
| --- | --- |
| Public usage and examples | Package README, book chapter, Haddock. |
| Semantics, invariants, dependencies | `ARCHITECTURE.md`, relevant contract, Cabal metadata, tests. |
| Decision acceptance or reversal | `DECISIONS.md`; use a superseding entry for a reversal. |
| Current status and implementation priorities | `CONTEXT.md`, `TODO.md`, capability records. |
| Reproduction commands and contributor procedures | This file, validation scripts, CI. |
| Released behavior | `CHANGELOG.md`, release notes, migration guide. |
| Review results and temporary investigation | PR description or review. |

Link to the maintained description instead of copying its full status or
validation history into every document. Keep source, public modules, metadata,
and documentation consistent; historical release evidence retains its revision.

## Reporting results and failures

Report the command, tested revision or worktree state, exit status, and relevant
output or CI URL. Distinguish code inspection, compilation, execution, and
measurement. Performance results need a representative workload, warmup/sample
policy, hardware/software versions, end-to-end timings, and dispersion.

When a check fails, preserve the failure, fix its cause, and rerun the affected
checks. Report unavailable tools and unrun checks. Do not weaken assertions to
make a failing or flaky test pass. Resolve a semantic contradiction in the
contract before proceeding with dependent changes.

## Release and book publication

During release work, run:

```sh
bash scripts/check-release-metadata
bash scripts/check-release-policy
python3 scripts/test_release_tool.py
```

Follow [RELEASE-CHECKLIST.md](../RELEASE-CHECKLIST.md) for full preparation from
a clean immutable revision. Preparation validates the archive graph and
consumers, repeatable archive bytes, checksums, SBOMs, and provenance. Validate
archives before extraction and verify workflow attestations against their
actual subjects. A manifest or successful preparation does not publish packages.

Publication requires explicit authorization. If Hackage publication stops partway
through, record the published subset; published versions cannot be replaced.

The [Pages workflow](../.github/workflows/pages.yml) deploys the checked book
artifact from `main`. Configure Pages for GitHub Actions and restrict its
environment to `main`. Before claiming publication, verify a successful hosted
run for the deployed revision and a reachable site. Package CI remains separate.
See the [book workflow](book/src/book-workflow.md) for publication details.

## Backend performance and CUDA

Run reference differential tests and representative benchmarks. Include
compilation, transfer, and setup costs. Enabled CUDA compilation runs in the
pinned no-GPU workflow; device tests run separately on protected hardware.
Portable package checks cannot establish device correctness.

For CUDA changes, run the disabled contract first. On protected hardware, normalize and require the configured UUID and pass it to the executable for `DeviceByUUID` selection. Reproduce the PTX; run admission, independent CPU/CUDA matrix and VJP differentials, all-coordinate finite differences, scoped-fork ownership, and the device compile-fail boundary. Enable `cuda-fault-injection` only for validation and exercise allocation, transfer, launch, second launch, synchronization, copy-back, free, teardown, and combined primary/action/cleanup failures. Then run Compute Sanitizer and the transfer-inclusive benchmark. Never run untrusted pull-request code on the persistent GPU runner. Fallback tests must distinguish launch commitment from fallback permission and reject fallback after any cleanup failure. A missing selected device is a hardware-job failure, not a skip.

Record hardware, software versions, precision, seed policy, model sizes, kernel ABI and hash, complete benchmark commands, every raw sample, and whether timing includes admission, transfer, synchronization, copy-back, and cleanup. Do not infer speedup from the `-O0` list-based CPU reference.

<a id="82-deployment-scoped-gpu-evidence-amendment--2026-09-08"></a>

## GPU deployment evidence

The 2026-09-08 policy permits deployment-scoped raw-data retention. Historical
receipts and release obligations retain their original requirements.

At each GPU deployment or promotion, require the complete raw evidence and verify it against the actual deployed revision.
Preserve source, profile, PTX, executable, command, log, session, UUID, tool-version, checksum, and all 20 ordered raw-sample bindings.
Keep correctness, all four sanitizer records, and benchmark evidence in one bound session.
Reject missing or changed bytes, substituted commands, mismatched identities, and failed outcomes under the existing validator rules.

Verify signatures for every evidence subject against its actual bytes.
Keep certificate identity, issuer, repository, workflow, source revision, run/attempt, claims, and transparency checks.
Unsigned metadata, artifact retrieval, digests alone, and prose summaries do not replace cryptographic verification.
Do not weaken validator failure order, numeric comparisons, sanitizer markers, or the semantic checksum.

Finite storage is allowed, including the current 90-day workflow retention.
The complete evidence must remain available through deployment or promotion verification, not forever afterward.
Later expiry does not retroactively fail a completed verification or deployment.
Missing raw data cannot support a new verification or promotion.
Obtain newly bound evidence when the required raw data is unavailable.

Preserve a compact record after raw-data expiry.
Record the tested revision, deployed revision when applicable, profile identity, workflow run/attempt, session, and subject digests.
Also record the verification date, commands/tool identity, signature checks, result, evidence location, and retention expiry.
Keep the original result and record later data unavailability separately.
A compact record records past verification. It does not replace raw evidence for a new verification.

Governance acceptance may cite exact tested revision `3e850085fa96c4e48a80270b9e49e9f55fe0f757` plus an explicit, separately reviewed docs-only delta.
Name both delta endpoints and changed paths. Do not assume a descendant commit has the tested revision's hardware evidence.
Actual GPU deployment still requires evidence that binds its deployed revision.
Do not relabel failed runs or combine them with a successful receipt.

The recorded environment has no required-reviewer rules or deployment-branch restrictions.
This amendment changes neither permissions nor environment configuration and adds no infrastructure gate.
Keep separate review and authorization requirements. Do not infer technical enforcement that the environment does not provide.
Released sources, tags, assets, immutable tensor storage, pinned actions, and
digest-based content identity retain their existing requirements. This policy
does not authorize a workflow run, deployment, environment change, or publication.
