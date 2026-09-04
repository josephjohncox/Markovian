# D-077 worktree scope

**Decision status:** Proposed

This report records the mixed worktree seen during the D-077 repair. It does not approve or merge any proposal. The D-078, D-079, and D-080 files were already present and were not reverted. Shared files contain changes for more than one proposal.

## D-077 surface

The D-077 repair uses these areas:

- `.github/workflows/ci.yml`, `.github/workflows/cuda-compile.yml`, and `.github/workflows/cuda-hardware.yml`
- `backends/markovian-gpu/` source, tests, benchmark, profile, generated artifacts, scripts, Cabal metadata, and README
- `docs/evidence/CUDA-D077-RECEIPTS.md` and `docs/evidence/CUDA-TENSOR-2026-09-02.md`
- the CUDA sections in `README.md`, `RELEASE-NOTES.md`, `TODO.md`, `docs/CONTEXT.md`, `docs/DECISIONS.md`, `docs/WORKFLOWS.md`, `docs/book/src/backends.md`, and `docs/book/src/laws-and-boundaries.md`
- `release/published-releases.json`, `release/README.md`, `RELEASE-CHECKLIST.md`, and the release policy, preparation, and test scripts
- shared source-distribution entries in `Markovian.cabal`

## Concurrent D-078 surface

D-078 changes the root `Markovian` package:

- `src/Markovian/Feedback/Value/` and `test/FeedbackValueExact.hs`
- `test/Main.hs`, `test/compile-fail/FeedbackBoundary.hs`, and `scripts/check-feedback-boundary`
- root library and test entries in `Markovian.cabal`
- `release/exposed-modules/Markovian.txt`
- feedback sections in `docs/ARCHITECTURE.md`, `docs/book/src/api-map.md`, `docs/book/src/choose-an-api.md`, and `docs/book/src/feedback.md`

## Concurrent D-079 surface

D-079 changes `markovian-continuous`:

- `packages/markovian-continuous/src/Markovian/Continuous/Kernel/JointAffine/`
- `packages/markovian-continuous/test/JointAffine.hs`
- `packages/markovian-continuous/test/compile-fail/JointAffineBoundary.hs` and `JointAffineRoles.hs`
- `packages/markovian-continuous/test/golden/joint-affine-accounting.txt`
- its Cabal file, README, changelog, main test, and boundary script
- `release/exposed-modules/markovian-continuous.txt`
- the Gate A text in `docs/book/src/continuous-probability.md`

## Concurrent D-080 surface

D-080 changes `markovian-autodiff`:

- `packages/markovian-autodiff/src/Markovian/Autodiff/Quote.hs`
- the public, check, compile, and internal syntax modules in that package
- `packages/markovian-autodiff/test/Main.hs`
- the three `AutodiffQuote*.hs` compile-fail fixtures
- its Cabal file, README, and boundary script
- `release/exposed-modules/markovian-autodiff.txt`
- quotation text in `docs/ARCHITECTURE.md`, `docs/book/src/api-map.md`, `docs/book/src/autodiff-lowering.md`, and `docs/book/src/choose-an-api.md`

## Shared governance and release files

`TODO.md`, `docs/CONTEXT.md`, `docs/DECISIONS.md`, `release/API-REVIEW.md`, and several book pages contain concurrent proposal text. `Markovian.cabal` contains both D-077 distribution entries and D-078 package entries. All 16 Cabal packages, sibling bounds, source-repository tags, the release package manifest, and the release example retain `2026.9.3.0` under the task's no-version-change invariant. This worktree is not release source, and `release/published-releases.json` prevents release preparation from rebuilding that published version from another revision. The unchanged metadata does not approve the concurrent proposals or create a candidate. No release candidate was prepared. Candidate notes and review records remain future work.
