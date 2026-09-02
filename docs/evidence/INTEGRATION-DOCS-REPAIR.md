# Integration and documentation repair evidence

Status: local dirty-worktree evidence only.

This record applies to the uncommitted overlay on `1834799773c6f600624d879b1d823c5e6f2a09cf`. It does not accept D-061 through D-076, establish release readiness, or authorize publication.

## Checked integration inventory

- `release/packages.tsv` and `ci/packages.tsv` agree on 16 packages and dependency tiers.
- `release/components.tsv` contains 18 test suites and 11 benchmarks. The eighteenth suite is the reviewed private tensor allocator-fault suite.
- The release metadata checker enforces the exact public sibling dependency graph, unique exposed-module ownership, one exposed-module golden per package, sibling bounds, and `Proposed` status for D-061 through D-076.
- Generated archive projects enable tests and benchmarks globally and enable `+markovian-integration` for `markovian-neural`.
- The release tool can generate one isolated archive dependency closure for each selected package. Closure construction includes test-only sibling edges without treating them as public architecture.

## Commands that passed

```text
all 16 package-local cabal check commands
bash scripts/check-package-manifest
bash scripts/check-root-topology
bash scripts/check-release-metadata
bash scripts/check-release-policy
python3 scripts/test_release_tool.py                  # 26 tests
cabal build all --project-file=cabal.project.ci       # GHC 9.8.4, -Werror
cabal test all --project-file=cabal.project.ci --test-show-details=direct
bash packages/markovian-reverse/scripts/check-reverse-boundary
bash packages/markovian-tensor/scripts/check-tensor-boundary
bash packages/markovian-tensor-reverse/scripts/check-tensor-reverse-boundary
bash packages/markovian-safetensors/scripts/check-safetensors-boundary
scripts/check-book                                    # 414 display-math blocks
cabal sdist all                                       # 16 archives
```

All 18 manifested suites passed in the recorded GHC 9.8.4 run. Each of the 16 fresh archives passed bounded archive validation. An isolated root archive build and root-topology boundary passed. A generated archive-only closure containing `markovian-reverse`, `markovian-tensor`, `markovian-tensor-reverse`, and `markovian-gpu` passed the CUDA-disabled GPU build, test, and device boundary.

The artifact manifest implementation now records byte counts and SHA-256 digests for every packaged `test/golden` file. `SHA256SUMS` covers the artifact manifest. This binds deterministic semantic reports to a prepared bundle; it does not prove the reports are correct.

## Open gates

The 11 benchmarks, all 16 isolated archive consumers, isolated warning-free Haddock and declaration coverage passes, GHC 9.4.8, preferred-oldest, CUDA-enabled archive compilation, hosted compile-only CUDA, protected UUID-bound hardware, four-tool Compute Sanitizer, hosted provenance, and a complete clean immutable-revision preparation were not run by this repair. Human publication authorization is also absent.

`Markovian.Tensor.Reverse` is owned by `markovian-tensor`. Its allocator capability remains private inside that package and preserves atomic staged multi-output allocation. `markovian-tensor-reverse` owns only the bounded reverse-program adapter. There is no generic tensor or CUDA reverse-program lowering.

No commit, tag, push, upload, release, or publication operation was performed.
