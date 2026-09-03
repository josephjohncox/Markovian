# Public API review

This review approves the `2026.9.3.0` 16-package integration API, 18 test suites, and 11 benchmarks for their stated bounded scopes.

The files in `release/exposed-modules` are checked API snapshots. Each public API change needs a new review.

## D-061 boundary

The D-061 package migration is accepted for the reviewed package graph below.

- The root library depends only on `base`.
- `Markovian.Action` owns the nominal `ActionId`.
- The root does not expose numerical, sampled, learning, approximate, dense, benchmark, tensor, continuous, neural, or device modules.
- `ExactFiniteDist` has checked bind and no `Monad` instance.
- `ExactKernel` is fallible and has no unrestricted category or arrow instances.
- The old finite IR is private regression code.
- Public dense execution starts from exact circuits.
- `Markovian.Tensor.Reverse` is the tensor package's closed public tape API. Its allocator capability remains a private `other-modules` implementation detail. `markovian-tensor-reverse` owns only the bounded adapter from those tapes to effect-capable reverse programs.
- `denseReverseCircuit` is the neural package's explicit adapter to `markovian-reverse`.
- Exact constructors and sequencing consumers expose typed bounded failure paths; no unchecked Cartesian helper is reviewed.

D-061 is `Accepted`. The complete compiler, archive, Haddock, hosted CI, and immutable-revision gates passed before approval.

## Package review

The following package groups have public-module snapshots:

- root exact structure: `Markovian`
- floating finite models: `markovian-numerical`
- sampled execution: `markovian-sampling`
- tabular learning: `markovian-learning`
- dense exact execution: `markovian-dense-exact`
- exact benchmark fixtures: `markovian-exact-benchmarks`
- continuous, reverse, autodiff, tensor, SafeTensors, neural, bridge, and GPU packages

The bounded `markovian-tensor-reverse` package and pinned metadata-free F64 `markovian-safetensors` profile passed the complete archive, compiler, hosted, and release gates. D-067 and D-073 are `Accepted` for those bounded scopes.

D-074 is `Accepted` for the owned dynamic CUDA 13 matrix/VJP fragment. The hosted compile-only, archive-enabled, protected UUID-bound hardware, and four-tool Compute Sanitizer gates passed before approval.

## Version and bounds review

All current packages use version `2026.9.3.0`. Public sibling dependencies use `^>=2026.9.3.0`.

The declared `base` bound is `>=4.17.2.1 && <4.20`. The final supported-compiler and lower-bound matrix must confirm it.

A later incompatible public change must increment the PVP major pair.

## Completed final review

Before publication approval, the release process completed these steps:

1. Completed D-061 through D-076 acceptance gates.
2. Re-reviewed the checked 16-package public dependency graph and every test-only integration edge.
3. Obtained complete warning-free Haddock evidence.
4. Ran each supported compiler and lower-bound job.
5. Ran archive-only consumers and package boundaries.
6. Added immutable revision metadata.
7. Recorded human publication approval separately.
