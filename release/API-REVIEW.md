# Public API review

This review records the current 16-package integration API, 18 test suites, and 11 benchmarks. It does not approve the first release.

The files in `release/exposed-modules` are checked API snapshots. Each public API change needs a new review.

## D-061 boundary

The D-061 package migration remains under review and is not accepted.

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

D-061 remains `Proposed`. The complete compiler, archive, Haddock, hosted CI, and immutable-revision gates have not passed.

## Package review

The following package groups have public-module snapshots:

- root exact structure: `Markovian`
- floating finite models: `markovian-numerical`
- sampled execution: `markovian-sampling`
- tabular learning: `markovian-learning`
- dense exact execution: `markovian-dense-exact`
- exact benchmark fixtures: `markovian-exact-benchmarks`
- continuous, reverse, autodiff, tensor, SafeTensors, neural, bridge, and GPU packages

The bounded `markovian-tensor-reverse` package and pinned metadata-free F64 `markovian-safetensors` profile are present. Their focused boundaries do not satisfy the complete archive, compiler, hosted, and release gates. D-067 and D-073 remain `Proposed`.

D-074 has an owned dynamic CUDA 13 driver table and a pinned compile-only workflow definition. Hosted compile-only, archive-enabled, protected hardware, and four-tool Compute Sanitizer receipts remain open.

## Version and bounds review

All current packages use version `0.1.0.0`. Public sibling dependencies use `^>=0.1.0.0`.

The declared `base` bound is `>=4.17.2.1 && <4.20`. The final supported-compiler and lower-bound matrix must confirm it.

A later incompatible public change must increment the PVP major pair.

## Required final review

Before publication approval:

1. Complete D-061 through D-076 acceptance gates.
2. Re-review the checked 16-package public dependency graph and every test-only integration edge.
3. Get complete warning-free Haddock evidence.
4. Run each supported compiler and lower-bound job.
5. Run archive-only consumers and package boundaries.
6. Add immutable revision metadata.
7. Record human publication approval separately.
