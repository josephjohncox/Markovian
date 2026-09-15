# Migration guide

This guide covers changes since `2026.9.3.0`, followed by the original package migration.

## Upgrading from 2026.9.3.0

The new release is `2026.9.15.1`. Calendar versions identify releases without
promising PVP compatibility. Pin the coordinated package set with
`==2026.9.15.1`. An old `^>=2026.9.3.0` constraint admits this release even
though the tensor error datatype changes; review and tighten downstream bounds.

Use GHC 9.14.1 and Cabal 3.18.1.0. Packages now require
`base >=4.22.0.0 && <4.23`; packages using `bytestring` require
`bytestring >=0.12.2.0 && <0.13`. The earlier GHC 9.4.8 and 9.8.4
project builds are no longer supported.

`TensorError(..)` adds `TensorAffineError !AffineProblem`. Update exhaustive
error matches to handle it. The existing seven-argument `tensorSessionLimits`
constructor keeps affine operations disabled until explicitly enabled, but its
derived `Show` output now includes `limitAffine = Nothing`.

Complete tensor operations inside the session callback. Join or cancel and join
dependent child threads before returning, including on errors and exceptions.
Region indices do not prevent escaping IO closures or existentially packaged
tensors. Return ordinary copied data after its reads have completed.

`renderDevicePlanReport` adds a `profile-sha256:` line. Update text snapshots
and report parsers. CUDA device admission now checks the pinned device
constraints, including a minimum of 128 threads per block; unsupported-device
diagnostics identify that profile's digest.

The release adds seven public modules and preserves the existing public
function signatures. New APIs retain explicit resource limits and bounded
contracts; see [the release notes](RELEASE-NOTES.md).

## D-061 package moves

The accepted D-061 migration established these package boundaries for the first release. Add the package that owns each moved module.

| Module group | Package |
| --- | --- |
| `Markovian.Action` | `Markovian` |
| `Markovian.Probability`, `Reward`, `Objective`, `Kernel`, `MRP`, `MDP`, and `Policy` | `markovian-numerical` |
| `Markovian.Circuit.Interpret.Approximate` | `markovian-numerical` |
| `Markovian.Sampling` and `Markovian.Interpreter.Sampled.*` | `markovian-sampling` |
| `Markovian.Learning.*` | `markovian-learning` |
| `Markovian.Backend.CPU.Exact` | `markovian-dense-exact` |
| `Markovian.Benchmark.Inventory.*` | `markovian-exact-benchmarks` |

Import `ActionId`, `actionId`, and `actionValue` from `Markovian.Action`. `Markovian.MDP` does not re-export them.

The root `Markovian` library now depends only on `base`. Its public modules contain finite exact and exact-neutral structural semantics.

## Removed APIs

This first release contains the corrected API and has no compatibility shims for pre-release interfaces.

- `Markovian.Category.Finite.Exact` is no longer public.
- `lowerExactIR` is removed.
- `ExactFiniteDist` has no `Applicative` or `Monad` instance. Its `Functor`, `Foldable`, and `Traversable` instances preserve the bounded support and validated masses.
- `bindExactFiniteDist` is removed; no unchecked sequencing helper replaces it.
- Construct `ExactBindLimits` with `exactBindLimits`, then use `bindExactFiniteDistChecked`. Every operation has explicit result-support, work, numerator-bit, and denominator-bit limits. Work charges outer and inner support traversal, continuation calls, and mass multiplications. Failure is atomic and returns neither a partial distribution nor a report.
- The checked bind preserves positive labeled duplicates and deterministic support order. Exact identity and associativity apply only when every compared operation is admitted; admission and reports can depend on association.
- `exactFiniteDist` rejects raw supports above 4096 entries after inspecting at most entry 4097. It now uses bounded default work and rational-size limits. Use `exactDistributionLimits` and `exactFiniteDistChecked` for explicit support, work, numerator-bit, and denominator-bit limits and a deterministic constructor report.
- `canonicalExactDistribution` bounds the raw spine before validation or duplicate aggregation and meters label comparisons and duplicate additions. Infinite duplicate input terminates at the raw support limit.
- `exactTraceDistributionChecked` and `exactTraceDistributionFromChecked` take explicit bind limits. Policy closure, Bayesian pushforward and conditioning, compiled policy closure, trace enumeration, and POMDP filtering now return typed sequencing failures instead of building unchecked Cartesian products.
- The same global maximum caps admitted bind output.
- `ExactKernel` has no `Category`, `Arrow`, or `ArrowChoice` instance.
- Use `composeExactKernel` with explicit checked-bind limits.
- `runExactKernel` and exact policy kernels have an explicit failure result.

The private legacy finite IR exists only in the `markovian-dense-exact` regression sublibrary. Public dense execution starts from exact circuits.

## Continuous accounting limits

`ExactLimits` now has `limitRawExpansionTerms` and `limitCanonicalExpansionTerms`. Set both fields when you construct exact continuous limits.

Bivariate integration uses the raw limit as a cumulative Cartesian-pair limit. It uses the canonical limit for the largest live canonical expansion. `RawExpansionTermLimitExceeded` and `CanonicalExpansionTermLimitExceeded` identify these failures.

`ExactIntegralReport` now exposes symbolic, moment, summation, and rational-size counters. Existing result, degree, input-term, owner, and work accessors remain available. The bivariate operation returns no report after any limit failure.

## Other package moves

The pure D-067 foundation moved from `Markovian.Backend.Neural.Reverse.Program` to `Markovian.Reverse.Program`. Add `markovian-reverse` directly.

Effect-capable execution is exposed by `Markovian.Reverse.Program.Effect`; keep pure callers on `Markovian.Reverse.Program`. `Markovian.Tensor.Reverse` is owned by `markovian-tensor`; add that package for closed primitive tapes. `Markovian.Tensor.Reverse.Program` remains in `markovian-tensor-reverse` as the bounded host adapter. The tensor allocator capability is private and preserves atomic staged multi-output allocation. The neural package now exposes `denseReverseCircuit` as an explicit adapter to `markovian-reverse`, and the orphan neural reverse re-export wrappers are removed. Generic tensor and CUDA reverse-program lowering remain unsupported. D-067 is accepted only for the bounded effect and host-adapter scope.

## SafeTensors profile

Add `markovian-safetensors` to parse or emit the pinned metadata-free F64 profile. Construct limits with `safeTensorLimits`, keep decoding inside `withTensorSession`, and use `finiteTensor` separately when finite values are required. The decoder now rejects metadata, non-F64 dtypes, duplicate names or descriptor fields, malformed UTF-8 or JSON, invalid shapes and products, and noncontiguous payload coverage. This is a bounded API, not a compatibility surface for arbitrary SafeTensors files. D-073 is accepted only for the pinned metadata-free F64 profile.

## Release status

D-061 is `Accepted`. The source split passed the full compiler, archive, hosted CI, and release gates before publication.

Do not treat a modified checkout or a preparation bundle as publication authorization. Publication requires a separate human request.
