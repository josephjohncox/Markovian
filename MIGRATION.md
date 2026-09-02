# Migration guide

Markovian has no published release. This guide applies to repository snapshots only.

## D-061 package moves

The unaccepted D-061 migration changes package ownership in this worktree. Add the package that currently owns each moved module; this table does not claim release acceptance.

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

The project is unreleased, so the correction has no compatibility shims.

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

Effect-capable execution is exposed by `Markovian.Reverse.Program.Effect`; keep pure callers on `Markovian.Reverse.Program`. `Markovian.Tensor.Reverse` is owned by `markovian-tensor`; add that package for closed primitive tapes. `Markovian.Tensor.Reverse.Program` remains in `markovian-tensor-reverse` as the bounded host adapter. The tensor allocator capability is private and preserves atomic staged multi-output allocation. The neural package now exposes `denseReverseCircuit` as an explicit adapter to `markovian-reverse`, and the orphan neural reverse re-export wrappers are removed. Generic tensor and CUDA reverse-program lowering remain unsupported, and D-067 remains `Proposed` until all gates pass.

## SafeTensors profile

Add `markovian-safetensors` to parse or emit the pinned metadata-free F64 profile. Construct limits with `safeTensorLimits`, keep decoding inside `withTensorSession`, and use `finiteTensor` separately when finite values are required. The decoder now rejects metadata, non-F64 dtypes, duplicate names or descriptor fields, malformed UTF-8 or JSON, invalid shapes and products, and noncontiguous payload coverage. This is a new bounded API, not a compatibility surface for arbitrary SafeTensors files. D-073 remains `Proposed` until all gates pass.

## Release status

D-061 remains `Proposed`. The source split and focused boundaries do not satisfy the full compiler, archive, hosted CI, and release gates.

Do not treat this worktree as a release candidate. Publication requires a separate human authorization.
