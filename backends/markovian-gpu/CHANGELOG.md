# Changelog

## 2026.9.15.1 - 2026-09-15

- Add the D-077 canonical CUDA profile and strict same-session receipt validation, including corrected sanitizer-version parsing, the racecheck zero-hazard marker, and an unambiguous benchmark standard-deviation label.
- Record D-077 as Accepted for the bounded profile, receipt, exact-dyadic reference, and deployment-scoped evidence policy. Deployment still requires complete evidence bound to the deployed revision; no general correctness, portability, or speed claim follows.
- Add `Markovian.Backend.GPU.Graph`, a closed typed F64 multiply-chain graph with lexical sharing, admitted affine views, complete preparation budgets, deterministic gradients for every input, and one scoped executor. Intermediate affine operations remain host-materialized.
- Record D-082 as Accepted for this bounded graph and all-input VJP scope. It does not add generic reverse-program lowering, arbitrary tensor graphs, fusion, or a graph speed claim.
- Target GHC 9.14.1 with `base >=4.22 && <4.23` and align the `markovian-tensor` bound with the coordinated release.

## 2026.9.3.0 — 2026-09-03

- Replace the list-only dense call with bounded typed matrix and matrix-VJP
  plans over `markovian-tensor` inputs. Add the reviewed integration edge to
  `markovian-tensor-reverse` without claiming a generic CUDA resolver.
- Add structured capability probing, explicit device selection, private CUDA
  executor ownership, stream synchronization, checked cleanup, and
  pre-user-launch fallback.
- Add CUDA-disabled, CPU/CUDA differential, boundary, and transfer-inclusive
  benchmark coverage.
- Make executor ownership rank-2 scoped and serialize teardown with execution.
  Preserve launch commitment separately from fallback permission, prohibit
  fallback after cleanup failure, and carry first-launch commitment across the
  two-call VJP transaction.
- Check every kernel index product, use heap-backed FFI transfer buffers, and
  add deterministic allocation, transfer, launch, synchronization, copy-back,
  free, and teardown fault injection for protected validation.
- Bind protected hardware execution to a configured UUID. Add all-coordinate
  finite differences, a plan golden, semantic benchmark checksums, and host and
  compiler metadata.
- Replace direct `libcuda` linkage with an executor-owned `RTLD_NOW | RTLD_LOCAL`
  CUDA 13 driver table. Resolve all required versioned symbols atomically before
  initialization, reject devices outside the pinned `sm_121` profile, unload
  only after resource teardown, and add deterministic missing-library and
  missing-symbol fallback fixtures.
- Add a digest-pinned CUDA 13 compile-only workflow for strict C/Haskell builds,
  driver-header and PTX/header reproduction, and no-GPU loader tests. Pass the
  Cabal data-directory override to protected Compute Sanitizer invocations.
