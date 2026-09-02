# Changelog

## 0.1.0.0 (unreleased)

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
