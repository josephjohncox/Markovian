# Changelog

## Unreleased

- Add the proposed D-077 canonical CUDA profile and typed same-session receipt validation. Retain profile, PTX, executable, command, outcome, native observation, sanitizer, and ordered benchmark evidence. D-077 remains Proposed. Separate governance review remains open.
- Record the [2026-09-08 user amendment](../../docs/WORKFLOWS.md#gpu-deployment-evidence) permitting finite evidence retention, including the current 90-day period. GPU deployment or promotion requires complete raw evidence and signature verification bound to its deployed revision. Keep compact verification and expiry records. Later expiry does not invalidate past verification, but missing raw data cannot support new verification or promotion.
- Fix sanitizer-version collection after protected run 34172607126 stopped before hardware tests. Use the same anchored Version-line parser for collection and receipt validation; reject partial or ambiguous observations. Add real-banner and malformed-input regression tests without changing the profile or public interfaces.

- Correct racecheck's success marker after run 34175882779 failed receipt validation. Require zero hazards, errors and warnings in its tool-specific summary; retain the other sanitizer markers and exit/binding checks. Regenerate the profile-bound outputs and plan golden, and test against independently recorded summaries. This creates a new profile identity, not a repaired historical receipt or hardware evidence.
- Rename the benchmark statistic label to `standard deviation (sample)` so it cannot be counted as a raw sample. Preserve the statistic, all 20 raw samples, and strict receipt validation; add producer-drift and rehashed-log regressions.

- Record successful same-session run 34181389307 at source `3e850085fa96c4e48a80270b9e49e9f55fe0f757`: six validated records, 20 raw samples and 14 cryptographically verified subjects. Disclose separate executable-mode reconstruction, expiring storage and absent environment protections. No release or D-077 acceptance follows.

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
