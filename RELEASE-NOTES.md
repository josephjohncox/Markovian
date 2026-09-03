# Markovian 2026.9.3.0 release notes

Status: **released 2026-09-03**.

This release uses coordinated UTC CalVer `2026.9.3.0`. Its Git tag is `v2026.9.3.0`.

## Scope

This release provides a finite exact semantic core. Optional packages own numerical, continuous, neural, autodiff, tensor, serialization, and device execution.

Exact finite APIs use `Rational` and literal equality. Numerical APIs use separate checked floating values and named comparison rules.

Sampled APIs own an explicit generator. Dynamic APIs keep terminal, horizon, reward, successor, observation, lead-time, and discount timing explicit.

The checked graph contains 16 packages, 18 test suites, and 11 benchmarks. Release preparation verifies deterministic source archives, isolated archive dependency closures, exact public Haddock coverage, SPDX 2.3 SBOMs, checksums, a fresh exact consumer, and GitHub-hosted SLSA provenance.

## Release evidence

- The complete GHC 9.4.8, GHC 9.8.4, lower-bound, source, documentation, archive, and benchmark matrix passed.
- The digest-pinned CUDA 13.0.2 compile-only workflow reproduced the checked PTX and passed enabled missing-driver and missing-symbol fixtures without GPU access.
- The UUID-bound protected NVIDIA GB10 workflow passed enabled matrix and VJP differentials, lifecycle fault fixtures, PTX reproduction, the transfer-inclusive benchmark, and Compute Sanitizer `memcheck`, `initcheck`, `racecheck`, and `synccheck`.
- Release preparation produced 16 source archives, 16 SPDX 2.3 SBOMs, 29 component logs, complete SHA-256 checksums, and a cryptographically verified 37-subject SLSA provenance statement.
- D-061 through D-076 are accepted only for their stated bounded scopes.

## Nonclaims

The release will not claim universal trace, arbitrary feedback, arbitrary continuous disintegration, or arbitrary-Haskell autodiff.

It will not claim equilibrium existence, complete real-equilibrium enumeration, general tensor semantics, or general device correctness.

It does not claim general device correctness, GPU advantage, or cross-device reproducibility.
