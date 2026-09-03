# Markovian 2026.9.3.0 release notes

Status: **release candidate**.

The candidate uses coordinated UTC CalVer `2026.9.3.0`. Its Git tag is `v2026.9.3.0`.

## Scope

This release provides a finite exact semantic core. Optional packages own numerical, continuous, neural, autodiff, tensor, serialization, and device execution.

Exact finite APIs use `Rational` and literal equality. Numerical APIs use separate checked floating values and named comparison rules.

Sampled APIs own an explicit generator. Dynamic APIs keep terminal, horizon, reward, successor, observation, lead-time, and discount timing explicit.

The checked graph contains 16 packages, 18 test suites, and 11 benchmarks. Release preparation verifies deterministic source archives, isolated archive dependency closures, exact public Haddock coverage, SPDX 2.3 SBOMs, checksums, a fresh exact consumer, and GitHub-hosted SLSA provenance.

## Candidate gates

- Run the complete compiler, lower-bound, source, documentation, archive, and attestation gates on the final merged revision.
- Run the pinned CUDA 13 compile-only workflow on the final revision.
- Run the UUID-bound protected CUDA workflow and all four Compute Sanitizer tools before making the bounded enabled-device claim.
- Keep D-061 through D-076 `Proposed` until the applicable final-revision evidence passes.

## Nonclaims

The release will not claim universal trace, arbitrary feedback, arbitrary continuous disintegration, or arbitrary-Haskell autodiff.

It will not claim equilibrium existence, complete real-equilibrium enumeration, general tensor semantics, or general device correctness.

It does not claim general device correctness, GPU advantage, or cross-device reproducibility.
