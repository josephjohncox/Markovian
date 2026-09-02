# Markovian 0.1.0.0 release notes

Status: **unreleased and blocked**.

This file is a draft. Do not add a release date until D-075 passes.

## Intended scope

The first release will provide a finite exact semantic core. Optional packages will own numerical, continuous, neural, autodiff, tensor, and device execution.

Exact finite APIs use `Rational` and literal equality. Numerical APIs use separate checked floating values and named comparison rules.

Sampled APIs own an explicit generator. Dynamic APIs keep terminal, horizon, reward, successor, observation, lead-time, and discount timing explicit.

## Current blockers

- The D-061 package migration remains unaccepted; its complete acceptance matrix has not passed.
- The checked graph has 16 packages, 18 test suites, and 11 benchmarks. The bounded tensor-reverse and metadata-free F64 SafeTensors packages are present, but their complete archive and hosted gate matrix has not passed.
- Root exposed declarations have complete Haddock coverage in the local release-mode check.
- D-067 now has separate pure and effect-capable reverse interpreters plus a bounded host tensor adapter, but neural `Identity` migration, archive evidence, and the complete gate matrix remain open.
- The focused autodiff suite has a bounded two-layer neural differential for all represented primal, input, weight, and bias coordinates under both tape policies. This is fixture evidence only; it does not establish general neural lowering or release readiness.
- D-073 has focused malformed-corpus, raw-IEEE, canonical-byte, exact-limit, and compile-fail evidence. Archive-only and full release evidence remain open.
- D-074 now has an owned dynamically loaded CUDA 13 driver table, explicit
  missing-library/symbol and unsupported-device behavior, strict ABI assertions,
  deterministic loader/fallback fixtures, and a digest-pinned no-GPU compile
  workflow. Protected four-tool sanitizer, archive, and full hosted evidence
  remain open, so D-074 remains `Proposed`.
- Release preparation now checks exact public sibling edges and the complete test-only integration-edge manifest. It derives exact Haddock rows from each exposed-module golden. It checks all component flags, plan entries, archive dependency closures, and semantic-golden SHA-256 values. It builds in a fresh detached checkout with scrubbed user and Cabal state. Each component receipt binds the compiler and result to a normalized invocation log. The tool regenerates and compares each SPDX document before admission. These mechanisms are not release evidence until the clean immutable-revision workflow passes.
- Local focused checks include warning-free dependency-use builds, exact root tests, generated root-only ownership probes, exact public Haddock coverage, and two identical 16-package sdists. The complete supported-compiler, preferred-oldest, archive-consumer, attestation, and protected hardware gates have not passed on one immutable revision.

## Nonclaims

The release will not claim universal trace, arbitrary feedback, arbitrary continuous disintegration, or arbitrary-Haskell autodiff.

It will not claim equilibrium existence, complete real-equilibrium enumeration, general tensor semantics, or general device correctness.

It will not claim release readiness until D-075 passes.
