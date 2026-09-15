# Markovian 2026.9.15.1 release notes

This coordinated release adds bounded exact solvers and circuit caching,
continuous joint-law operations, polynomial quotation, host tensor views,
CUDA matrix graphs, and resumable DQN training across 16 packages.

## Changes

- Exact CE and CCE solvers return the first checked rational witness within explicit limits. Exhaustion is an error, not a proof that no equilibrium exists.
- Strict-discount affine feedback computes exact value coefficients. Reward JVPs keep probabilities, topology, and discount fixed. Supplied-partition aggregation checks one closed policy.
- Exact continuous operations add joint-affine left-successor substitution and paired-difference moments. Substitution returns the right reward and successor; it does not accumulate temporal rewards.
- Closed polynomial quotation adds lexical sharing and cumulative compilation budgets.
- Host F64 tensors add checked affine views, materialization, and base-coordinate pullbacks.
- CUDA input/view/multiply graphs support explicit sharing and all-input VJPs. Intermediates return to the host between launches; no graph speedup is claimed.
- The reference DQN trainer supports explicit transition fuel, generator state, FIFO replay, target checkpoints, and atomic failed updates.
- Acyclic exact circuits can retain local tables and replay source validation with separate executor limits.
- Executable book examples cover the new APIs and demonstrate that equal denotations can have different resource admission.

## Upgrade requirements

This release uses UTC CalVer `2026.9.15.1` and tag `v2026.9.15.1`.
Calendar versions do not promise PVP compatibility. Sibling dependencies are
pinned to `==2026.9.15.1`; downstream `^>=2026.9.3.0` bounds should be
replaced with an explicitly reviewed release constraint before upgrading.

Use GHC 9.14.1 and Cabal 3.18.1.0. The supported base range is
`>=4.22.0.0 && <4.23`. Update exhaustive `TensorError` matches for the new
`TensorAffineError` constructor. GPU report snapshots gain a profile digest,
and device admission enforces the pinned device constraints.

See [MIGRATION.md](MIGRATION.md) for tensor lifetime requirements and textual
report changes. All packages remain experimental; accepted scopes do not
establish unrestricted feedback, arbitrary-Haskell differentiation, general
equilibrium solving, production training, or cross-device GPU correctness.

## Distribution and verification

The release uses GitHub source artifacts. Hackage publication is separate.
The GitHub release records the exact source commit, preparation and hardware
workflow runs, and artifact verification results.

The preparation bundle contains 16 source archives, 16 SPDX 2.3 SBOMs,
31 component invocation logs, component results, a manifest, source revision,
and SHA-256 checksums. Preparation verifies both the combined archive graph
and each package dependency closure, warning-free installed Haddock,
all 18 test suites and 13 benchmarks, and archived teaching examples.
