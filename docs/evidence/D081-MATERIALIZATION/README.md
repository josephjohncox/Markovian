# Affine resource derivations and fixtures

These documents support the logical resource model in the
[D-081 materialization addendum](../../plans/D081-MATERIALIZATION-ADDENDUM.md).
See the [implementation record](../D081-AFFINE-IMPLEMENTATION.md) for current
scope and runnable checks.

- [Source proof](SOURCE-PROOF.md): reconciled schedules, arithmetic, and
  simultaneous-storage bounds. Its substitutions take precedence over the
  preliminary [prefix](advisories/d081-prefix-proof.md),
  [geometry](advisories/d081-geometry-proof.md), and
  [runtime](advisories/d081-runtime-proof.md) derivations.
- [Operation fixtures](FIXTURES-OPERATIVE.md) and [inputs](FIXTURE-INPUTS.md):
  expected cells, work, and peak values, including failure-state interpretation.
- [Prefix failure fixtures](FAILURE-FIXTURES-FINAL.md): count, rank, and sentinel
  cases with their expected diagnostics.

The tables specify expected results. Current execution comes from the Haskell
fixtures in `packages/markovian-tensor/test`, not from archived JSON reports.
The derivations use logical slots and elementary source operations; they do
not measure GHC allocation, garbage collection, or physical reclamation.

## Source basis

The original derivations use line references `I` and `S` for historical
`Internal.hs` and `Shape.hs` snapshots. Those line numbers do not identify
current source. The original Internal snapshot had SHA-256
`417337a68c1e660ffe537ab66d52062a156c0917f27c8140ae40edb38dc1c59c`;
the r4 candidate had
`8b0b50628bf10e260f6048e2fe96dcc10359de31c7958591fecfbc6aef98dee5`.
These identities distinguish the derivation's source from later implementation
revisions; they do not make the old snapshots executable from this archive.
`OPERATIVE-AMENDMENT.md` in the derivations refers to the materialization
addendum linked above.

The original campaign records remain in Git history at `ed9a352`.
Changes to implementation schedules still require checking these derivations
against the affected source and running the corresponding regression tests.
