# D-081 affine-view implementation

**Decision status:** Accepted

**Availability at implementation acceptance:** UNRELEASED

**Publication update — 2026-09-15:** The bounded D-081 implementation entered
the `v2026.9.15.1` source release at
`5d996ec63922aebad57e1bd30a1ca9ebf5f5cfa9`, as recorded in the
[published-release registry](../../release/published-releases.json). The
availability line above records the earlier review, not current publication.

The implementation was accepted at `cc900878dbf6f7bdc33f95affa9c15d2ea6f97ad`
for immutable host-F64 affine views in `markovian-tensor`.

## Supported operations

`Markovian.Tensor.Affine` provides checked signed affine maps, permutation,
reversal, bounded slicing, binding to the original base, materialization, and
base-coordinate pullback. It rejects out-of-bounds and overlapping maps,
including non-singleton zero strides. Region, owner, storage, shape, and map
witnesses retain distinct nominal roles.

The [affine contract](../plans/D081-AFFINE-VIEWS.md) specifies geometry, public
signatures, ownership, and errors. The [materialization addendum](../plans/D081-MATERIALIZATION-ADDENDUM.md)
supersedes its resource, producer, failure, and fixture requirements where
specified. The [resource derivations and fixture tables](D081-MATERIALIZATION/README.md)
explain those bounds.

## Verification

The maintained checks are:

- `packages/markovian-tensor/test/AffineContractTests.hs`: public policy, prefix
  admission, reservation, signed geometry, runtime, and mixed-history cases.
- `packages/markovian-tensor/test-fault/`: private runtime and fault controls,
  including affine materialization, publication, and cleanup.
- `packages/markovian-tensor/scripts/check-tensor-boundary`: constructor, role,
  and other compile-failure boundaries.

Run the package suite and boundary checks from the repository root:

```sh
cabal test markovian-tensor-test markovian-tensor-fault-test --project-file=cabal.project.ci --test-show-details=direct
bash packages/markovian-tensor/scripts/check-tensor-boundary
```

The derivations describe a logical source-resource model. They are not a
physical-allocation theorem. Tests of reports, demand order, and faults do not
measure every intermediate allocation or prove universal asynchronous cleanup.

## Lifetime and exclusions

Payload-dependent work must finish inside the session callback. Callers must
join, or cancel and join, dependent children on every exit. The runner does not
provide automatic joins, universal post-close rejection, close/read
synchronization, or prompt reclamation. Post-finalization use is unsupported.

Arbitrary map composition, broadcasting, mutation, general dtypes, borrowed
pointers, persistent devices, and generic device lowering remain outside this
scope. At this implementation acceptance, D-082 remained Proposed and
unimplemented; D-081 acceptance alone changed no package version or released
module membership. The later `v2026.9.15.1` source release separately included
bounded D-081 views and D-082 CUDA matrix graphs.
