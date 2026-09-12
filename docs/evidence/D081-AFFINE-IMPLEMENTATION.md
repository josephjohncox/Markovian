# D-081 affine-view implementation

**Decision status:** Accepted

**Availability:** UNRELEASED

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
scope. D-082 remains Proposed and unimplemented. No package version or released
module membership changes follow from D-081 acceptance.
