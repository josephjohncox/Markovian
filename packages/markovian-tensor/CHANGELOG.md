# Changelog

## 2026.9.15.1 - 2026-09-15

- Add `Markovian.Tensor.Affine` with checked signed affine maps, a logical-geometry descriptor, permutation, reversal, bounded slicing, materialization, and fresh base-coordinate pullback for immutable host F64 tensors.
- Add cumulative planning and runtime limits, nominal map and owner scopes, exact geometry checks, allocation-fault coverage, and direct-versus-materialized primal and pullback comparisons.
- Record D-081 as Accepted for this bounded affine-view scope. Overlapping maps, broadcasting, mutation, device storage, and general tensor semantics remain outside it.
- Breaking: `TensorError` adds the public `TensorAffineError AffineProblem` constructor, so exhaustive pattern matches must handle the new case.
- Target GHC 9.14.1 with `base >=4.22 && <4.23`.

## 2026.9.3.0 — 2026-09-03

- Add bounded region-scoped host F64 buffers and typed shapes.
- Add contiguous, transpose-view, checked reshape, and materialization APIs.
- Add deterministic finite CPU primitives and closed primitive VJPs.
- Keep semantic owner keys separate from physical storage IDs.
- Add deterministic payload/work reports, differential tests, boundaries, and benchmarks.
- Check rank and dimensions before capped products, preflight payloads before input materialization, and replace list-index matrix kernels with sequential bounded kernels.
- Add all-coordinate add, multiply, sum, tanh, and matrix VJP evidence, independent pairing, and a committed report golden.
- Stage multi-output allocations before commit, roll back partial sets explicitly, close committed buffers deterministically, and add private allocation and cleanup fault injection.
- Add opaque dynamically shaped F64 tensors and atomic checked batch construction for bounded serialization adapters; this does not add arbitrary layouts, owners, or serialization to the tensor package.
