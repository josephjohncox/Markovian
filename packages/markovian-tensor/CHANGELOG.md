# Changelog

## 2026.9.3.0 — Unreleased — Unreleased

- Add bounded region-scoped host F64 buffers and typed shapes.
- Add contiguous, transpose-view, checked reshape, and materialization APIs.
- Add deterministic finite CPU primitives and closed primitive VJPs.
- Keep semantic owner keys separate from physical storage IDs.
- Add deterministic payload/work reports, differential tests, boundaries, and benchmarks.
- Check rank and dimensions before capped products, preflight payloads before input materialization, and replace list-index matrix kernels with sequential bounded kernels.
- Add all-coordinate add, multiply, sum, tanh, and matrix VJP evidence, independent pairing, and a committed report golden.
- Stage multi-output allocations before commit, roll back partial sets explicitly, close committed buffers deterministically, and add private allocation and cleanup fault injection.
- Add opaque dynamically shaped F64 tensors and atomic checked batch construction for bounded serialization adapters; this does not add arbitrary layouts, owners, or serialization to the tensor package.
