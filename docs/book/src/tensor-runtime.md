# Checked host tensor runtime

`markovian-tensor` is an optional host-only F64 execution package. It does not
change the exact semantic core and does not convert numerical results back to
`Rational`.

## Shapes and storage

A shape is a type-level list of dimensions. `SNil` witnesses shape `[]`: one
rank-zero scalar. `SCons` adds a dimension. A zero dimension gives zero
elements.

```haskell
matrixShape = SCons (Proxy @2) (SCons (Proxy @3) SNil)

result <- withTensorSession limits $ \session -> do
  input <- finiteTensorFromList session matrixShape [1, 2, 3, 4, 5, 6]
  -- inspect or execute input here; it cannot escape this region
```

Session admission consumes at most the allowed rank plus one singleton nodes.
It checks rank and each dimension before it evaluates a capped element product,
byte product, or machine-index conversion. Session limits also check maximum
single payload, cumulative fresh payload, buffer count, and scalar work. Counts
use `Natural`. A budget failure occurs before input materialization and before
the operation calls its first payload allocator. This is not an operating-system
memory guarantee: host allocation can still fail after preflight. The opaque
`DynamicHostTensor` and checked batch constructor exist for bounded format
adapters. A batch validates every runtime shape, payload, buffer, work, and
input length before its first allocator call, stages all allocations, and
commits only the complete set.

`HostTensor` preserves raw IEEE values. `finiteTensor` checks the numerical
refinement. Numerical primitives accept only `FiniteTensor` and reject NaN,
infinity, and nonfinite results.

## Layouts

The first layout family is deliberately small:

- contiguous row-major storage;
- an immutable zero-copy transpose view for rank two;
- checked contiguous reshape;
- explicit `contiguousCopy` materialization.

A transpose has the same `StorageId` as its source. A materialized copy has a
new storage ID. A transpose cannot use `reshapeContiguous` because equal
element counts do not establish contiguous coordinate order. There is no
public arbitrary stride, overlap, negative stride, broadcasting, mutation, or
raw-pointer constructor.

## CPU primitives

The deterministic reference loops provide elementwise addition and
multiplication, negation, `tanh`, total reduction, matrix multiplication, and
contiguous copy. Outputs are fresh and contiguous. There is no implicit
broadcasting. Reductions use a fixed left-to-right order; floating
reassociation is not a law.

For `A : m x k` and `B : k x n`, matrix multiplication returns `m x n`. A zero
inner dimension returns a zero matrix. The matrix kernels split and zip bounded
rows sequentially; they do not use list indexing hidden behind the arithmetic
work report. The test suite compares each primitive with independent formulas
and checks transpose views.

## Ownership is not aliasing

`TensorOwner owner shape` is semantic evidence about parameter control.
`StorageId region` identifies a physical host allocation. The following facts
are independent:

- two views can share storage without denoting two owners;
- two owners can reference one immutable buffer and remain independent;
- equal values in different buffers do not create a shared owner;
- gradient accumulation follows a semantic diagonal, not storage identity.

SafeTensors names, a future optimizer, or physical aliasing cannot manufacture
owner evidence.

## Primitive reverse tapes

The reverse module exposes opaque tapes only for its closed primitive set. It
uses the standard coordinate pairing. The implemented rules include

\\[
\bar{x+y}=(\bar z,\bar z),
\qquad
\overline{x\odot y}=(\bar z\odot y,\bar z\odot x),
\\]

\\[
\overline{\tanh x}=\bar y\odot(1-y^2),
\qquad
\overline{\sum_i x_i}=\operatorname{fill}(\bar y),
\\]

and, for `C=AB`,

\\[
\bar A=\bar C B^{\mathsf T},
\qquad
\bar B=A^{\mathsf T}\bar C.
\\]

The matrix and Hadamard pullbacks preflight both output payloads before either
payload allocation. Every represented coordinate for addition, multiplication, total sum, `tanh`,
and matrix multiplication is checked by central finite differences. An
independent directional JVP/VJP pairing also checks pointwise multiplication with step
`1e-6 * max 1 |x|`, absolute tolerance `2e-10`, and relative tolerance
`2e-8`.

The D-081 prerequisite repair adds direct-versus-`contiguousCopy`-first tests for every closed primitive primal and pullback on the existing rank-two transpose view. It checks every logical coordinate by finite differences and includes a rectangular `3 x 2` by `2 x 4` matrix case that detects dimension and stride reversal. Multiplication and matrix pullbacks allocate cotangents distinct from operand and seed storage. Addition deliberately retains its accepted immutable seed-sharing behavior. This is evidence for differentiation with respect to logical view coordinates only. It does not define a pullback into the underlying base tensor or implement signed strides, reversal, slicing, overlap, or broadcasting. D-081 remains `Proposed`.

These operations are reverse derivatives under a finite coordinate pairing.
They are not matrix dagger, Bayesian inversion, payoff pullback, feedback,
strategic duality, or disintegration.

## Optional device fragment

`markovian-gpu` depends on this package and `markovian-tensor-reverse`, but it
does not add a device API to `HostTensor` or a CUDA reverse-program resolver.
It prepares only positive-size F64 matrix multiplication and the
declared matrix-product VJP. Plans bound transfer bytes, scalar work, and user
launches before device admission. Device results are opaque type-indexed finite
host values rather than escaped buffers or exact values.

A CUDA executor selects one device explicitly, owns a private context, module,
and stream, and runs a module-load known-answer test. Calls synchronize before
copy-back. Teardown records cleanup failure. Configured CPU fallback is allowed
only before the first user-kernel launch. The VJP uses
`seed * transpose(right)` and `transpose(left) * seed` under the same coordinate
pairing as the CPU tape.

The committed PTX targets `sm_121`. Successful module admission on one current
device is not evidence for other devices. The effect interpreter does not add
a CUDA resolver, so this fragment still does not lower generic reverse
programs.

## Explicit boundaries

The host package does not provide general tensor semantics, a device buffer
API, BLAS, sparse storage, arbitrary-Haskell autodiff, differentiation through
sampling, optimizer mutation, cross-device reproducibility, or a speed claim.
The single-threaded benchmark is reproducibility evidence, not a competitive
performance result. The optional GPU package adds only the bounded fragment
above; it does not make these stronger claims.

The generic owned reverse foundation lives in `markovian-reverse`. The pure
`Either` API remains separate. `Markovian.Reverse.Program.Effect` adds bounded
execution where forward, recomputation, pullback, and cotangent addition cross
explicit `m (Either error value)` boundaries. Preparation is pure and closes
all primitive symbols.

`markovian-tensor` owns the public `Markovian.Tensor.Reverse` closed primitive-tape API. Its allocator capability remains private inside that package. `markovian-tensor-reverse` supplies a rank-2 host executor for only F64 `tanh` and pointwise multiplication. Its tapes and tensors cannot escape the session.
The tensor allocator preflights all outputs, stages allocations before commit,
rolls back a partial set, and explicitly closes committed allocations after
success, `Left`, or exceptions. This evidence does not establish arbitrary
tensor lowering, CUDA lowering, or release readiness. D-067 remains
`Proposed` until its full gates pass.

## Bounded SafeTensors profile

`markovian-safetensors` implements a separate metadata-free F64 profile pinned
to SafeTensors revision `6eb4dc9a28ebce297606e0f4836bbf28839cacef`.
The decoder retains duplicate JSON members until validation. It bounds and
checks the complete header, UTF-8 names, tensor and rank counts, dimensions,
shape products, offsets, exact payload coverage, and the complete allocation
plan before it allocates any decoded tensor. Holes, overlap, truncation,
trailing bytes, duplicate names or descriptor fields, metadata, and every
non-F64 dtype are explicit failures.

The canonical encoder sorts names by validated UTF-8 bytes. It uses fixed
`dtype`, `shape`, `data_offsets` field order, compact JSON, space padding to an
eight-byte boundary, and contiguous offsets. It reads a view in logical
row-major order, so a transpose is materialized in canonical coordinate order.
Raw IEEE words are retained, including NaN payloads, infinities, signed zero,
and subnormals. `finiteTensor` remains a separate refinement.

A file name does not create `TensorOwner` evidence. The profile does not
serialize storage IDs, layouts, tapes, callbacks, executors, streams, or
pointers. It does not support metadata, F16, BF16, F32, integer dtypes, sparse
storage, arbitrary strides, mapped zero-copy input, or devices. The malformed
corpus and canonical golden are evidence for this bounded profile, not a claim
of general SafeTensors interoperability. D-073 is `Accepted` only for this
profile after the compiler, archive, documentation, hosted, and release gates
passed.
