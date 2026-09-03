# markovian-tensor

`markovian-tensor` is a bounded, host-only F64 tensor runtime. It provides
managed immutable buffers, type-indexed shapes, checked contiguous and
transpose layouts, deterministic single-threaded CPU primitives, explicit
semantic owner keys, and a closed public tape API backed by a private atomic allocator capability.

A scalar tensor has shape `[]` and one element. A zero dimension gives zero
elements. Numerical operations accept `FiniteTensor` and reject NaN or
infinity. Rank and dimensions are checked before capped element and byte products. Every payload allocation and scalar-work charge is checked before input materialization and before the first payload allocator call for that operation. Allocations are staged before accounting commits. The session records each staged allocation before initialization. It defers asynchronous interruptions until it attempts all registered cleanup actions. Typed exit exceptions retain action failures, interruptions, and cleanup diagnostics. The session finalizes all committed buffers when the rank-2 session closes. The opaque `DynamicHostTensor` and atomic raw batch constructor let bounded serialization adapters preflight a complete runtime-shape plan without exposing a buffer constructor.

This package does not provide arbitrary strides, broadcasting, mutation,
foreign pointers, sparse layouts, devices, arbitrary-Haskell autodiff, general
tensor semantics, or serialization. The separate `markovian-safetensors`
package implements only the pinned metadata-free F64 profile. The pure and
effect-capable generic reverse interpreters are in `markovian-reverse`. `Markovian.Tensor.Reverse` owns the closed primitive-tape API in this package. The separate `markovian-tensor-reverse` package supplies a bounded host adapter for closed `tanh` and pointwise-multiplication symbols. `Markovian.Tensor.Internal.Reverse` is not exposed. This does not establish
generic tensor or device lowering. D-067 and D-073 are `Accepted` only for
their bounded reverse-execution and SafeTensors scopes.
