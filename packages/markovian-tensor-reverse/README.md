# markovian-tensor-reverse

This package owns `Markovian.Tensor.Reverse.Program`, which connects part of `markovian-tensor`'s closed host F64 tape API to the bounded effect reverse interpreter. The tensor package keeps its atomic staged allocator capability private; this adapter consumes only the public closed tape operations. `withTensorReverseExecutor` owns one rank-2 tensor session. Tensors, tapes, storage IDs, and executors cannot escape that session.

The program adapter supports `tanh` and pointwise multiplication. The tape API also covers the documented closed tensor primitives. It does not claim arbitrary tensor lowering, automatic differentiation, CUDA execution, or callback serialization.
