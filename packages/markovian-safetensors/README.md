# markovian-safetensors

`markovian-safetensors` implements one bounded SafeTensors profile. The format source is pinned to revision `6eb4dc9a28ebce297606e0f4836bbf28839cacef`. The supported fragment contains metadata-free F64 tensors only.

The decoder preserves duplicate JSON members until validation. It rejects duplicate tensor names and duplicate descriptor fields. Before it allocates a tensor, it validates:

- file and header lengths, including the pinned 100,000,000-byte header maximum;
- JSON syntax and UTF-8 names;
- tensor count and name length;
- the `F64` dtype;
- rank, Word64 wire dimensions, and capped shape products;
- zero-coordinate shapes without coordinate-order dependence;
- offset order and exact byte lengths;
- overlap, holes, truncation, trailing bytes, and total payload size;
- the complete tensor-session allocation plan.

The encoder applies the same Word64 dimension bound before it creates bytes. The encoder sorts names by validated UTF-8 bytes. It emits compact JSON with field order `dtype`, `shape`, `data_offsets`, pads the header with spaces to an eight-byte boundary, uses contiguous offsets, and writes little-endian F64 payloads. A transpose view is materialized in logical row-major order. Raw IEEE payloads are retained, including NaN payloads, infinities, signed zero, and subnormals. Use `finiteTensor` separately when finite values are required.

SafeTensors names do not create `TensorOwner` evidence. The file API does not serialize storage IDs, layouts, tapes, callbacks, executors, streams, or pointers. Metadata, F16, BF16, F32, integer dtypes, sparse tensors, arbitrary strides, devices, and general SafeTensors interoperability are outside this profile and produce explicit errors where applicable.

D-073 remains `Proposed` until the full compiler, archive, documentation, and release gates pass.
