# D-081 transpose reverse-equivalence evidence

**Decision status:** Proposed

This record covers the existing rank-two transpose fragment only. It does not add the proposed affine-view API.

## Test boundary

`packages/markovian-tensor/test/Main.hs` defines `viewReverseEquivalenceTests`.

The test creates zero-copy transpose views with known logical coordinate orders. It then creates reference tensors with `contiguousCopy`.

The test compares direct-view and materialize-first execution for these closed tapes:

- addition;
- pointwise multiplication;
- matrix multiplication;
- `tanh`;
- total sum.

Each comparison covers the primal output and the complete pullback output. Exact list equality checks both execution paths.

Independent central finite differences check every logical input coordinate. The test uses the existing host-F64 comparison policy.

A square fixture covers every closed tape. A rectangular matrix fixture covers a `3 x 2` view multiplied by a `2 x 4` view.

The rectangular fixture detects dimension and stride reversal. It checks all six left coordinates and all eight right coordinates.

Pointwise multiplication and matrix multiplication return fresh cotangents. Tests reject cotangents that reuse operand or seed storage.

The addition tape keeps its accepted immutable seed-sharing behavior. The evidence does not describe this result as a fresh cotangent.

## Interpretation

The finite differences treat each logical view coordinate as an independent variable. They do not differentiate the transpose constructor.

The tests do not define a pullback from view coordinates into base-storage coordinates. No transpose operation exists in the closed tape language.

`tensorToList` gathers logical values into a Haskell list. The direct path does not call `contiguousCopy`, but it still performs this logical gather.

This evidence does not cover signed strides, reversal, slicing, offsets, overlap, broadcasting, mutation, devices, or arbitrary tensor graphs.

## Verification

Run:

```text
fourmolu --mode check packages/markovian-tensor/test/Main.hs
hlint packages/markovian-tensor/test/Main.hs
cabal test markovian-tensor-test --test-show-details=direct
```

The wider D-081 contract still needs frozen signatures, limits, failure precedence, compile-fail boundaries, and allocation evidence. D-081 remains `Proposed`.
