# CPU, GPU, and neural boundaries

The semantic core does not depend on tensors, CUDA, autodiff, or a neural framework.

## Dense exact CPU backend

The CPU backend lowers an exact IR or circuit to a row-major rational matrix:

```haskell
dense <- lowerExactCircuit primitives circuit

denseExactShape dense
denseExactRows dense
result <- runDenseExactKernel dense input
```

The finite source and target layouts define the row and column indexes. The backend performs no random draws.

Use this backend as a reference for denotational differential tests and layout inspection.

## CUDA backend

The optional GPU package applies one row-major `Double` matrix to one source distribution:

```haskell
result <- gpuDenseApply rows columns matrix input
```

The result contains the output vector and transfer-inclusive duration.

The implementation includes context setup, host-to-device transfer, kernel execution, device-to-host transfer, and cleanup in its measurement.

The API reports these failures explicitly:

- CUDA support was not compiled.
- No CUDA device is available.
- Dimensions or buffer lengths are invalid.
- An input value is not finite.
- The CUDA driver returned an error.

CUDA execution is approximate. It does not inherit exact rational circuit laws.

## Neural categorical contract

The neural package converts finite logits to a categorical distribution:

```haskell
categorical <- categoricalFromLogits logits

neuralProbabilities categorical
neuralSoftmaxJacobian categorical
neuralContract categorical
```

The implementation subtracts the largest logit before exponentiation. This prevents avoidable overflow.

The Jacobian uses:

\[
\frac{\partial p_i}{\partial z_j}=p_i(\delta_{ij}-p_j).
\]

The contract states the normalization, gradient meaning, and score-function estimator assumptions.

Compare a neural result with an exact reference:

```haskell
maximumCategoricalError exactMasses approximateMasses
```

The package does not select an autodiff or tensor framework. An adapter must state its precision, observation relation, and error policy.

## Backend admission rule

A new backend must define these items before it can claim compatibility:

1. Define the source semantic object.
2. Define the numeric representation.
3. Define the approximation relation.
4. Define the accepted error bound.
5. Define the random-number meaning, if any.
6. Define the data-transfer boundary.
7. Add differential tests against the exact reference.

A fast result without this contract is only an unrelated computation.
