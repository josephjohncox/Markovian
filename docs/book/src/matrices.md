# Finite objects and semiring matrices

## Finite layouts

A `FiniteSet` stores a duplicate-free support and an explicit order. The order defines row and column indexes.

```haskell
colors <- finiteSet [Red, Green, Blue]
```

These layouts have the same support but different storage meanings:

```text
[Red, Green, Blue]
[Blue, Green, Red]
```

Use `sameFiniteSet` for support equality. Use `sameFiniteLayout` before indexed execution.

## Semiring matrices

A matrix has explicit source and target witnesses:

\[
M : X \rightarrow Y.
\]

Its entries use a `Semiring` scalar. Composition is:

\[
(N\circ M)(x,z)=\sum_y M(x,y)N(y,z).
\]

Different semirings give this equation different meanings. Nonnegative rationals compute exact weighted paths. A caller can add lawful Boolean, counting, tropical, or symbolic scalar instances.

The repository supplies `NonNegativeRational`. It does not claim that every semiring supports probability normalization.

## Matrix operations

The raw matrix layer supports:

- Checked construction and indexing.
- Identity and composition.
- Tensor product.
- Direct sum.
- Source and target reindexing.
- Transpose and conjugate transpose.
- Compact cups and caps.
- Finite categorical trace.

Tensor and direct sum are different operations. Tensor combines independent dimensions. Direct sum creates a block-diagonal choice of components.

## Dagger

For an involutive semiring:

\[
(M^\dagger)(y,x)=\overline{M(x,y)}.
\]

The dagger reverses composition:

\[
(N\circ M)^\dagger=M^\dagger\circ N^\dagger.
\]

Use this operation for raw algebraic reversal and diagrammatic calculations. Do not use it as Bayesian inversion.

## Compact structure

`cupMatrix` creates the equality-correlated tensor state. `capMatrix` tests equality across a tensor pair.

These raw matrices satisfy the finite snake equations. They do not generally define normalized stochastic states.

## Trace

For `F : (X,H) -> (Y,H)`:

\[
\operatorname{Tr}_H(F)(x,y)=\sum_h F((x,h),(y,h)).
\]

Use the trace to contract a finite internal index. Do not interpret it as a general stochastic feedback solver.
