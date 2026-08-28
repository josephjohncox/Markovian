# Probability values, distributions, and kernels

For the mathematical background, read [Measure theory and the finite specialization](measure-theory-primer.md) and [Categorical probability: the bridge](categorical-probability.md). The former explains kernels as measurable families of measures; the latter explains why their composition is categorical.

## Floating and exact domains

The package keeps floating and exact probability domains separate.

Use `Markovian.Probability` for sampled execution and learning. Its constructors reject nonfinite values, negative weights, and invalid normalization.

Use `Markovian.Probability.Exact` for reference semantics and law tests. It stores literal `Rational` values.

A floating result cannot prove an exact probability law. An exact result does not imply efficient large-scale execution.

## Finite distributions

A finite distribution has nonempty support and total mass one. The constructor validates each input weight before it combines duplicate values.

```haskell
weather <-
  exactFiniteDist
    [ (Dry, 3 / 4)
    , (Wet, 1 / 4)
    ]
```

This order of validation matters. A positive duplicate cannot hide a negative entry.

`ExactFiniteDist` has `Functor`, `Applicative`, and `Monad` instances. The exact finite domain supports literal law tests.

## Kernels

A kernel maps one input to one distribution:

\\[
K : X \rightarrow \mathcal{D}(Y).
\\]

```haskell
sensor = exactKernel $ \surface ->
  case surface of
    Dry -> exactFiniteDist [(Clear, 9 / 10), (Alarm, 1 / 10)]
    Wet -> exactFiniteDist [(Clear, 1 / 5),  (Alarm, 4 / 5)]
```

Kleisli composition integrates over the intermediate value:

\\[
(L \mathbin{>=>} K)(x)(z)=\sum_y K(x)(y)L(y)(z).
\\]

The kernel is one stochastic layer. It is not a recursive transition tree.

## Deterministic kernels

A Dirac distribution lifts a deterministic function:

```haskell
exactKernel (exactDirac . normalizeInput)
```

Use deterministic matrix or circuit refinements when you also need proof-carrying copy laws.

## Empty and nonempty finite witnesses

`FiniteSet` permits an empty carrier. Raw matrices therefore support empty boundaries.

`FiniteObject` proves that a carrier is nonempty. Probability-bearing states, priors, and distributions use this stronger witness.

Support equality ignores order. Layout equality compares both support and represented order:

```haskell
sameFiniteSupport left right
sameFiniteLayout left right
```

Use layout equality before indexed or dense execution.

## Further reading

- [Giry: probability as a monad](references.md#giry-probability-as-a-monad)
- [Moggi: Kleisli semantics](references.md#moggi-kleisli-semantics)
- [Fritz: Markov categories](references.md#fritz-markov-categories)
