# Probability values, distributions, and kernels

For the mathematical background, read [Measure theory and the finite specialization](measure-theory-primer.md) and [Categorical probability: the bridge](categorical-probability.md). The former explains kernels as measurable families of measures; the latter explains why their composition is categorical.

## Floating and exact domains

The package keeps floating and exact probability domains separate.

Use `Markovian.Probability` for sampled execution and learning. Its constructors reject nonfinite values, negative weights, and invalid normalization.

Use `Markovian.Probability.Exact` for reference semantics and law tests. It stores literal `Rational` values.

A floating result cannot prove an exact probability law. An exact result does not imply efficient large-scale execution.

## Finite distributions

A finite distribution has nonempty support and total mass one. `exactFiniteDist` validates each input weight, removes zero-mass entries, and preserves positive labeled duplicates in input order. It rejects more than 4096 raw entries after inspecting only entry 4097, so an infinite input spine is rejected without complete traversal.

```haskell
weather <-
  exactFiniteDist
    [ (Dry, 3 / 4)
    , (Wet, 1 / 4)
    ]
```

Validation occurs before normalization. A positive duplicate cannot hide a negative entry.

`ExactFiniteDist` has safe `Functor`, `Foldable`, and `Traversable` instances. These operations preserve the finite support spine and validated masses. It has no `Applicative` or `Monad` instance, and there is no unchecked bind helper.

Use checked sequencing with explicit limits:

```haskell
limits <- exactBindLimits 4096 8320 13 13
(result, report) <-
  bindExactFiniteDistChecked limits outer (Right . continuation)
```

One bind operation charges outer-support traversal, continuation calls, inner-support traversal, and mass multiplications. It checks every resulting rational product. Result-support, work, numerator-bit, denominator-bit, or continuation failure returns neither a partial distribution nor a report. Labeled duplicates and deterministic support order are retained on success.

Identity and associativity tests apply only when every compared computation is admitted. Resource admission and reports can depend on association, so these tests do not establish a `Monad` interface.

## Kernels

A kernel maps one input to one distribution:

\\[
K : X \rightarrow \mathcal{D}(Y).
\\]

```haskell
drySensor <- exactFiniteDist [(Clear, 9 / 10), (Alarm, 1 / 10)]
wetSensor <- exactFiniteDist [(Clear, 1 / 5), (Alarm, 4 / 5)]
let sensor = exactKernel $ \surface ->
      case surface of
        Dry -> drySensor
        Wet -> wetSensor
```

Checked Kleisli-style composition integrates over the intermediate value:

\\[
(L \mathbin{>=>} K)(x)(z)=\sum_y K(x)(y)L(y)(z).
\\]

`ExactKernel` has an explicit failure channel. `composeExactKernel` requires `ExactBindLimits`; no unrestricted `Category`, `Arrow`, or `ArrowChoice` instance is available. The kernel is one stochastic layer. It is not a recursive transition tree.

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
