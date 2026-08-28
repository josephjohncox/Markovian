# Stochastic, deterministic, and convex matrices

The raw matrix layer permits operations that do not preserve probability. Refinement types expose only operations that preserve their proofs.

## Stochastic matrices

A `StochasticMatrix` over nonnegative rational scalars proves that each source row sums to one.

```haskell
channel <- stochasticMatrix rawMatrix
```

The constructor checks nonnegativity through the scalar type and checks exact row normalization.

The stochastic API supports identity, composition, tensor, copy, and discard. It does not expose transpose, dagger, compact structure, trace, or raw addition.

A transpose can fail normalization. For a fair coin channel from `()` to `Bool`, the transpose has two source rows of mass `1/2`.

## Deterministic matrices

A `DeterministicMatrix` proves that each source row is one-hot:

```haskell
negation <-
  deterministicFromFunction bits bits not
```

A deterministic matrix embeds into the stochastic refinement:

```haskell
asChannel = embedDeterministic negation
```

The proof permits copy naturality:

\\[
\mathsf{copy}\circ f=(f\otimes f)\circ\mathsf{copy}.
\\]

This equation fails for a general stochastic computation. The type prevents the compiler from applying the rewrite there.

## Copy and independent execution

`copyStochastic` copies an existing value. It does not rerun the process that created the value.

Tensor executes two represented channels independently, conditional on their inputs.

This distinction controls correlation. A copied coin has support only on `(False,False)` and `(True,True)`. Two independent coins have four possible pairs.

## Convex mixtures

A convex family contains a nonempty set of weighted stochastic matrices. Its exact coefficients are nonnegative and sum to one.

```haskell
family <-
  convexFamily
    ((safeWeight, safePolicy) :| [(fastWeight, fastPolicy)])

mixedPolicy = convexMixture family
```

The denotation is:

\\[
K=\sum_i \lambda_iK_i,
\qquad
\lambda_i\ge0,
\qquad
\sum_i\lambda_i=1.
\\]

Use convex mixtures for mixed policies, controller ensembles, and uncertain environment models.

Do not replace this operation with raw matrix addition. Raw addition does not preserve row normalization.

## Further reading

- [Fritz: Markov categories](references.md#fritz-markov-categories)
- [Fritz: convex spaces](references.md#fritz-convex-spaces)
- [Jacobs: convexity and effects](references.md#jacobs-convexity-and-effects)
