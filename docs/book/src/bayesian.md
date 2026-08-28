# Bayesian inference

Bayesian inversion requires a prior. A likelihood matrix alone cannot define a reverse stochastic channel.

Read [Measure theory and the finite specialization](measure-theory-primer.md#conditioning-and-disintegration) for conditioning and null events, then [Categorical probability: the bridge](categorical-probability.md#bayesian-inversion-decorates-reversal-with-a-state) for the prior-indexed categorical equation. Primary sources are linked through [Cho and Jacobs](references.md#cho-and-jacobs-disintegration-and-bayesian-inversion), [Fritz](references.md#fritz-markov-categories), and [Faden](references.md#faden-regular-conditional-probabilities).

## Exact priors

A prior is a normalized distribution over an explicit nonempty finite object:

```haskell
sourcePrior <-
  prior sourceObject
    [ (Healthy, 99 / 100)
    , (Sick,     1 / 100)
    ]
```

The API supports prior mass lookup, positive support restriction, and conversion to exact finite distributions.

## Pushforward and evidence

For a prior `p` and channel `K`, pushforward computes:

\\[
q(y)=\sum_x p(x)K(x,y).
\\]

```haskell
outputPrior <- pushforward sourcePrior channel
evidence    <- observationEvidence sourcePrior channel observed
```

## Conditioning

```haskell
posterior <- condition sourcePrior channel observed
```

For positive evidence:

\\[
p(x\mid y)=\frac{p(x)K(x,y)}{q(y)}.
\\]

The function returns a structured error for an observation outside the target or an observation with zero evidence.

## Diagnostic example

Assume:

\\[
P(\mathit{Sick})=\frac1{100},
\quad
P(+\mid\mathit{Sick})=\frac9{10},
\quad
P(+\mid\mathit{Healthy})=\frac1{20}.
\\]

Then:

\\[
P(\mathit{Sick}\mid+)=
\frac{(1/100)(9/10)}{(1/100)(9/10)+(99/100)(1/20)}
=\frac2{13}.
\\]

Exact arithmetic preserves this result without a floating tolerance.

## Support-restricted inversion

```haskell
inverse <- bayesianInverse sourcePrior channel
reverseChannel = inverseMatrix inverse
```

The inverse maps positive output support to positive input support. It does not invent rows for zero-evidence observations.

The value also records the restricted forward channel and both supported priors. These values state the domain on which the Bayesian equations hold.

## Almost-sure equality

Two channels can differ on a source row with zero prior mass. `almostSureEqual` compares them under one explicit prior.

This is not ordinary `Eq`. The prior is part of the equivalence statement.

## Bayesian channels

A `BayesianChannel` packages a channel with its input and output priors. Composition checks that the middle prior flows correctly.

The type has no plain `Category` or dagger instance. Composition without prior compatibility would be unsound.

## Further reading

- [Cho and Jacobs: disintegration and Bayesian inversion](references.md#cho-and-jacobs-disintegration-and-bayesian-inversion)
- [Fritz: Markov categories](references.md#fritz-markov-categories)
