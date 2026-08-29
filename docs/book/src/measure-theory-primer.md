# Measure theory and the finite specialization

Markovian implements finite discrete probability. Measure theory explains which parts generalize to continuous spaces and which conveniences are specific to finite carriers.

This chapter assumes familiarity with sets, functions, countable unions, and elementary probability. It introduces only the measure-theoretic concepts needed to understand kernels, conditioning, and the project's continuous-model boundary.

## Why probability needs measurable structure

On a finite set `X`, every subset can be assigned a probability. On an uncountable set, assigning probabilities consistently to every subset can be impossible. Measure theory therefore chooses a collection of observable subsets.

A **sigma-algebra** `Σ_X` on `X` contains:

- the empty set
- the complement of every member
- every countable union of members

The pair `(X,Σ_X)` is a measurable space. Members of `Σ_X` are measurable events.

For a finite set, Markovian implicitly uses the full power set:

\\[
\Sigma_X=\mathcal{P}(X).
\\]

Every subset is measurable. Every function between finite measurable spaces is measurable. This is one reason finite probability has clean executable semantics.

For a systematic introduction, read [Kallenberg, Chapters 1 and 4](references.md#kallenberg-foundations-of-modern-probability) or [Pollard, Chapters 2 and 5](references.md#pollard-measure-theoretic-probability).

## Measures and probability measures

A measure `μ` assigns a nonnegative extended real value to each measurable event. It satisfies:

\\[
\mu(\varnothing)=0
\\]

and countable additivity for pairwise disjoint events:

\\[
\mu\left(\bigcup_{n=1}^{\infty}A_n\right)=
\sum_{n=1}^{\infty}\mu(A_n).
\\]

A probability measure has total mass one:

\\[
\mu(X)=1.
\\]

On a finite set, a probability mass function `p : X → [0,1]` determines a measure:

\\[
\mu(A)=\sum_{x\in A}p(x).
\\]

Conversely, `p(x)=μ({x})`. Thus Markovian's `ExactFiniteDist` is a coordinate presentation of a finite probability measure.

## Measurable functions and random variables

A function `f : X → Y` is measurable when the inverse image of every measurable event is measurable:

\\[
B\in\Sigma_Y
\Longrightarrow
f^{-1}(B)\in\Sigma_X.
\\]

A random variable is a measurable function from an underlying probability space to a measurable value space. The term “random variable” does not mean that the function itself changes randomly. Randomness comes from the input state.

Markovian usually works directly with distributions and kernels, without representing an underlying sample space. This is an extensional view: two random variables with the same law are not distinguished unless correlation or sharing is represented explicitly.

## Pushforward measure

A measurable function transports a measure from `X` to `Y`. The pushforward is:

\\[
f_{\ast}\mu(B)=\mu(f^{-1}(B)).
\\]

For finite mass functions:

\\[
(f_{\ast}p)(y)=\sum_{x:f(x)=y}p(x).
\\]

This is the deterministic `map` operation on exact distributions. It is also multiplication by the one-hot stochastic matrix induced by `f`.

Pushforward preserves total mass. Categorically, it is the action of the distribution functor on deterministic morphisms.

## Integration becomes finite summation

The integral of a measurable function generalizes a weighted average. In finite probability:

\\[
\int_X h(x)\\,p(dx)=\sum_{x\in X}p(x)h(x).
\\]

Expected reward, Bellman backups, kernel composition, and marginalization all use this pattern.

For example, the expected one-step return under a kernel is:

\\[
\mathbb{E}[r+\gamma V(s')\mid s,a]=
\sum_{r,s'}K(r,s'\mid s,a)\left(r+\gamma V(s')\right).
\\]

In a continuous model, the sum becomes an integral. That replacement is not purely syntactic. The implementation would need measurable rewards, integrability conditions, and a representation of measures and kernels.

## Markov kernels

A Markov kernel `K` from `(X,Σ_X)` to `(Y,Σ_Y)` assigns a probability measure on `Y` to every input `x`:

\\[
x\longmapsto K(x,-).
\\]

It must satisfy two conditions:

1. For fixed `x`, `K(x,-)` is a probability measure on `Y`.
2. For fixed measurable `B⊆Y`, the function `x↦K(x,B)` is measurable.

Kernel composition integrates over the intermediate space:

\\[
(L\circ K)(x,C)=\int_Y L(y,C)\\,K(x,dy).
\\]

For finite spaces:

\\[
(L\circ K)(z\mid x)=\sum_yK(y\mid x)L(z\mid y).
\\]

This is why finite stochastic matrices form a category. Countable additivity, measurability, and integration provide the corresponding measure-theoretic construction.

Kallenberg develops kernels, products, and disintegration in [*Foundations of Modern Probability*](references.md#kallenberg-foundations-of-modern-probability). Fritz explains their categorical abstraction in [Markov categories](references.md#fritz-markov-categories).

## Product measures and independent tensor

Given probability measures `p` on `X` and `q` on `Y`, their product measure models independent pairs:

\\[
(p\otimes q)(A\times B)=p(A)q(B).
\\]

For finite points:

\\[
(p\otimes q)(x,y)=p(x)q(y).
\\]

The tensor of finite stochastic kernels applies the same factorization conditionally:

\\[
(K\otimes L)((y,v)\mid(x,u))=K(y\mid x)L(v\mid u).
\\]

Independence is therefore represented by tensor factorization. Shared randomness is different. If one random value is copied to two consumers, the joint law is concentrated on the diagonal rather than equal to a product measure.

See [Categorical probability: the bridge](categorical-probability.md#copying-sharing-and-independence).

## Joint measures and marginalization

A joint probability measure `J` on `X×Y` determines marginals:

\\[
p(x)=\sum_yJ(x,y),
\qquad
q(y)=\sum_xJ(x,y).
\\]

In measure notation, the marginals are pushforwards along coordinate projections.

A prior `p` and channel `K` determine a joint law:

\\[
J(x,y)=p(x)K(y\mid x).
\\]

This factorization records a generative direction. The same joint law may also factor in the reverse direction when a suitable conditional distribution exists.

## Conditioning and disintegration

A disintegration of a joint measure gives a conditional kernel. Schematically:

\\[
J(dx,dy)=p(dx)K(x,dy).
\\]

In finite probability, when `p(x)>0`:

\\[
K(y\mid x)=\frac{J(x,y)}{p(x)}.
\\]

For a general measurable space, existence is a theorem with hypotheses, not an automatic operation. Regular conditional probabilities exist on important classes such as standard Borel spaces, but can fail on arbitrary measurable spaces.

This is a central boundary for any future continuous Markovian API. A type called `Kernel` is not enough. The implementation would need to state the measurable-space class and the disintegration theorem it relies on.

Read [Kallenberg](references.md#kallenberg-foundations-of-modern-probability), [Faden](references.md#faden-regular-conditional-probabilities), and [Cho and Jacobs](references.md#cho-and-jacobs-disintegration-and-bayesian-inversion).

## Almost-sure equality

Two measurable functions are equal almost surely under `p` when they differ only on a set of `p`-measure zero:

\\[
f=_{p\text{-a.s.}}g
\quad\Longleftrightarrow\quad
p\\{x:f(x)\ne g(x)\\}=0.
\\]

For finite distributions:

\\[
f=_{p\text{-a.s.}}g
\quad\Longleftrightarrow\quad
p(x)>0\Rightarrow f(x)=g(x).
\\]

Conditional distributions are generally unique only almost surely. Their values on impossible conditioning events are not observable under the original measure.

Markovian makes the prior explicit in `almostSureEqual`. It does not collapse all kernels that happen to agree under some unspecified measure.

## Zero evidence and undefined conditioning

Bayes' formula for prior `p` and likelihood channel `K` is:

\\[
K^{\sharp}_{p}(x\mid y)=
\frac{p(x)K(y\mid x)}{\underset{x'\in X}{\sum}p(x')K(y\mid x')}.
\\]

The denominator is the evidence probability `q(y)`. When `q(y)=0`, this expression is undefined.

One can assign an arbitrary posterior on an impossible observation, but different assignments are equivalent only relative to the output prior. Markovian instead exposes positive support and rejects direct conditioning on zero evidence. This makes the mathematical precondition visible.

## Absolute continuity and densities

A measure `ν` is absolutely continuous with respect to `μ`, written `ν≪μ`, when every `μ`-null set is also `ν`-null.

The Radon–Nikodym theorem gives conditions under which there is a density `dν/dμ` satisfying:

\\[
\nu(A)=\int_A\frac{d\nu}{d\mu}\\,d\mu.
\\]

In finite probability, the density is a ratio on positive support:

\\[
\frac{d\nu}{d\mu}(x)=\frac{\nu(x)}{\mu(x)}
\\]

when `μ(x)>0`.

Likelihood ratios, importance weights, and continuous Bayes formulas use this idea. Markovian's current off-policy algorithms do not implement importance sampling, and the exact Bayesian layer uses finite mass ratios rather than a general Radon–Nikodym API.

## Standard Borel spaces

A standard Borel space is a measurable space arising from the Borel sets of a Polish space, up to measurable isomorphism. This class includes familiar finite, countable, Euclidean, and many function spaces while retaining strong existence properties for regular conditional probabilities.

Standard Borel spaces are a common domain for measure-theoretic probability kernels and categorical probability. They are a plausible future continuous carrier class, but Markovian does not implement them.

The distinction matters because “continuous probability” is not one feature. It requires choices about:

- measurable-space representation
- measure and kernel representation
- integration
- equality and almost-sure equality
- disintegration existence
- computability and approximation

The finite implementation avoids these unresolved choices rather than silently assuming them.

## Finite versus measure-theoretic dictionary

| Measure theory | Finite Markovian specialization |
| --- | --- |
| Measurable space `(X,Σ_X)` | Finite duplicate-free support with full power set |
| Probability measure | `ExactFiniteDist X` |
| Measurable function | Finite function or deterministic matrix |
| Pushforward | Distribution map or deterministic composition |
| Markov kernel | `ExactKernel X Y` or exact stochastic matrix |
| Integral | Finite weighted sum |
| Product measure | Independent tensor |
| Disintegration | Exact finite conditioning on positive support |
| Equality almost surely | Equality at every positive-mass point |
| Density | Ratio of finite masses on support |

This dictionary explains both the power and the limit of exact finite semantics.

## Reading route

1. Read [Pollard](references.md#pollard-measure-theoretic-probability) for a motivated introduction to measure-theoretic probability.
2. Use [Kallenberg](references.md#kallenberg-foundations-of-modern-probability), Chapters 1, 4, and 8, for measures, kernels, and conditioning.
3. Read [Faden](references.md#faden-regular-conditional-probabilities) for why regular conditional probabilities require hypotheses.
4. Read [Probability values, distributions, and kernels](probability-kernels.md) for the finite API.
5. Continue to [Categorical probability: the bridge](categorical-probability.md) and [Bayesian inference](bayesian.md).
