# Categorical probability: the bridge

Measure theory describes probability through sigma-algebras, measures, and integrals. Category theory describes how probabilistic processes compose. Markovian's finite exact semantics sits where these descriptions coincide.

This chapter connects the two views. Read [Category theory behind the interfaces](category-primer.md) and [Measure theory and the finite specialization](measure-theory-primer.md) first if either vocabulary is unfamiliar. [Information theory for finite stochastic models](information-theory.md) then assigns entropy, divergence, and mutual information to these finite states and channels, while [Categorical structure of learning and neural networks](categorical-learning.md) explains reverse derivatives and optimization without conflating them with Bayesian inversion.

## Three presentations of one finite channel

For finite sets `X` and `Y`, the following data are equivalent:

1. A function `K : X → D(Y)` that returns a finite distribution.
2. A conditional mass table `K(y|x)` whose rows sum to one.
3. A row-stochastic matrix with source `X` and target `Y`.

The presentations emphasize different facts.

- The function presentation emphasizes effectful computation.
- The conditional table emphasizes probability.
- The matrix presentation emphasizes linear algebra and compilation.

Markovian uses all three rather than forcing one API to carry every proof and execution concern.

## From finite sums to kernel integrals

Finite kernel composition is:

\\[
(L\circ K)(z\mid x)=\sum_yK(y\mid x)L(z\mid y).
\\]

For measurable spaces, the same pattern becomes:

\\[
(L\circ K)(x,C)=\int_YL(y,C)\\,K(x,dy).
\\]

The finite sum is therefore not an unrelated special case. It is integration against an atomic probability measure.

Associativity follows from rearranging finite sums in Markovian. In measure theory it relies on measurable-kernel integration and Tonelli or Fubini type results. The categorical equation is the same:

\\[
M\circ(L\circ K)=(M\circ L)\circ K.
\\]

Category theory hides the integration proof after the category has been constructed. It does not eliminate the analytic hypotheses needed to construct that category.

## Probability monads and Kleisli categories

A probability monad packages:

- Dirac measures as `pure`
- pushforward as functorial `map`
- averaging of measures as `join`

Its Kleisli morphisms have the form:

\\[
X\longrightarrow\mathcal{P}(Y),
\\]

where `P(Y)` denotes an appropriate space of probability measures.

For finite sets, an unrestricted mathematical distribution construction is often presented as a monad. For measurable spaces, the Giry monad is the classical example. Its Kleisli composite performs integration over the intermediate measure.

Markovian does not expose that unrestricted interface. `ExactFiniteDist` has no `Applicative` or `Monad` instance, because checked sequencing can reject or report differently under reassociation. The executable laws cover only admitted checked binds whose limits allow both sides; `ExactKernel` has no unrestricted `Category`, `Arrow`, or `ArrowChoice` instance.

Read [Giry](references.md#giry-probability-as-a-monad), [Moggi](references.md#moggi-kleisli-semantics), and [Riehl, Chapter 5](references.md#riehl-category-theory-in-context).

## States are measures

In a monoidal category, a state of `X` is a morphism from the tensor unit:

\\[
p:I\longrightarrow X.
\\]

In finite stochastic semantics, `I` has one point, so the only input row is a probability distribution on `X`.

A channel `K : X → Y` pushes the state forward:

\\[
q=K\circ p.
\\]

In coordinates:

\\[
q(y)=\sum_xp(x)K(y\mid x).
\\]

In measure notation:

\\[
q(B)=\int_XK(x,B)\\,p(dx).
\\]

This single composition operation covers prediction through a sensor, one Markov transition, and marginalization of a joint law.

## Copying, sharing, and independence

Every object has a deterministic copy map:

\\[
\Delta_X(x)=(x,x).
\\]

Applying copy after a state produces the diagonal coupling:

\\[
(\Delta_X\circ p)(x_1,x_2)=
\begin{cases}
p(x_1) & x_1=x_2,\\\\0 & x_1\ne x_2.
\end{cases}
\\]

Tensoring the state with itself produces independent draws:

\\[
(p\otimes p)(x_1,x_2)=p(x_1)p(x_2).
\\]

These are different joint measures. A fair bit gives diagonal masses `1/2` under sharing and four masses `1/4` under independent repetition.

Categorically, sharing is `Δ∘p`; independent repetition is `p⊗p`. In measure theory, one is a pushforward along the diagonal function and the other is a product measure.

This equivalence between wiring and coupling is one of the main reasons categorical notation is useful for probabilistic programs.

## Why deterministic maps preserve copy

For a measurable function `f : X → Y`:

\\[
\Delta_Y\circ f=(f\otimes f)\circ\Delta_X.
\\]

Both sides send `x` to `(f(x),f(x))`.

For a genuinely stochastic channel, the left side samples once and shares the result. The right side samples twice. Equality can fail.

A deterministic morphism in a Markov category is characterized by preservation of copy. In the finite nonnegative setting, this corresponds to one-hot stochastic rows.

Markovian's deterministic matrix refinement and circuit purity index carry this evidence. They justify copy-naturality rewrites without claiming them for arbitrary stochastic boxes.

Read [Fritz, §§2–3](references.md#fritz-markov-categories) and [Stochastic circuits and deterministic compilation](circuits.md).

## Joint states and conditionals

A joint state has type:

\\[
J:I\longrightarrow X\otimes Y.
\\]

A marginal discards one component:

\\[
p=(\mathrm{id}_X\otimes !_Y)\circ J.
\\]

A conditional channel `K : X → Y` disintegrates `J` when rebuilding the joint from the marginal gives the original state. In finite coordinates:

\\[
J(x,y)=p(x)K(y\mid x).
\\]

The string diagram expresses factorization without choosing a coordinate order. The finite implementation computes the same factorization by exact division on positive support.

In arbitrary measurable spaces, existence of such a conditional is not automatic. The categorical definition can be stated generally, while each concrete category must prove or assume existence.

See [Cho and Jacobs](references.md#cho-and-jacobs-disintegration-and-bayesian-inversion), [Faden](references.md#faden-regular-conditional-probabilities), and [Bayesian inference](bayesian.md).

## Bayesian inversion decorates reversal with a state

Given prior `p : I → X` and channel `K : X → Y`, define the output state:

\\[
q=K\circ p.
\\]

A Bayesian inverse points backward:

\\[
K^{\sharp}_{p}:Y_q\longrightarrow X_p.
\\]

The decorated object notation reminds us that the carrier comes with a state or positive support. The balance equation is:

\\[
p(x)K(y\mid x)=q(y)K^{\sharp}_{p}(x\mid y).
\\]

Bayesian inversion reverses compatible composition:

\\[
(L\circ K)^{\sharp}_{p}=
K^{\sharp}_{p}\circ L^{\sharp}_{q}
\\]

on the appropriate positive supports and modulo almost-sure equality.

This resembles dagger functoriality, but the prior and support are essential. A plain stochastic matrix does not carry enough data to define the operation.

See [Cho and Jacobs, §5](references.md#cho-and-jacobs-disintegration-and-bayesian-inversion) and [Derived categorical and mathematical insights](categorical-insights.md#5-bayesian-inversion-is-dagger-like-only-after-adding-state).

## Almost-sure equality is contextual equality

Measure theory identifies conditional versions that differ only on a null event. Categorically, equality is indexed by a state.

For channels `F,G : X → Y` and state `p : I → X`, finite almost-sure equality means:

\\[
p(x)>0\Longrightarrow F(-\mid x)=G(-\mid x).
\\]

This is weaker than literal table equality and stronger than saying that one selected expectation happens to agree.

The context `p` is part of the proposition. A row ignored by one prior can matter under another prior.

Markovian exposes this relation directly rather than using it as global equality for all kernels.

## Conditional independence as factorization

Suppose `X`, `Y`, and `Z` have a joint state. Informally, `X` and `Y` are conditionally independent given `Z` when their conditional joint law factors:

\\[
P(x,y\mid z)=P(x\mid z)P(y\mid z).
\\]

Categorically, this is a diagram in which the `Z` value is copied and sent through separate conditional channels. The equation is independent of coordinates.

Markovian does not currently expose a general conditional-independence decision procedure. The concept still explains why tensor, copy, and sharing are kept explicit in circuit and Bayesian semantics.

Fritz develops categorical conditional independence and its relationship to ordinary kernels in [§§11–12](references.md#fritz-markov-categories).

## Convexity is external random choice

A convex mixture of channels:

\\[
K=\sum_i\lambda_iK_i
\\]

can be understood as sampling a hidden index `i` with state `λ`, then running `K_i`.

If the same hidden index controls two outputs, the outputs are correlated through the shared choice. If each side samples its own index, tensor creates independent choices and cross terms.

Thus convex enrichment and monoidal tensor describe two different constructions:

- mixture hides one random choice
- tensor combines processes conditionally independently

Markovian tests separate affinity but rejects the false joint-affinity equation. See [Algebra behind the implementation](algebra-primer.md#convex-combinations) and the [law catalogue](laws-and-boundaries.md#exact-convex-laws).

## Bellman operators are not categorical traces

A raw matrix trace contracts a finite index:

\\[
\operatorname{Tr}^{U}(F)(x,y)=
\sum_uF((x,u),(y,u)).
\\]

A discounted Bellman equation seeks a fixed point:

\\[
V=T(V).
\\]

Both can be drawn with a loop, but the loops mean different mathematics. The first is finite summation over a repeated label. The second is recursive dependence solved by contraction or linear algebra.

Markovian keeps raw matrix trace, exact Bellman evaluation, and cyclic open-system interpretation separate. The open-system interpreter currently accepts only a validated acyclic fragment.

## Open systems separate topology from probability

A structured cospan describes how named interfaces attach to an internal graph. It composes by pushout. This is topological composition.

An acyclic denotation assigns exact channels to graph edges and eliminates hidden finite values:

\\[
D_G(y\mid x)=
\sum_h\prod_{e\in E}
K_e\bigl(a_{\mathrm{out}(e)}\mid a_{\mathrm{in}(e)}\bigr).
\\]

The product combines local factors. The sum marginalizes hidden assignments. This is the same sum-product algebra used by graphical-model variable elimination.

The structured-cospan category says how boundaries glue. The stochastic denotation says how probabilities compose. Neither one alone supplies the other.

Read [Baez and Courser](references.md#baez-and-courser-structured-cospans), [Koller and Friedman](references.md#koller-and-friedman-sum-product-elimination), and [Open systems and acyclic networks](open-systems.md).

## What category theory contributes

Category theory contributes three practical forms of discipline.

### Interface discipline

Types state the source and target of every process. Composition is only defined when boundaries match.

### Equation discipline

A rewrite must follow from the structure present at that layer. Copy naturality needs determinism. Dagger needs involution. Bayesian reversal needs a prior. Fixed points need convergence assumptions.

### Compositional proofs

Local equations can be substituted inside larger diagrams. A circuit interpreter that preserves identity, composition, and tensor transports syntax-level equations to denotations.

Category theory does not prove scalar arithmetic, measurability, numerical stability, or convergence automatically. Those obligations remain in algebra, measure theory, and analysis.

## Correspondence table

| Finite computation | Measure-theoretic form | Categorical form |
| --- | --- | --- |
| Probability vector | Probability measure | State `I → X` |
| Conditional table | Markov kernel | Morphism `X → Y` |
| Sum over hidden value | Integral or marginal | Composition or discard |
| Independent product | Product measure | Tensor |
| Reuse one draw | Diagonal pushforward | Copy after state/channel |
| Deterministic function | Measurable map | Copy-preserving morphism |
| Conditional distribution | Disintegration kernel | Conditional morphism |
| Bayes rule | Reverse conditional | Prior-indexed Bayesian inverse |
| Ignore null rows | Equality almost everywhere | State-indexed almost-sure equality |

## Reading route

1. Read [Leinster](references.md#leinster-basic-category-theory) or [Riehl](references.md#riehl-category-theory-in-context) for basic categorical language.
2. Read [Kallenberg](references.md#kallenberg-foundations-of-modern-probability) or [Pollard](references.md#pollard-measure-theoretic-probability) for measure kernels and conditioning.
3. Read [Giry](references.md#giry-probability-as-a-monad) for the probability monad.
4. Read [Fritz](references.md#fritz-markov-categories) for Markov categories and [Cho–Jacobs](references.md#cho-and-jacobs-disintegration-and-bayesian-inversion) for Bayesian inversion.
5. Read [Information theory](information-theory.md) for channel observables and data processing.
6. Read [Categorical learning](categorical-learning.md) for reverse derivatives, sharing, and optimization.
7. Read [Polarity, push-pull duality, and games](polarity-and-games.md) to separate forward state propagation, backward payoff evaluation, and prior-indexed inference.
8. Return to the [law catalogue](laws-and-boundaries.md) to see which equations are implemented and tested exactly.
