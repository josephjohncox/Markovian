# Category theory behind the interfaces

This chapter assumes that the reader knows the definitions of a category and a functor, but may not know why they matter for stochastic software. It connects categorical vocabulary directly to Markovian's finite types. The consequences for entropy, reverse derivatives, parameter sharing, and optimization are developed in [Information theory for finite stochastic models](information-theory.md) and [Categorical structure of learning and neural networks](categorical-learning.md).

## Category: objects, morphisms, and composition

A category has:

- objects `X`, `Y`, and `Z`
- morphisms `f : X → Y`
- identity morphisms `id_X : X → X`
- composition `g ∘ f : X → Z` for `f : X → Y` and `g : Y → Z`

The two defining equations are identity and associativity:

\\[
\mathrm{id}_Y\circ f=f=f\circ\mathrm{id}_X,
\\]

\\[
h\circ(g\circ f)=(h\circ g)\circ f.
\\]

These equations say that a composite can be understood locally. Parenthesization and inserted identity wires do not change its meaning.

In Markovian, several different categories occur:

- finite sets and deterministic functions
- finite distributions and stochastic kernels under Kleisli composition
- finite semiring matrices under matrix multiplication
- validated stochastic matrices under the same multiplication
- structured cospans under pushout composition

They share the word “category” because they satisfy the same shape of laws. They do not thereby share every operation.

For a compact introduction to categories, functors, natural transformations, and universal properties, read [Leinster](references.md#leinster-basic-category-theory) or [Riehl](references.md#riehl-category-theory-in-context).

## Morphisms encode the allowed notion of process

An object does not determine a category by itself. The chosen morphisms matter.

For finite sets, a deterministic function `f : X → Y` chooses one output for every input. A stochastic kernel instead chooses a probability distribution:

\\[
K(-\mid x)\in\mathcal{D}(Y).
\\]

The same carriers `X` and `Y` therefore participate in two categories with different morphisms.

A raw matrix is more general again. Its entries can be arbitrary semiring values. It need not be normalized and need not represent probability.

This explains Markovian's refinement layers. Forgetting a deterministic proof gives a stochastic matrix. Forgetting normalization gives a raw nonnegative matrix. These are forgetful directions; the reverse direction requires evidence.

## The finite distribution functor and monad

Let `D(X)` be the finite probability distributions on `X`. A function `f : X → Y` pushes a distribution forward:

\\[
\mathcal{D}(f)(p)(y)=\sum_{x:f(x)=y}p(x).
\\]

This assignment is functorial:

\\[
\mathcal{D}(\mathrm{id}_X)=\mathrm{id}_{\mathcal{D}(X)},
\\]

\\[
\mathcal{D}(g\circ f)=\mathcal{D}(g)\circ\mathcal{D}(f).
\\]

The distribution functor is also a monad.

- `pure` sends a value to its Dirac distribution.
- `map` pushes a distribution through a deterministic function.
- `join` averages a distribution of distributions.
- `bind` chooses an intermediate value and then runs the next stochastic computation.

For `p ∈ D(X)` and `K : X → D(Y)`:

\\[
(p\mathbin{\operatorname{bind}}K)(y)=\sum_x p(x)K(y\mid x).
\\]

The monad laws become the identity and associativity laws for stochastic sequencing.

Moggi's account explains why Kleisli categories model effectful computation; see [Moggi](references.md#moggi-kleisli-semantics). Giry's measure-theoretic probability monad is the continuous analogue; see [Giry](references.md#giry-probability-as-a-monad).

## Kleisli composition is kernel composition

A finite stochastic kernel is a function:

\\[
K:X\longrightarrow\mathcal{D}(Y).
\\]

Given `K : X → D(Y)` and `L : Y → D(Z)`, Kleisli composition is:

\\[
(L\circ K)(z\mid x)=\sum_yK(y\mid x)L(z\mid y).
\\]

This is the law of total probability. It is also stochastic matrix multiplication. The three views coincide in the finite setting:

1. effectful function composition
2. conditional-probability composition
3. normalized matrix multiplication

Markovian keeps the APIs distinct where their evidence differs, but their denotations agree.

## Monoidal structure means parallel composition

A monoidal category has a tensor product `⊗`, a unit object `I`, associators, unitors, and coherence laws. The tensor combines objects and morphisms in parallel:

\\[
f\otimes g:X\otimes U\longrightarrow Y\otimes V.
\\]

For finite stochastic kernels, object tensor is cartesian product and channel tensor is conditional independence:

\\[
(K\otimes L)((y,v)\mid(x,u))=K(y\mid x)L(v\mid u).
\\]

The tensor laws include interchange:

\\[
(L_1\circ K_1)\otimes(L_2\circ K_2)=
(L_1\otimes L_2)\circ(K_1\otimes K_2).
\\]

Associators and unitors move parentheses and unit values. They are not computational guesses. They witness canonical isomorphisms between layouts such as `(X × Y) × Z` and `X × (Y × Z)`.

Mac Lane's coherence theorem justifies suppressing many such maps in diagrams. Read [Mac Lane](references.md#mac-lane-categories-and-monoidal-coherence) and [Selinger](references.md#selinger-graphical-languages).

## Cartesian structure is stronger than monoidal structure

A cartesian category has natural copy and discard maps:

\\[
\Delta_X:X\longrightarrow X\times X,
\qquad
!_X:X\longrightarrow 1.
\\]

For deterministic functions, copy is natural:

\\[
\Delta_Y\circ f=(f\otimes f)\circ\Delta_X.
\\]

The left side computes once and copies the result. The right side copies the input and computes twice. Determinism makes them equal.

For a stochastic kernel, the left side shares one random draw while the right side performs two conditionally independent draws. They generally differ. Thus the stochastic category is symmetric monoidal but not cartesian in the ordinary sense.

This distinction is operationally important. In circuit syntax:

- `share c` executes `c` once and copies its sampled result
- two occurrences of `c` execute separately

The deterministic refinement is therefore a proof that cartesian rewrites are sound. It is not only an optimization annotation.

## Markov categories isolate the probability structure

A Markov category is a symmetric monoidal category in which each object has copy and discard operations satisfying commutative comonoid laws, and discard is natural for all morphisms.

Every stochastic channel preserves discard:

\\[
!_Y\circ K=!_X.
\\]

This equation says that total probability remains one. Copy is available as a deterministic morphism, but it is natural only for deterministic channels.

The framework captures probability without choosing coordinates or integrals in every statement. It supports categorical formulations of:

- deterministic morphisms
- conditional independence
- disintegration
- Bayesian inversion
- almost-sure equality
- sufficient statistics

Markovian implements a finite exact fragment of this picture. The primary source is [Fritz](references.md#fritz-markov-categories). For a focused treatment of disintegration and Bayesian inversion, read [Cho and Jacobs](references.md#cho-and-jacobs-disintegration-and-bayesian-inversion).

## States, channels, and effects

In a monoidal category, a state of `X` is a morphism from the tensor unit:

\\[
p:I\longrightarrow X.
\\]

For finite stochastic kernels, this is exactly a probability distribution on `X`.

A channel is an ordinary morphism:

\\[
K:X\longrightarrow Y.
\\]

Composing a state with a channel gives the pushforward state:

\\[
q=K\circ p.
\\]

An effect points from an object to the unit. In a Markov category with normalized channels, the only deterministic discard effect is `!_X`. More general probabilistic predicates require a richer effect or partial-map setting than Markovian's normalized kernel core.

This notation helps separate a prior from a channel. Bayesian inversion depends on both.

## Bayesian inversion is not matrix dagger

For a prior `p` and channel `K`, let `q=K∘p`. A Bayesian inverse `K_p^♯` satisfies:

\\[
p(x)K(y\mid x)=q(y)K^{\sharp}_p(x\mid y)
\\]

where `q(y)>0`.

The inverse depends on the prior and is determined only on positive-evidence outputs. It reverses compatible composition modulo almost-sure equality, but it is not an ordinary inverse and not a transpose.

Raw matrix dagger uses scalar involution and transpose. Boundary reversal swaps interface legs. Bayesian inversion uses state and support. Markovian exposes three operations because their categorical contexts differ.

See [Bayesian inference](bayesian.md), [Measure theory and the finite specialization](measure-theory-primer.md), and [Cho and Jacobs](references.md#cho-and-jacobs-disintegration-and-bayesian-inversion).

## Dagger, compact structure, and trace belong to raw matrices

Finite raw matrices over an involutive semiring have dagger:

\\[
A^{\dagger}(y,x)=\overline{A(x,y)}.
\\]

Finite matrix categories also have cups and caps, which satisfy snake equations, and a finite trace that contracts one repeated index.

These structures support string-diagram bending and feedback-like notation at the raw algebraic layer. They do not preserve stochastic normalization in general.

This is why Markovian does not claim that normalized stochastic kernels form a dagger compact category. The exact scope is stated in [Finite objects and semiring matrices](matrices.md) and checked in the [law catalogue](laws-and-boundaries.md#dagger-laws).

Read [Kelly and Laplaza](references.md#kelly-and-laplaza-compact-closed-coherence), [Joyal, Street, and Verity](references.md#joyal-street-and-verity-traced-monoidal-categories), and [Selinger](references.md#selinger-graphical-languages).

## Structured cospans describe open boundaries

An open system has an input interface, an output interface, and an apex containing internal structure. A structured cospan has the shape:

\\[
L(X)\longrightarrow G\longleftarrow L(Y).
\\]

Composition glues the output interface of one system to the input interface of another by a pushout. Tensor places systems side by side through disjoint union.

This category describes system topology. A separate denotation maps a validated acyclic open circuit to an exact stochastic channel. Keeping topology and denotation separate prevents a graph operation from silently acquiring unsupported stochastic semantics.

Vertical interface maps, horizontal cospans, and cells form a double-category fragment. Markovian tests identity, associativity up to canonical representation, tensor, and interchange for finite witnesses.

Read [Baez and Courser](references.md#baez-and-courser-structured-cospans), [Fong](references.md#fong-decorated-cospans), and [Open systems and acyclic networks](open-systems.md).

## How to read a string diagram

A string diagram is read from input boundary to output boundary.

- A wire is an object.
- A box is a morphism.
- Sequential boxes compose.
- Side-by-side boxes tensor.
- A fork is copy.
- A terminated wire is discard.
- A state has no input wire.

Two diagrams denote the same morphism when the corresponding categorical laws permit the deformation. A diagram is not permission to apply every visual rewrite. Copy-through-box is valid only for deterministic boxes. Wire bending is valid only where compact structure exists. Feedback requires a trace or fixed-point semantics, not merely a drawn loop.

Selinger's survey is the most direct reference for translating between equations and diagrams: [author PDF](references.md#selinger-graphical-languages).

## Structure map

| Layer | Objects | Morphisms | Main structure |
| --- | --- | --- | --- |
| Finite deterministic | Finite sets | Functions or one-hot matrices | Cartesian symmetric monoidal |
| Finite stochastic | Finite sets | Normalized kernels | Markov category fragment |
| Raw matrix | Finite objects | Semiring matrices | Biproduct, tensor; dagger/compact/trace with stronger scalars |
| Circuit syntax | Typed finite wires | Purity-indexed syntax | Symmetric monoidal, explicit sharing |
| Open topology | Finite interfaces | Structured cospans | Symmetric monoidal double-category fragment |
| Bayesian channel | Prior-decorated finite supports | Prior-compatible channels | Prior-indexed reversal modulo almost-sure equality |

No row of this table inherits every structure from another row.

## Reading route

1. Review [Leinster](references.md#leinster-basic-category-theory), Chapters 1 and 5, or [Riehl](references.md#riehl-category-theory-in-context), Chapters 1, 3, and 5.
2. Read [Selinger](references.md#selinger-graphical-languages) for monoidal diagrams.
3. Read [Fritz](references.md#fritz-markov-categories), especially §§2–3, 11, and 13.
4. Read [Categorical probability: the bridge](categorical-probability.md) for the connection to measure kernels.
5. Read [Information theory](information-theory.md) for channel observables and [categorical learning](categorical-learning.md) for reverse derivatives, diagonals, and optimization.
6. Use the [law catalogue](laws-and-boundaries.md) to find executable finite witnesses.
