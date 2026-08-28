# Law catalogue and proof boundaries

This chapter states the equations that the implementation tests. It also states the equality relation and scalar assumptions for each equation.

A test suite does not prove a theorem for every inhabitant of a Haskell type. The fixtures check representative finite witnesses, reordered layouts, noncommutative scalar multiplication where relevant, and explicit counterexamples. The class laws remain obligations on each admitted instance.

## Notation and equality

Write `g ∘ f` for standard right-to-left composition. The Haskell API passes `f` and then `g` to `composeMatrix` or `composeStochastic`.

Write `I` for the tensor unit, `σ` for symmetry, `α` for associators, `λ` and `ρ` for unitors, `Δ` for copy, and `!` for discard.

Raw matrices use extensional labelled equality. Two matrices are equivalent when they have the same finite source and target supports and the same entry for every labelled pair. Storage order can differ.

Exact distributions and exact kernels use literal rational equality after canonical duplicate aggregation. Floating kernels do not use this equality claim.

## Exact finite distributions and kernels

For a finite exact distribution `p` and a kernel `K`, bind is the finite sum

\\[
(p \mathbin{\operatorname{bind}} K)(y)=\sum_x p(x)K(y\mid x).
\\]

The Dirac distribution is

\\[
\delta_x(y)=
\begin{cases}
1 & x=y,\\
0 & x\ne y.
\end{cases}
\\]

The tested monad laws are

\\[
\delta_x \mathbin{\operatorname{bind}} K=K(x),
\\]

\\[
p \mathbin{\operatorname{bind}} \delta=p,
\\]

and

\\[
(p \mathbin{\operatorname{bind}} K)\mathbin{\operatorname{bind}}L=
p\mathbin{\operatorname{bind}}\bigl(x\mapsto K(x)\mathbin{\operatorname{bind}}L\bigr).
\\]

A kernel `K : X -> D(Y)` composes with `L : Y -> D(Z)` by

\\[
(L\circ K)(z\mid x)=\sum_y K(y\mid x)L(z\mid y).
\\]

Its identity is `x ↦ δx`. Therefore, the kernel category laws are

\\[
K\circ\mathrm{id}=K,
\qquad
\mathrm{id}\circ K=K,
\\]

and

\\[
M\circ(L\circ K)=(M\circ L)\circ K.
\\]

The associativity calculation expands both sides to the same finite sum:

\\[
\sum_{y,z}K(y\mid x)L(z\mid y)M(w\mid z).
\\]

Exact rational arithmetic and canonical finite support make this equality literal. The fixtures also check the `Category`, `Arrow`, and `ArrowChoice` operations.

The direct expected-return evaluator and trace enumerator satisfy

\\[
\mathbb{E}_{\tau\sim\operatorname{traces}(M,\pi,H,\gamma)}[R(\tau)]=
V^{\pi}_{H}(s_0).
\\]

The fixture has expected reward `5` and one transition in every trace.

**Executable evidence:** [`testExactKernelLaws`](https://github.com/josephjohncox/Markovian/blob/main/test/Main.hs#L1875) and [`testExactTraceExpectation`](https://github.com/josephjohncox/Markovian/blob/main/test/Main.hs#L712).

## Why floating kernels do not claim literal associativity

IEEE floating addition is not associative. For example, a typical binary64 evaluation gives

\\[
(10^{16}+(-10^{16}))+1=1,
\\]

but

\\[
10^{16}+((-10^{16})+1)=0.
\\]

Kernel composition changes the grouping of finite sums and products. Two mathematically equal association orders can therefore produce different low bits.

The floating API validates finite values and normalization. It does not advertise literal equality of differently associated computations.

## Raw semiring matrix category

For `A : X -> Y` and `B : Y -> Z`, composition is

\\[
(B\circ A)_{xz}=\sum_{y\in Y}A_{xy}B_{yz}.
\\]

A semiring supplies `0`, `1`, addition, multiplication, distributivity, and additive commutativity. These laws give matrix identity and associativity:

\\[
A\circ I_X=A,
\qquad
I_Y\circ A=A,
\\]

\\[
C\circ(B\circ A)=(C\circ B)\circ A.
\\]

The associativity calculation is

\\[
\sum_z\left(\sum_y A_{xy}B_{yz}\right)C_{zw}=
\sum_y A_{xy}\left(\sum_z B_{yz}C_{zw}\right).
\\]

Finite distributivity moves multiplication through each sum. Associativity of scalar multiplication removes the remaining parentheses.

For a concrete two-state example, let

\\[
A=\begin{bmatrix}1&2\\3&4\end{bmatrix},
\quad
B=\begin{bmatrix}0&1\\1&1\end{bmatrix},
\quad
C=\begin{bmatrix}1&1\\0&1\end{bmatrix}.
\\]

Then

\\[
(AB)C
=\begin{bmatrix}2&5\\4&11\end{bmatrix}
=A(BC).
\\]

The executable fixture adds a harder case with a reordered middle support.

Matrix composition distributes over entrywise addition:

\\[
B\circ(A+A')=B\circ A+B\circ A',
\\]

\\[
(B+B')\circ A=B\circ A+B'\circ A.
\\]

The test deliberately composes through a reordered middle finite support. This checks labelled semantics rather than accidental row order.

## Tensor and interchange

For a commutative semiring, the tensor matrix uses the product finite set and Kronecker entries:

\\[
(A\otimes B)_{(x,u),(y,v)}=A_{xy}B_{uv}.
\\]

Tensor preserves composition through the interchange law:

\\[
(C\otimes D)\circ(A\otimes B)=
(C\circ A)\otimes(D\circ B).
\\]

Commutativity is needed to reorder the scalar factors from the two tensor components.

The circuit layer also checks the symmetric monoidal coherence equations:

- the associator pentagon
- the associator-unitor triangle
- the symmetry hexagon
- associator and symmetry naturality
- symmetry involution
- left and right unitor round trips

These equations say that canonical rewiring does not change denotation.

## Biproduct laws

Direct sum uses `Either` carriers and block-diagonal matrices. Let `ι₁`, `ι₂` be injections and `π₁`, `π₂` be projections. The tested equations are

\\[
\pi_i\circ\iota_j=
\begin{cases}
\mathrm{id} & i=j,\\
0 & i\ne j,
\end{cases}
\\]

and

\\[
\iota_1\circ\pi_1+\iota_2\circ\pi_2=
\mathrm{id}_{X\oplus Y}.
\\]

For a block matrix `B ⊕ C`, extraction recovers each block:

\\[
\pi_1\circ(B\oplus C)\circ\iota_1=B,
\\]

\\[
\pi_2\circ(B\oplus C)\circ\iota_2=C.
\\]

This additive structure belongs to raw matrices. Stochastic matrices do not expose raw addition because `K + L` has row mass two.

## Dagger laws

For an involutive semiring, conjugate transpose is

\\[
(A^\dagger)_{yx}=\overline{A_{xy}}.
\\]

The tested laws are

\\[
(A^\dagger)^\dagger=A,
\\]

\\[
(B\circ A)^\dagger=A^\dagger\circ B^\dagger,
\\]

and, for the commutative tensor domain,

\\[
(A\otimes B)^\dagger=A^\dagger\otimes B^\dagger.
\\]

One fixture uses complex-like scalars so conjugate transpose differs from plain transpose. Another uses noncommutative `2 × 2` matrix scalars. That fixture would fail if dagger did not reverse scalar products.

## Compact structure

For a finite basis `X`, the cup and cap have entries

\\[
\eta_X((),(x,x'))=\mathbf{1}_{x=x'},
\\]

\\[
\varepsilon_X((x,x'),())=\mathbf{1}_{x=x'}.
\\]

Each bold indicator is `1` when its subscript equation holds and `0` otherwise.

After the required associators and unitors, the two snake equations are

\\[
(\mathrm{id}_X\otimes\varepsilon_X)
\circ
(\eta_X\otimes\mathrm{id}_X)=
\mathrm{id}_X,
\\]

\\[
(\varepsilon_X\otimes\mathrm{id}_X)
\circ
(\mathrm{id}_X\otimes\eta_X)=
\mathrm{id}_X.
\\]

These are raw matrix equations. The cup has total mass `|X|` when `X` has more than one element. It is not a normalized stochastic state.

## Trace laws

For `F : X ⊗ U -> Y ⊗ U`, the implemented trace is

\\[
\operatorname{Tr}^{U}(F)_{xy}=
\sum_{u\in U}F_{(x,u),(y,u)}.
\\]

The fixtures check the standard traced symmetric monoidal laws.

### Naturality

For `h : X' -> X` and `g : Y -> Y'`,

\\[
\operatorname{Tr}^{U}
\bigl((g\otimes\mathrm{id}_U)\circ F\circ(h\otimes\mathrm{id}_U)\bigr)=
g\circ\operatorname{Tr}^{U}(F)\circ h.
\\]

### Dinaturality or sliding

For `F : X ⊗ U -> Y ⊗ V` and `k : V -> U`,

\\[
\operatorname{Tr}^{U}
\bigl((\mathrm{id}_Y\otimes k)\circ F\bigr)=
\operatorname{Tr}^{V}
\bigl(F\circ(\mathrm{id}_X\otimes k)\bigr).
\\]

The fixture uses distinct and reordered trace supports.

### Vanishing

Tracing over the tensor unit changes nothing after unitors:

\\[
\operatorname{Tr}^{I}(F)=F.
\\]

Tracing over a product is the same as iterated tracing after reassociation:

\\[
\operatorname{Tr}^{U\otimes V}(F)=
\operatorname{Tr}^{U}
\bigl(\operatorname{Tr}^{V}(F)\bigr).
\\]

### Superposing

For an unrelated matrix `G`,

\\[
\operatorname{Tr}^{U}(F)\otimes G=
\operatorname{Tr}^{U}(F\otimes G),
\\]

with the canonical symmetry and reassociation understood.

### Yanking

\\[
\operatorname{Tr}^{U}(\sigma_{U,U})=\mathrm{id}_U.
\\]

Entrywise, this follows from

\\[
\sum_u \mathbf{1}_{x=u}\mathbf{1}_{u=y}=\mathbf{1}_{x=y}.
\\]

This trace contracts a finite index. It does not solve a stochastic feedback equation.

**Executable evidence:** [`testMatrixCategory`, `testDaggerAndCompact`, and `testTraceLaws`](https://github.com/josephjohncox/Markovian/blob/main/test/AlgebraicFoundation.hs#L184-L420).

## Normalized stochastic matrices

A stochastic matrix `K : X -> Y` satisfies

\\[
K_{xy}\ge0,
\qquad
\sum_y K_{xy}=1
\\]

for every represented source `x`.

Identity, composition, and tensor preserve normalization. For composition,

\\[
\sum_z(L\circ K)_{xz}=
\sum_yK_{xy}\sum_zL_{yz}=
\sum_yK_{xy}=
1.

\\]

The stochastic fixtures check identity, associativity, tensor interchange, and closure of copy and discard.

Each object has a commutative comonoid:

\\[
\Delta_X(x,(x_1,x_2))=\mathbf{1}_{x=x_1=x_2},
\\]

\\[
!_X(x,())=1.
\\]

The structural equations include

\\[
\sigma\circ\Delta=\Delta,
\\]

\\[
(\Delta\otimes\mathrm{id})\circ\Delta=
(\mathrm{id}\otimes\Delta)\circ\Delta,
\\]

and the two counit equations

\\[
(!_X\otimes\mathrm{id})\circ\Delta=\mathrm{id}=
(\mathrm{id}\otimes !_X)\circ\Delta.
\\]

Discard is natural for every stochastic channel:

\\[
!_Y\circ K=!_X.
\\]

Copy is not natural for every stochastic channel.

## Deterministic subcategory and copy naturality

A deterministic matrix has one entry `1` in each source row and `0` elsewhere. Such matrices are closed under identity, composition, and tensor.

For a deterministic `f : X -> Y`, copy is natural:

\\[
\Delta_Y\circ f=(f\otimes f)\circ\Delta_X.
\\]

For a fair coin `c : I -> Bool`, the left side samples once and copies:

\\[
P_c(F,F)=P_c(T,T)=\frac12.
\\]

The right side runs two tensor occurrences:

\\[
P_{c\otimes c}(F,F)=P(F,T)=P(T,F)=P(T,T)=\frac14.
\\]

Therefore, copy naturality would equate correlated and independent draws. The purity index permits this rewrite only for deterministic circuits.

**Executable evidence:** [`testStochasticNormalization` and `testDeterministic`](https://github.com/josephjohncox/Markovian/blob/main/test/AlgebraicFoundation.hs#L421-L558), plus [`testPurityAndSharing`](https://github.com/josephjohncox/Markovian/blob/main/test/StochasticCircuit.hs#L147-L193).

## Exact convex laws

For coefficients `λᵢ ≥ 0` with `Σᵢ λᵢ = 1`, the mixture is

\\[
\operatorname{mix}(\lambda_i,K_i)=\sum_i\lambda_iK_i.
\\]

The fixtures check these laws:

- singleton: `mix(1,K) = K`
- zero elimination: zero-weight branches do not change the result
- permutation invariance
- flattening: a mixture of mixtures multiplies outer and inner weights
- composition is affine in either argument separately
- tensor is affine in either argument separately

For example,

\\[
L\circ\left(\sum_i\lambda_iK_i\right)=
\sum_i\lambda_i(L\circ K_i).
\\]

However, tensor is not jointly affine over one shared index:

\\[
\sum_i\lambda_i(K_i\otimes L_i)
\ne
\left(\sum_i\lambda_iK_i\right)
\otimes
\left(\sum_i\lambda_iL_i\right)
\\]

in general. The right side introduces cross terms `Kᵢ ⊗ Lⱼ`. It represents independent choices of branch indexes. The left side represents one shared branch index.

The same distinction applies to correlated composition pairs. This is another form of the shared-versus-independent randomness boundary.

**Executable evidence:** [`testConvex`](https://github.com/josephjohncox/Markovian/blob/main/test/AlgebraicFoundation.hs#L559-L693).

## Bayesian inversion laws

Let `p` be a prior on `X`, let `K : X -> Y`, and let

\\[
q(y)=\sum_x p(x)K(y\mid x).
\\]

On positive supports, the Bayesian inverse `K^{\sharp}_p : Y_q -> X_p` satisfies the balance equation

\\[
p(x)K(y\mid x)=q(y)K^{\sharp}_p(x\mid y).
\\]

The fixtures check:

### Identity

\\[
(\mathrm{id}_X)^{\sharp}_p=
\mathrm{id}_{X_p}.
\\]

### Reversal of composition

When the intermediate prior is the pushforward `q = pK`,

\\[
(L\circ K)^{\sharp}_p=
K^{\sharp}_p\circ L^{\sharp}_q.
\\]

### Independent tensor

\\[
(K\otimes L)^{\sharp}_{p\otimes r}=
K^{\sharp}_p\otimes L^{\sharp}_r.
\\]

### Double inversion

\\[
\left(K^{\sharp}_p\right)^{\sharp}_q=
K\vert_{X_p\to Y_q}.
\\]

The right side is the forward channel restricted to positive prior and evidence supports.

### Almost-sure uniqueness

Two inverse candidates can differ on a zero-evidence row. They remain equal under `q`:

\\[
H\sim_q H'
\quad\Longleftrightarrow\quad
q(y)>0\Rightarrow H(-\mid y)=H'(-\mid y).
\\]

The fixtures check reflexivity, symmetry, transitivity, positive-support disagreement, and preservation by postcomposition.

These laws are dagger-like only after the prior, support restriction, and almost-sure equality are part of the statement. They do not define a dagger on plain stochastic matrices.

**Executable evidence:** [`testBayesianInversionLaws` and `testAlmostSureAndChannels`](https://github.com/josephjohncox/Markovian/blob/main/test/BayesianExact.hs#L155-L323).

## Circuit interpretation laws

For the exact interpreter `⟦-⟧`, the fixtures check

\\[
\lbrack\!\lbrack\mathrm{id}\rbrack\!\rbrack=\mathrm{id},
\\]

\\[
\lbrack\!\lbrack g\circ f\rbrack\!\rbrack=
\lbrack\!\lbrack g\rbrack\!\rbrack\circ\lbrack\!\lbrack f\rbrack\!\rbrack,
\\]

\\[
\lbrack\!\lbrack f\otimes g\rbrack\!\rbrack=
\lbrack\!\lbrack f\rbrack\!\rbrack\otimes\lbrack\!\lbrack g\rbrack\!\rbrack.
\\]

They also check symmetry involution, associator and unitor round trips, copy cocommutativity, copy counits, stochastic discard naturality, pentagon, triangle, hexagon, and structural naturality.

The deterministic compiler preserves identity, composition, products, pairing, and projections. Pairing compiles through copy followed by tensor.

**Executable evidence:** [`testHomomorphismAndCoherence` and `testMonoidalCoherence`](https://github.com/josephjohncox/Markovian/blob/main/test/StochasticCircuit.hs#L194-L369), plus [`testDeterministicCompilation`](https://github.com/josephjohncox/Markovian/blob/main/test/StochasticCircuit.hs#L465).

## Open-system and acyclic denotation laws

Structured cospan composition is associative up to the canonical representation isomorphism induced by nested pushouts. It is not literal Haskell equality because the nested sum carrier types differ.

For compatible open-system cells, the double interchange equation is

\\[
(\beta_2\odot\beta_1)\circ(\alpha_2\odot\alpha_1)=
(\beta_2\circ\alpha_2)\odot(\beta_1\circ\alpha_1),
\\]

where `⊙` is horizontal composition and `∘` is vertical composition.

For the supported acyclic interpreter `D`, the tested denotational laws are

\\[
D(\mathrm{id}_X)=\mathrm{id}_{D(X)},
\\]

\\[
D(H\circ G)=D(H)\circ D(G),
\\]

\\[
D(G\otimes H)=D(G)\otimes D(H),
\\]

up to named-boundary reindexing where pushouts choose different carrier representations.

The interpreter also checks normalization, sharing, discard marginalization, conditional products, renaming invariance, boundary permutation invariance, and ready-edge schedule independence.

**Executable evidence:** [`testOpenAssociativity`, `testDoubleInterchange`, and `testOpenCircuitDenotation`](https://github.com/josephjohncox/Markovian/blob/main/test/OpenSystems.hs#L353-L621), plus the [acyclic semantic fixtures](https://github.com/josephjohncox/Markovian/blob/main/test/AcyclicOpenSystems.hs#L810-L1247).

## What the tests do and do not establish

The fixtures establish executable evidence for the represented finite examples and rejection boundaries. Compile-fail tests also show that users cannot construct selected invalid proof refinements through the public API.

They do not mechanically prove every class law for every future scalar instance. Each new scalar, interpreter, or backend needs its own evidence.

The implementation does not claim:

- compact closure of normalized stochastic matrices
- a stochastic trace or general feedback operator
- Bayesian inversion without a prior
- literal associativity for floating kernels
- strict equality of differently nested pushout carriers
- arbitrary cyclic open-graph denotation
- unrestricted coherence beyond the supported finite syntax

## References and further reading

The equations above follow standard structures from category theory, categorical probability, and stochastic control. See these annotated sections:

- [Category theory and string diagrams](references.md#category-theory-and-string-diagrams)
- [Probability kernels, Markov categories, and Bayes](references.md#probability-kernels-markov-categories-and-bayes)
- [Semirings, matrices, and convexity](references.md#semirings-matrices-and-convexity)
- [Open systems and compositional networks](references.md#open-systems-and-compositional-networks)
- [MDPs, POMDPs, and learning](references.md#mdps-pomdps-and-learning)
- [Numerical computation](references.md#numerical-computation)
