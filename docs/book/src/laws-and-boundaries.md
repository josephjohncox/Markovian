# Law catalogue and proof boundaries

This chapter states the equations that the implementation tests. It also states the equality relation and scalar assumptions for each equation.

A test suite does not prove a theorem for every inhabitant of a Haskell type. The fixtures check representative finite witnesses, reordered layouts, noncommutative scalar multiplication where relevant, and explicit counterexamples. Each instance must satisfy the class laws.

Use [Algebra behind the implementation](algebra-primer.md), [Category theory behind the interfaces](category-primer.md), [Measure theory and the finite specialization](measure-theory-primer.md), and [Categorical probability: the bridge](categorical-probability.md) when a law's surrounding structure is unfamiliar. Their reading routes link directly to primary sources.

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

## Exact discounted control bounds

For a policy-free compiled finite MDP, define the Bellman optimality operator:

\\[
(T_{\star}V)(s)=\max_{a\in A(s)}
\sum_{r,s'}K(s,a)(r,s')\left(r+\gamma V(s')\right).
\\]

Terminal values remain fixed at their payoffs. With `0 <= gamma < 1`, `T_*` is a contraction in sup norm.

For residual `delta = ||T_*V - V||_infinity`, the implemented value bound is:

\\[
\lVert V-V^{\star}\rVert_\infty\le\frac{\delta}{1-\gamma}.
\\]

The greedy policy from `V` has the implemented performance bound:

\\[
\lVert V^{\star}-V^{\pi_V}\rVert_\infty
\le\frac{2\gamma\delta}{(1-\gamma)^2}.
\\]

Action maxima use model availability order and strict greater-than replacement. Exact ties retain the first available action.

Policy iteration solves fixed-policy equations over signed rationals. It then replaces a policy action only when an exact action value strictly improves.

**Executable evidence:** `test/ExactControl.hs` in the source distribution.

## Tabular one-step targets

All tabular updates use `x' = x + alpha * (y - x)`. Their continuing targets differ:

\\[
\begin{aligned}
y_{\mathrm{TD(0)}}&=r+\gamma V(s'),\\
y_{\mathrm{SARSA}}&=r+\gamma Q(s',a'),\\
y_{\mathrm{Expected\ SARSA}}&=r+\gamma\sum_a\pi_\epsilon(a\mid s')Q(s',a),\\
y_{\mathrm{Q-learning}}&=r+\gamma\max_a Q(s',a).
\end{aligned}
\\]

All four terminal targets are:

\\[
y_{\mathrm{terminal}}=r+\gamma g(s').
\\]

The root tests compare pure targets and exact seeded traces. They do not assert learning convergence.

**Executable evidence:** `test/TabularLearning.hs` in the source distribution.

## Neural derivative and update boundaries

The neural package tests dense input and parameter VJPs, categorical Jacobians, and selected-action score gradients with central finite differences.

The typed parametric reverse fixtures use literal rational equality to check cotangent zero, additive identity, associativity, commutativity, scalar distributivity, primitive VJP zero/additivity/homogeneity, composition identities and associativity, independent parameter products, tensor products, and identity/input/parameter diagonals. Nonlinear composition and input-diagonal fixtures use central differences. Failure fixtures cover primitive evaluation, composition, tensor, shared parameters, and diagonal addition. These fixtures test declared `CotangentSpace` operations and supplied primitives; the generic constructor does not prove their module or pullback laws.

The finite reverse-program fixtures additionally check represented primal and cotangent layouts, structural owner products, duplicate independent owner rejection, matching shared ownership, exact preparation boundaries, deterministic traversal errors and reports, typed stored and recomputed tapes, repeated tape use, composition and tensor laws through explicit pair bijections, and two- and three-way diagonal accumulation. A heterogeneous nonlinear program checks every represented input and parameter coordinate under both tape policies with step `1e-6 * max 1 |x|`, absolute tolerance `2e-10`, and relative tolerance `2e-8`. A literal floating reassociation counterexample remains unequal. Compile-fail fixtures protect typed intermediates, parameter products, tape endpoints, and constructor opacity.

The tests also check REINFORCE, actor-critic, replay, target synchronization, and DQN update fixtures. The floating checks use explicit tolerances. They provide local evidence for supplied VJPs. They do not prove all-input derivatives, general autodiff, checkpoint optimality, or training convergence.

**Executable evidence:** [`backends/markovian-neural/test`](https://github.com/josephjohncox/Markovian/tree/main/backends/markovian-neural/test).

## Exact availability to neural mask boundary

A neural `ActionMask` validates a positive complete output width and nonempty ordered active indices. Its Boolean flags describe membership in global output order. The ordered index list separately controls deterministic argmax tie breaking. Equal flags therefore do not imply equal represented masks. Gather and scatter inspect no more than the expected input plus one overrun witness, so an infinite input is rejected without an unrestricted `length`. Scatter also rejects nonfinite active values.

The bridge checks two independent obligations:

1. the policy or dense output width equals the exact global action cardinality;
2. the bridge layout and compiled model have the same represented global action order.

For a continuing state, membership agrees exactly with compiled availability, and availability order is unchanged. A terminal state has no mask. Complete bridge compilation uses explicit state, cumulative action-entry, and conservative traversal-work limits. It preflights the complete model before it returns a collection. Policy and DQN consumers gather before softmax or argmax. Policy scattering writes literal positive `0.0` on every unavailable parameter row. Nominal roles prevent representational action relabelling across the root, finite-index, output-layout, and support-mask witnesses.

A numeric-zero counterexample uses negative available Q-values. Multiplication changes an unavailable value to zero, which can exceed every available value. The structural gather excludes that output. Bridge differential fixtures also compare policy score gradients with central differences and a masked one-step DQN maximum with an explicitly converted exact rational maximum.

These fixtures do not prove feature-map consistency, differentiation through masks, neural convergence, or a general masked-network VJP.

**Executable evidence:** [`backends/markovian-neural/test/ActionMask.hs`](https://github.com/josephjohncox/Markovian/blob/main/backends/markovian-neural/test/ActionMask.hs) and [`backends/markovian-neural-bridge/test/Main.hs`](https://github.com/josephjohncox/Markovian/blob/main/backends/markovian-neural-bridge/test/Main.hs).

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

## Exact state-payoff push-pull laws

For a normalized channel `K : X -> Y`, an exact state `p` on `X`, and a signed rational payoff `u` on `Y`, the payoff pullback is

\\[
(K^{\ast}u)(x)=\sum_y K(y\mid x)u(y).
\\]

The fixtures check identity and contravariant composition:

\\[
\mathrm{id}^{\ast}u=u,
\\]

\\[
(L\circ K)^{\ast}u=K^{\ast}(L^{\ast}u).
\\]

They also check the exact pairing law:

\\[
\langle K_{\ast}p,u\rangle_Y=\langle p,K^{\ast}u\rangle_X.
\\]

The evidence includes signed hand calculations, empty finite pullback, reordered layouts, complete-table validation, object mismatch errors, and singleton state-source validation. These equations are fixture laws, not a machine-checked theorem for every scalar type. The implementation specializes payoff values to `Rational`; it does not claim that the nonnegative scalar hierarchy has additive inverses.

Payoff pullback uses no prior, support restriction, division, or posterior. The following Bayesian laws are separate.

**Executable evidence:** [`PushPullExact`](https://github.com/josephjohncox/Markovian/blob/main/test/PushPullExact.hs).

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

## Fixed-batch finite and stationary differentials

The finite execution fixtures check batch congruence, material conservation, exact supplier delay, complete reachable closure, and same-demand cost/successor pairing. Independent recursive calculations agree with both the finite-horizon oracle and a fixed-policy evaluator.

The stationary module checks the exact forward difference

\\[
c_i(\ldots,R_i)=C_i(\ldots,R_i+1)-C_i(\ldots,R_i)
\\]

and Theorem 1's equivalent weak and strict discrete inequalities. An independently enumerated two-stage law agrees with the implemented shortfall recursion. A fixture pins a discrete plateau where the weak condition holds and the strict condition does not. The unit-batch fixture checks `S=R+1`.

These are finite exact fixtures under the reported conditioned demand and grid. They do not prove an unbounded minimizer, continuous equality, average-cost convergence, or optimality of a constant `(R,nQ)` policy for the finite-horizon oracle. A pinned counterexample has different finite-horizon and stationary selections.

**Executable evidence:** [`FixedBatchRnQ.hs`](https://github.com/josephjohncox/Markovian/blob/main/test/FixedBatchRnQ.hs).

## Circuit interpretation laws

For the exact interpreter `⟦-⟧`, the fixtures check

\\[
\lbrack\\!\lbrack\mathrm{id}\rbrack\\!\rbrack=\mathrm{id},
\\]

\\[
\lbrack\\!\lbrack g\circ f\rbrack\\!\rbrack=
\lbrack\\!\lbrack g\rbrack\\!\rbrack\circ\lbrack\\!\lbrack f\rbrack\\!\rbrack,
\\]

\\[
\lbrack\\!\lbrack f\otimes g\rbrack\\!\rbrack=
\lbrack\\!\lbrack f\rbrack\\!\rbrack\otimes\lbrack\\!\lbrack g\rbrack\\!\rbrack.
\\]

They also check symmetry involution, associator and unitor round trips, copy cocommutativity, copy counits, stochastic discard naturality, pentagon, triangle, hexagon, and structural naturality.

The deterministic compiler preserves identity, composition, products, pairing, and projections. Pairing compiles through copy followed by tensor.

**Executable evidence:** [`testHomomorphismAndCoherence` and `testMonoidalCoherence`](https://github.com/josephjohncox/Markovian/blob/main/test/StochasticCircuit.hs#L194-L369), plus [`testDeterministicCompilation`](https://github.com/josephjohncox/Markovian/blob/main/test/StochasticCircuit.hs#L465).

## Bounded circuit costs and deterministic rewrites

The cost interpreter tests additive caller-owned work for sequence and tensor. It also tests the elaborations

\\[
\operatorname{share}(f)=f;\operatorname{copy}
\\]

and

\\[
\operatorname{fanout}(f,g)=\operatorname{copy};(f\otimes g).
\\]

A raw syntax node is charged before descent. Exceeding the node, declared-work, layout-cardinality, matrix-cell, or owner-entry limit returns no report. Convex cost includes every represented branch, including a zero-coefficient branch. These rules define repository accounting, not categorical complexity.

The exact rewrite checker tests

\\[
\mathrm{id};f=f,
\qquad
f;\mathrm{id}=f,
\\]

\\[
(f;g);h=f;(g;h),
\\]

and deterministic copy naturality. It requires represented endpoint equality and literal row-major matrix equality in addition to labelled extensional equality. The four represented Boolean total functions and a compiled deterministic term exercise the copy fixture.

The counterexamples are part of the contract: a shared fair coin differs from two executions; stochastic provenance remains stochastic for a Dirac denotation; zero declared cost grants no certificate; reordered primitive layouts fail exact checking; and explicit checker exhaustion returns no witness. Compile-fail evidence keeps candidate and checked constructors opaque and rejects stochastic deduplication.

Mac Lane, Chapter I §1, supplies the category assumptions for identity and associativity. Mac Lane, Chapter VII §§1–2, and Selinger supply the stated monoidal setting. Fritz §3 supplies the deterministic restriction on copy-preserving maps in a Markov category. None of these sources defines Markovian's cost report or proves every Haskell fixture.

**Executable evidence:** [`CircuitCostRewrite.hs`](https://github.com/josephjohncox/Markovian/blob/main/test/CircuitCostRewrite.hs) and [`check-circuit-purity`](https://github.com/josephjohncox/Markovian/blob/main/scripts/check-circuit-purity).

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

## Finite alternating protocol laws

For the represented finite fixtures, write `≈` for a successful bounded `observationallyEqual` check: exact equality of external prefix-closed play sets under labelled-equivalent endpoints. The tests check copycat identities and one representative associativity equation:

\\[
\operatorname{copycat}_A;\sigma\approx\sigma,
\qquad
\sigma;\operatorname{copycat}_B\approx\sigma,
\\]

\\[
(\sigma;\tau);\upsilon\approx\sigma;(\tau;\upsilon).
\\]

They also check reflexivity, symmetry, transitivity, and congruence for successful representative compositions. Composition synchronizes literal middle move identities and hides them. It then validates prefix closure, exhaustive Opponent receptivity, and one total Player response again. Composition is partial: a hidden internal interaction can leave an invalid visible Player position, which is rejected. Work or result exhaustion returns no strategy. These fixtures do not prove closure, unrestricted associativity, or category laws.

This equality is not contextual equivalence, an AJM quotient, payoff equivalence, or bisimulation. The fixtures do not establish a `Category` instance or universal associativity. The arena has no justification pointers or views, so the laws do not establish Hyland--Ong semantics, AJM semantics, innocence, bracketing, full abstraction, open-game best response, or equilibrium.

**Executable evidence:** `test/GameCore.hs` and `scripts/check-game-core-boundary`.

## Finite open-game laws and counterexamples

For a represented context `(x,k)`, pure equilibrium is checked by

\\[
(\sigma,\sigma)\in B(x,k).
\\]

Sequential play and coplay, incumbent continuation transformation, and tensor's fixed-other-incumbent continuations follow Definitions 9 and 12 of Ghani and colleagues on the represented finite carriers. Concrete optic identity and associativity fixtures use labelled extensional equality. Open-game identity uses an explicit strategy-unit bijection; no `Category` instance or literal nested-product equality is claimed.

Observational equality checks play, coplay, owner support, and every best-response membership under a supplied finite strategy bijection. A counterexample gives two games with identical play and coplay but different best-response relations; equality rejects them. Matching pennies has no pure equilibrium. A sequential entry-deterrence fixture retains a non-credible threat, showing that the enumerator is not subgame perfect.

All two-player `2 x 2` payoff tables over the represented utility carrier `{0,1}` are compared with independent unilateral-deviation enumeration. This is exhaustive differential evidence for that finite carrier, not a universal equilibrium theorem.

**Executable evidence:** `test/FiniteOpenGames.hs` and the two open-game golden reports.

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
- [MDPs, POMDPs, inventory control, and learning](references.md#mdps-pomdps-inventory-control-and-learning)
- [Numerical computation](references.md#numerical-computation)
