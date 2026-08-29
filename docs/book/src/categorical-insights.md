# Derived categorical and mathematical insights

This chapter records deductions that become visible when the semantic layers are kept separate. These are not claims of historical priority. Most ingredients are standard. The useful contribution is their placement in one checked finite implementation.

Each section distinguishes an implemented fact, a deduction, and a boundary. Definitions and background appear in [Algebra behind the implementation](algebra-primer.md), [Category theory behind the interfaces](category-primer.md), and [Categorical probability: the bridge](categorical-probability.md).

## 1. Algebraic structure belongs at different refinement levels

### Implemented fact

Raw semiring matrices have entrywise addition, direct sums, tensor, dagger under an involution, compact cups and caps, and finite trace.

Normalized stochastic matrices retain identity, composition, tensor, copy, and discard. They do not retain raw addition, transpose, cup, cap, or trace.

### Deduction

The semantic tower is not a list of unrelated APIs. It is a refinement diagram in which each proof removes operations that do not preserve the refinement.

\\[
\text{deterministic}
\longrightarrow
\text{stochastic}
\longrightarrow
\text{nonnegative raw matrix}
\longrightarrow
\text{semiring matrix}.
\\]

The arrows forget evidence. They do not manufacture stronger structure.

This placement answers a common design question. An operation belongs at the weakest layer whose invariants it preserves. Matrix transpose belongs below normalization. Convex mixture belongs above normalization but requires exact coefficient evidence.

### Boundary

The implementation does not claim a universal lattice of categorical structures. The diagram describes these finite types and forgetful maps.

See [Fritz on Markov categories](references.md#fritz-markov-categories) and [Selinger on graphical languages](references.md#selinger-graphical-languages).

## 2. The deterministic fragment is the copy-preserving core

### Implemented fact

Every stochastic channel preserves discard:

\\[
!_Y\circ K=!_X.
\\]

Only deterministic channels are allowed to use copy naturality:

\\[
\Delta_Y\circ f=(f\otimes f)\circ\Delta_X.
\\]

### Deduction

The deterministic refinement identifies the cartesian core inside the finite stochastic category. It is not merely an optimization tag.

Copy naturality says that computing once and copying equals copying the input and computing twice. This is true exactly when the output is determined by the input. A stochastic channel can introduce fresh randomness, so the two sides can have different correlations.

The purity index therefore has a categorical meaning. It records whether a morphism is a comonoid homomorphism.

### Boundary

The implementation proves determinism through one-hot finite matrices and deterministic syntax. It does not decide semantic determinism for arbitrary Haskell functions or continuous kernels.

The standard abstract treatment is [Fritz, §§2–3](references.md#fritz-markov-categories).

## 3. Shared randomness is a structural resource

### Implemented fact

For a stochastic circuit `c`,

\\[
\operatorname{share}(c)=\Delta\circ c,
\\]

while fanout expands as

\\[
\operatorname{fanout}(c,c)=(c\otimes c)\circ\Delta.
\\]

A fair-coin fixture gives diagonal mass `1/2` for sharing and four masses `1/4` for fanout.

### Deduction

Correlation is not metadata attached after evaluation. It is created by the wiring diagram.

A copied stochastic result represents one latent event with multiple consumers. Two circuit occurrences represent two conditionally independent events. This gives the syntax a small correlation calculus without adding a separate correlation coefficient or copula API.

### Boundary

Tensor expresses independence only relative to the represented inputs and primitive interpreter. It does not assert unconditional independence in every surrounding circuit.

See [Markov categories](references.md#fritz-markov-categories) and the string-diagram account in [Selinger](references.md#selinger-graphical-languages).

## 4. Separate affinity exposes hidden versus independent choices

### Implemented fact

Composition and tensor are affine in either argument separately:

\\[
L\circ\sum_i\lambda_iK_i=
\sum_i\lambda_i(L\circ K_i),
\\]

\\[
\left(\sum_i\lambda_iK_i\right)\otimes L=
\sum_i\lambda_i(K_i\otimes L).
\\]

They are not jointly affine over one correlated index:

\\[
\sum_i\lambda_i(K_i\otimes L_i)
\ne
\left(\sum_i\lambda_iK_i\right)
\otimes
\left(\sum_i\lambda_iL_i\right).
\\]

### Deduction

A convex coefficient can denote a latent branch selection. Reusing the same index on both sides preserves correlation. Mixing each side first creates two independent branch selections and cross terms.

This is the convex analogue of shared draw versus repeated execution. The same structural distinction appears in mixture models, randomized controllers, and ensemble selection.

### Boundary

The inequality is a counterexample statement, not a claim that the sides always differ. They can agree in degenerate cases.

See [Fritz on convex spaces](references.md#fritz-convex-spaces) and [Jacobs on convexity and effects](references.md#jacobs-convexity-and-effects).

## 5. Bayesian inversion is dagger-like only after adding state

### Implemented fact

For a prior `p`, channel `K`, and pushforward `q=pK`, the inverse satisfies

\\[
p(x)K(y\mid x)=q(y)K^{\sharp}_p(x\mid y)
\\]

on positive supports. It reverses compatible composition, preserves independent tensor, and recovers the supported forward channel after double inversion.

### Deduction

Consider an object as a finite carrier with a chosen prior `(X,p)`. Consider a channel admissible from `(X,p)` to `(Y,q)` only when `q=pK`.

On positive supports and modulo almost-sure equality, Bayesian inversion behaves like a contravariant involution:

\\[
(L\circ K)^{\sharp}_p=
K^{\sharp}_p\circ L^{\sharp}_q,
\\]

\\[
(K^{\sharp}_p)^{\sharp}_q=
K\vert_{X_p\to Y_q}.
\\]

Thus, state is not an optional parameter around a dagger. State is the data that makes the dagger-like equations meaningful.

### Boundary

A Bayesian inverse is not an ordinary inverse. In general,

\\[
K^{\sharp}_p\circ K\ne\mathrm{id}
\\]

and

\\[
K\circ K^{\sharp}_p\ne\mathrm{id}.
\\]

The implementation does not add a `Dagger` instance for stochastic matrices or even for `BayesianChannel`. Support changes and prior witnesses remain explicit.

See [Cho and Jacobs, §§3–5](references.md#cho-and-jacobs-disintegration-and-bayesian-inversion) and [Fritz, §§11 and 13](references.md#fritz-markov-categories).

## 6. Almost-sure equality is the correct quotient for inference

### Implemented fact

Two inverse candidates can differ on observations with zero pushforward mass. The implementation compares them by `almostSureEqual` under the output prior.

### Deduction

The posterior row for an impossible observation is not missing mathematical information. It is outside the observable part of the model.

Quotienting by almost-sure equality removes arbitrary zero-evidence fillers while preserving every expectation and postcomposition observable under the prior.

This explains why extensional matrix equality is too strong for Bayesian uniqueness. It also explains why the implementation rejects zero-evidence conditioning instead of selecting a filler posterior.

### Boundary

Almost-sure equality always names a prior. Changing the prior can change the equivalence relation.

See [Cho and Jacobs §4](references.md#cho-and-jacobs-disintegration-and-bayesian-inversion) and [Fritz §13](references.md#fritz-markov-categories).

## 7. Raw trace and Bellman feedback solve different equations

### Implemented fact

Raw matrix trace contracts an equal finite index:

\\[
\operatorname{Tr}^{U}(F)_{xy}=
\sum_u F_{(x,u),(y,u)}.
\\]

The Bellman solver instead seeks a fixed point of an affine operator:

\\[
V=T(V),
\\]

with a contraction discount and an explicit stopping bound.

### Deduction

Both operations can appear as a loop in a string diagram, but they close different mathematics.

Trace performs finite summation along a matched wire. Bellman evaluation solves recursive value dependence. Importing compact trace into normalized stochastic channels would silently replace a fixed-point problem with index contraction.

### Boundary

The current open-system interpreter rejects cycles. It does not choose either operation implicitly.

See [Joyal–Street–Verity](references.md#joyal-street-and-verity-traced-monoidal-categories) for categorical trace and [Puterman Chapter 6](references.md#puterman-markov-decision-processes) for discounted fixed points.

## 8. Acyclic open evaluation is sum-product elimination

### Implemented fact

For a boundary-functional DAG, each edge contributes one local stochastic factor. The complete denotation can be written schematically as

\\[
D_G(y\mid x)=
\sum_{h}
\prod_{e\in E}
K_e\bigl(a_{\mathrm{out}(e)}\mid a_{\mathrm{in}(e)}\bigr).
\\]

The sum ranges over hidden apex assignments consistent with the named input `x` and output observation `y`.

The implementation does not enumerate the complete apex assignment. It eliminates dead values as soon as no later edge or output can read them.

### Deduction

The live frontier is a variable-elimination boundary. Its width, not only the edge count, controls the largest intermediate assignment space.

Ready-edge schedule independence follows from the same sum-product algebra when both schedules respect dependencies. Finite sums can be reordered, and independent local factors commute over exact nonnegative rationals.

The stored-value rule also gives graphical-model sharing. Multiple consumers use one variable. Separate edge occurrences contribute separate factors.

### Boundary

The fixture suite checks representative schedules and a complete-valuation differential equation. It is not a machine-checked proof for every finite DAG. Runtime can remain exponential in frontier width.

See [Koller and Friedman, Chapters 9–10](references.md#koller-and-friedman-sum-product-elimination) and [Baez and Courser](references.md#baez-and-courser-structured-cospans).

## 9. Finite layout is representation, not semantics

### Implemented fact

Finite objects retain an explicit order for dense storage. Semantic equality can ignore this order. Matrix equivalence compares labelled entries, while `sameFiniteLayout` checks representation order.

Pushout composition can choose differently nested carrier representations. Open denotations compare after named-boundary reindexing.

### Deduction

A finite layout behaves like a chosen coordinate system. Permuting coordinates changes the dense array but not the labelled morphism.

This separation lets exact laws survive compilation and pushout composition without pretending that all concrete arrays are literally equal.

### Boundary

A backend cannot ignore layout. It must either require `sameFiniteLayout` or apply an explicit reindexing matrix.

See [Mac Lane on categorical isomorphism and coherence](references.md#mac-lane-categories-and-monoidal-coherence) and [Fong–Spivak on compositional representations](references.md#fong-and-spivak-applied-category-theory).

## 10. Four reversals live in four different contexts

### Implemented fact

Markovian has four separate operations:

1. conjugate transpose of a raw matrix
2. prior-indexed Bayesian inversion
3. reversal of structured-cospan boundary legs
4. manual reverse derivatives through neural-network VJPs

### Deduction

The word “reverse” hides four distinct dependencies.

- Matrix dagger needs an involutive scalar.
- Bayesian inversion needs a state and positive evidence.
- Boundary reversal needs an interface presentation.
- A reverse derivative needs a primal point, a local derivative, an output cotangent, and additive cotangent accumulation.

A universal reversal interface would erase exactly the data that distinguishes the operations. An adjoint functor is a fifth mathematical use of “adjoint,” but it is a hom-set correspondence rather than one of these value-level operations.

### Boundary

No equation in one context transfers automatically to any of the others. In particular, backpropagation is not Bayesian inversion and a neural VJP does not add a dagger to stochastic kernels.

See [Categorical structure of learning and neural networks](categorical-learning.md#four-meanings-of-adjoint-or-reverse).

## 11. Exact semantics acts as an executable specification

### Implemented fact

Circuit interpretation, dense lowering, POMDP filtering, dynamic programming, and selected backends are compared with exact rational references.

### Deduction

The exact layer is more than a slow backend. It is an executable specification that fixes support, timing, correlation, and algebraic laws before approximation.

A numerical backend can then state a relation to this reference. Examples include exact equality, max-norm error, seeded trace equality, or equality in distribution.

### Boundary

Exact reference agreement on bounded fixtures does not prove scalability, numerical stability, or statistical calibration for a larger backend.

See [Higham](references.md#higham-floating-point-stability) and [Goldberg](references.md#goldberg-floating-point-arithmetic).

## 12. One diagonal governs sharing forward and accumulation backward

### Implemented fact

Circuit sharing uses one forward diagonal after one computation. Dense neural references compute VJPs and aggregate one complete parameter gradient before applying an atomic update.

### Deduction

For differentiable deterministic structure, the reverse derivative of the diagonal adds cotangents:

\\[
R[\Delta_X](x,\bar x_1,\bar x_2)=\bar x_1+\bar x_2.
\\]

The same structural node therefore has two compatible readings:

- forward: one value has multiple consumers;
- reverse: every consumer contributes to one shared source sensitivity.

This connects probabilistic correlation accounting with neural parameter-sharing accounting. Replacing sharing by repeated execution can change both the joint distribution and the gradient.

### Boundary

The reverse equation requires differentiable additive structure. It is not a law of arbitrary stochastic kernels. Score-function estimators remain necessary at discrete sampling nodes.

See [Categorical structure of learning and neural networks](categorical-learning.md#diagonals-parameter-sharing-and-gradient-accumulation) and [Cockett and colleagues](references.md#cockett-and-colleagues-reverse-derivatives).

## 13. Exact support can compile approximate action masks

### Implemented fact

Exact finite models retain validated action availability and positive support. Neural policies and DQN transitions use explicit nonempty ordered action masks.

### Deduction

A lowering pass can derive each neural action mask from the exact model's available-action witness. Softmax normalization, score gradients, greedy selection, and bootstrap maxima then range over exactly the actions permitted by the semantic model.

This is a near-free correctness gain: the exact layer already contains the support evidence. Reusing it prevents unavailable actions from receiving probability or entering a target maximum.

### Boundary

The current packages test common masks but do not yet expose one public compiler from every exact MDP state to a neural observation and mask. Feature extraction and state abstraction remain user-supplied and can still merge states with incompatible action sets.

## 14. Approximate interpreters should satisfy commuting-square tests

### Implemented fact

Exact kernels, dense rational lowering, sampled execution, CUDA application, and selected neural updates already have bounded differential fixtures.

### Deduction

Each approximate backend can be tested as a square:

\\[
\begin{array}{ccc}
X_{\mathrm{exact}} & \xrightarrow{F_{\mathrm{exact}}} & Y_{\mathrm{exact}} \\\\
\downarrow L_X && \downarrow L_Y \\\\
X_{\mathrm{approx}} & \xrightarrow{F_{\mathrm{approx}}} & Y_{\mathrm{approx}}.
\end{array}
\\]

The proof obligation is not literal equality. It is the backend's declared observation relation between `L_Y(F_exact x)` and `F_approx(L_X x)`.

This pattern composes. If two adjacent approximate squares satisfy compatible error contracts, the composite has an explicit accumulated obligation. It supplies a disciplined route from denotational laws to backend tests without claiming that floating arithmetic inherits exact equality.

### Boundary

Error composition is backend-specific. Absolute bounds, relative bounds, probabilistic confidence statements, and optimizer convergence cannot be merged through one universal rule.

## Summary

The strongest deductions come from refusing to collapse adjacent layers:

- deterministic versus stochastic separates copy-preserving maps from fresh randomness
- shared versus repeated execution separates correlation from independence
- convex mixing versus tensor separates shared latent choice from independent choice
- prior-decorated Bayes versus matrix dagger separates inference from algebraic transpose
- raw trace versus Bellman fixed points separates finite contraction from recursion
- semantic support versus finite layout separates meaning from storage
- structured cospan composition versus acyclic denotation separates topology from execution
- forward diagonals versus reverse accumulation connects sharing with tied-parameter gradients
- exact availability versus approximate masks turns support evidence into backend constraints
- exact denotation versus approximate observation turns semantic laws into commuting-square tests

These distinctions are standard mathematics applied as software boundaries. Their combination gives a precise guide for future extensions.
