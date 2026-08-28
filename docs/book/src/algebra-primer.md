# Algebra behind the implementation

This chapter explains the algebraic structures that appear in Markovian. It assumes familiarity with functions, finite sums, and ordinary matrix multiplication. The purpose is not to replace an algebra text. It is to explain why the library asks for different scalar capabilities at different layers.

## Operations and laws

An algebraic structure is a carrier together with operations and laws. The laws matter more than the names of the operations.

A **monoid** consists of a set `M`, an associative operation, and an identity element. Writing the operation as multiplication:

\\[
(ab)c=a(bc),
\qquad
1a=a=a1.
\\]

A **commutative monoid** also has an order-independent operation. Finite sums require a commutative additive monoid because the result must not depend on enumeration order.

A **semiring** has two monoid structures. Addition is commutative, multiplication need not be, multiplication distributes over addition, and zero annihilates multiplication:

\\[
(a+b)+c=a+(b+c),
\qquad
a+b=b+a,
\qquad
a+0=a,
\\]

\\[
(ab)c=a(bc),
\qquad
1a=a=a1,
\\]

\\[
a(b+c)=ab+ac,
\qquad
(a+b)c=ac+bc,
\qquad
0a=0=a0.
\\]

A semiring need not have subtraction. This omission is deliberate. Probabilities, path counts, and reachability do not need negative values.

Markovian's `Semiring` class states these laws as an instance contract. Haskell cannot prove them from the methods alone, so the test suite checks finite witnesses for concrete instances.

## Four useful semirings

The same matrix code can express different computations by changing the scalar semiring.

| Carrier | Addition | Multiplication | Interpretation |
| --- | --- | --- | --- |
| Natural numbers | `+` | `×` | Count paths |
| Booleans | `or` | `and` | Record reachability |
| Nonnegative reals or rationals | `+` | `×` | Aggregate weighted or probabilistic paths |
| Tropical values | `min` | `+` | Select shortest path cost |

In the Boolean semiring, adding two reachable paths still gives `true`. In the probability semiring, their masses add. In the tropical semiring, the cheaper path wins. The matrix shape is unchanged, but the meaning of aggregation changes.

This is why a raw semiring matrix is more general than a stochastic matrix. A stochastic matrix adds normalization and nonnegativity conditions that Boolean and tropical matrices do not share.

See [Mohri's semiring path treatment](references.md#mohri-semiring-path-algorithms) for a direct algorithmic development.

## Matrices over a semiring

For finite objects `X` and `Y`, a matrix `A : X → Y` assigns a scalar `A(x,y)` to every source-target pair. Markovian uses source rows and target columns.

If `A : X → Y` and `B : Y → Z`, composition is matrix multiplication:

\\[
(B\circ A)(x,z)=\sum_{y\in Y}A(x,y)B(y,z).
\\]

The intermediate label `y` is hidden. Multiplication combines one path segment through `y`; addition aggregates all possible intermediate labels.

Associativity follows from semiring laws:

\\[
C\circ(B\circ A)=(C\circ B)\circ A.
\\]

Entrywise, both sides reduce to the same finite sum:

\\[
\sum_{y,z}A(x,y)B(y,z)C(z,w).
\\]

The identity matrix uses the semiring's zero and one:

\\[
I_X(x,x')=
\begin{cases}
1 & x=x',\\\\
0 & x\ne x'.
\end{cases}
\\]

These facts explain the category instance for raw matrices. They do not use division, normalization, order, or subtraction.

## Vectors and semimodules

A row vector on `X` is a function `v : X → S`. It can be multiplied by a matrix:

\\[
(vA)(y)=\sum_x v(x)A(x,y).
\\]

Over a semiring, vectors form a **semimodule**, the subtraction-free analogue of a vector space. Vector addition and scalar multiplication satisfy familiar distributive laws, but additive inverses need not exist.

A finite probability distribution is a special vector over nonnegative scalars:

\\[
p(x)\ge 0,
\qquad
\sum_x p(x)=1.
\\]

A stochastic matrix is a matrix whose every row is such a distribution:

\\[
K(y\mid x)\ge 0,
\qquad
\sum_y K(y\mid x)=1.
\\]

The normalization proof is extra structure. Raw matrix addition can destroy it because two normalized rows add to total mass `2`.

## Why exact nonnegative rationals form a separate layer

Exact probability needs more than the semiring laws. Markovian's exact stochastic construction needs:

- decidable zero
- positivity for every nonzero scalar
- exact division by nonzero values
- zero-sum-freeness
- no zero divisors
- commutative multiplication

These capabilities support normalization, positive support, and conditioning. They are collected in `ExactNonNegativeSemifield` and instantiated by `NonNegativeRational`.

A **semifield** permits division by nonzero values but does not require additive inverses. Nonnegative rationals have exactly this shape. They support Bayes' rule without allowing a negative probability.

The signed `Rational` type appears separately in exact policy iteration. Gaussian elimination needs subtraction:

\\[
(I-\gamma P)V=r.
\\]

That equation lives in a field-like signed calculation. Its result is checked before being wrapped as an exact reward. Markovian does not pretend that subtraction belongs to the nonnegative probability scalar.

## Direct sum and biproduct

For finite objects `X` and `Y`, the direct sum `X ⊕ Y` is a tagged disjoint union. A block matrix on direct sums has the familiar form:

\\[
F=
\begin{bmatrix}
A & B\\\\
C & D
\end{bmatrix}.
\\]

Injections and projections select the corresponding blocks. Raw matrix addition, a zero matrix, and direct sum together give the finite matrix category a biproduct structure.

The biproduct combines categorical product-like and coproduct-like behavior. It does **not** mean that stochastic matrices have unrestricted biproducts. Their zero and addition operations generally violate row normalization.

## Tensor product

The tensor of `A : X → Y` and `B : U → V` acts on product objects:

\\[
(A\otimes B)((x,u),(y,v))=A(x,y)B(u,v).
\\]

For stochastic channels, multiplication of masses expresses independent conditional execution. Given the input pair `(x,u)`, the output pair mass factors:

\\[
P(y,v\mid x,u)=P(y\mid x)P(v\mid u).
\\]

Tensor is not the same operation as direct sum.

- Direct sum chooses one tagged component.
- Tensor combines both components.
- Matrix addition aggregates alternatives with the same boundary.

Confusing these operations produces incorrect probability and incorrect wiring.

## Convex combinations

A convex combination uses nonnegative coefficients that sum to one:

\\[
\lambda_i\ge 0,
\qquad
\sum_i\lambda_i=1.
\\]

Channels with the same source and target can be mixed pointwise:

\\[
\left(\sum_i\lambda_iK_i\right)(y\mid x)=
\sum_i\lambda_iK_i(y\mid x).
\\]

Normalization is preserved because the coefficients themselves are normalized. This is different from raw matrix addition.

Convex mixing can represent a hidden controller choice. Reusing the same mixture index on two outputs preserves correlation. Mixing both outputs independently creates cross terms. The distinction is developed in [Derived categorical and mathematical insights](categorical-insights.md#4-separate-affinity-exposes-hidden-versus-independent-choices).

For the abstract laws of convex spaces, see [Fritz on convex spaces](references.md#fritz-convex-spaces) and [Jacobs on convexity and effects](references.md#jacobs-convexity-and-effects).

## Involution, dagger, and compact algebra

An involutive semiring has an operation that reverses products and is its own inverse:

\\[
\overline{ab}=\overline b\\,\overline a,
\qquad
\overline{\overline a}=a.
\\]

This supports matrix dagger:

\\[
A^{\dagger}(y,x)=\overline{A(x,y)}.
\\]

Raw finite matrices then support dagger, cups, caps, and finite trace. These operations are algebraically valid at the raw matrix layer.

They do not generally preserve stochastic normalization. Transposing a row-stochastic matrix produces a column-stochastic matrix, not necessarily another row-stochastic matrix. This is why Markovian does not expose one universal reversal operation.

See [Finite objects and semiring matrices](matrices.md), [Stochastic, deterministic, and convex matrices](matrix-refinements.md), and the exact equations in the [law catalogue](laws-and-boundaries.md#raw-semiring-matrix-category).

## Floating-point arithmetic is not the same algebra

IEEE `Double` operations approximate real arithmetic. Addition is not associative:

\\[
(a+b)+c\ne a+(b+c)
\\]

for some representable values. Underflow can also turn a positive mathematical value into floating zero.

The neural and sampled layers therefore report approximation contracts and use finite-difference or exact-reference comparisons. They do not inherit exact semiring laws merely because their source formulas look algebraic.

See [Higham](references.md#higham-floating-point-stability) and [Goldberg](references.md#goldberg-floating-point-arithmetic).

## Where the structures live

| Structure | Markovian layer | What it enables |
| --- | --- | --- |
| Semiring | Raw matrices | Composition, identity, addition |
| Commutative semiring | Raw matrices | Symmetric tensor |
| Involutive semiring | Raw matrices | Dagger |
| Exact nonnegative semifield | Exact stochastic matrices | Normalization and conditioning |
| Convex scalar | Exact stochastic matrices | Valid channel mixtures |
| Signed rationals | Exact control solver | Gaussian elimination |
| Checked `Double` | Sampled and neural packages | Approximate execution and learning |

The progression is capability-driven. An operation is placed at the weakest layer whose laws make it valid.

## Reading route

1. Read [Leinster's free *Basic Category Theory*](references.md#leinster-basic-category-theory) for monoids, categories, products, and universal properties in context.
2. Read [Mohri](references.md#mohri-semiring-path-algorithms) for semirings as path algebras.
3. Read [Finite objects and semiring matrices](matrices.md) for Markovian's concrete API.
4. Use the [law catalogue](laws-and-boundaries.md#raw-semiring-matrix-category) to connect each equation to an executable fixture.
