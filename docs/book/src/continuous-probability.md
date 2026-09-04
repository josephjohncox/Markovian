# Restricted continuous probability

Continuous probability is optional. It does not change the finite root semantic core.

## Exact represented family

`markovian-continuous` supports finite discrete spaces, the real Borel space, and finite products. These witnesses do not enumerate sigma algebras. Only closed affine real maps are executable. No constructor accepts an arbitrary Haskell callback as a measurable map.

A represented real law has this form:

\\[
X=c+\sum_i q_i U_i,
\\]

All data are rational. Each `U_i` is uniform on a compact rational interval. Equal noise owners mean shared noise. Different owners mean independent noise. The package uses these exact uniform moments:

\\[
E[U^k]=\frac{u^{k+1}-l^{k+1}}{(k+1)(u-l)}.
\\]

Exactness applies only to the supported operations. It does not supply an event oracle. Public list constructors inspect at most their raw limit plus one cell. They do this before filtering, sorting, or duplicate checks. Zero coefficients consume traversal and work budget. The active rational limit covers support products, moments, likelihoods, posterior weights, and posterior quotients.

Bivariate polynomial integration uses one operation-wide meter. The meter charges each raw expansion pair before it constructs the pair. It also charges monomial comparisons, power merges, coefficient operations, cancellations, canonical combinations, moment operations, and final sums. Separate limits bound cumulative raw pairs and the largest live canonical expansion. The report gives every counter and the largest intermediate numerator or denominator size.

The canonical expansion has a stable monomial order. Each generated pair enters this expansion immediately. A raw-pair, canonical-term, work, or rational failure returns no expansion and no report. The tests admit each exact boundary and reject its one-below boundary.

## Correlation

Joint laws retain noise ownership. Complete test fixtures use nonzero powers on both coordinates. An independent raw symbolic enumerator checks shared-owner and independent-owner results. Cancellation and duplicate-monomial fixtures check deterministic canonicalization.

For `U` uniform on `[0,1]`, the test suite checks these values:

\\[
E[U U]=1/3,\qquad E[U_1U_2]=1/4.
\\]

A future dynamic adapter must construct one joint reward, successor, and post-transition observation. It must first inspect terminal status. It must pay terminal value once. A zero horizon must not draw noise. Each transition applies one reward and one discount. Lead time must remain explicit.

## Kernels

The executable kernel family has this form:

\\[
K(x)=a x+b+\sum_i c_iU_i.
\\]

Composition checks owner collisions and declared budgets. Alpha-renaming rejects duplicate source mappings, duplicate targets, and collisions with an unmapped owner. Thus, the package does not supply an unrestricted `Category` instance.

### Joint affine Gate A

`ExactJointAffineKernel` represents two closed coordinates over one real input:

\\[
R(x)=a_Rx+b_R+\\sum_i c_iU_i,\\qquad
S(x)=a_Sx+b_S+\\sum_i d_iU_i.
\\]

One bounded table stores each local owner, its compact rational interval, and the coefficients `c_i` and `d_i`. Admission counts raw rows before canonicalization and rejects duplicate owners before it removes rows where both coefficients are zero. The owner, input, reward, and successor labels have nominal roles. A partial alpha-renaming has the same owner-scope type. `reScopeJointAffineKernel` changes that type only after a complete injective map names every retained owner. Thus `[]` cannot re-scope a nonempty kernel. Gate A has no cross-kernel composition operation.

Materialization at a rational input always returns `ExactJointLaw RealBorel RealBorel`. The input, reward, and successor phantom parameters are coordinate labels rather than measurable-space parameters; they cannot manufacture an `ExactJointLaw` for an arbitrary type. An owner with two nonzero coefficients supplies the same uniform variable to reward and successor. Distinct owners remain independent. Support extrema over a compact input interval select one endpoint for each signed coefficient and use checked rational products and sums.

Construction, same-scope renaming, scope transition, materialization, and support return no semantic result after a limit failure. Renaming preflights mapping traversal, source and target duplicate scans, retained-owner membership, complete-map coverage where required, replacement lookup, post-rename collision checks, and canonical sorting before those scans or derived owner lists. Reports contain deterministic raw-row, owner, output, coefficient, preflight-work, arithmetic-work, and maximum-rational-bit counts. The public module states the total failure precedence for each operation.

Measurability is an argued syntax-directed obligation. Each admitted input coordinate is a rational affine map, hence continuous and Borel measurable. Each source is a compact Borel uniform law. Finite products of the represented sources are standard Borel, and both outputs are finite affine projections. This argument covers every syntax constructor in Gate A. It does not establish measurability for an arbitrary Haskell function because no callback constructor exists.

D-079 remains `Proposed`. Gate A adds no control package, regular conditional probability or disintegration, point conditioning, polynomial value function, multi-step control, or numerical certificate.

## Finite-observation conditioning

An observation likelihood is finite and affine on a checked compact interval. The constructor checks completeness, nonnegativity, normalization, and prior support. Positive evidence gives this expectation:

\\[
E[g(X)\mid O=o]
=\frac{E[g(X)\ell_o(X)]}{E[\ell_o(X)]}.
\\]

Let `X` be uniform on `[0,1]`, with `P(H|X=x)=x`. Tests get evidence `1/2`. The posterior means are `2/3` after `H` and `1/3` after `not H`. A zero-evidence row has no posterior. A finite disintegration exposes only positive-evidence rows.

The package has no generic point conditioner or arbitrary density constructor. It has no continuous-to-continuous disintegrator or general Radon--Nikodym operator. Standard-Borel existence does not supply a universal algorithm or a globally unique version. Finite disintegration uses one cumulative work account across all observation rows, including zero-evidence rows.

## Numerical execution

`markovian-continuous-numerical` converts rational values to finite `Double` values. Each conversion records the exact rounding difference. The adaptive GK15/7 integrator has evaluation, subdivision, and depth limits. Tie selection is deterministic. A callback failure, nonfinite value, stall, or exhausted budget returns no estimate. `EstimatedToleranceMet` uses this test:

\\[
\hat e\leq\max(\epsilon_{abs},\epsilon_{rel}|\hat I|).
\\]

The embedded error is an estimate. It is not a certified bound. Successful quadrature requires finite aggregate value, aggregate estimated error, and tolerance threshold. Floating intervals are admitted only when both endpoints and their strictly positive width are finite; midpoint and uniform interpolation use the admitted finite width.

Sampling receives and returns an owned SplitMix64 state. Named uniform, normal, and exponential laws validate their parameters. The test suite pins raw output and successor-state vectors, then checks uniform bit conversion, Box--Muller normal sampling, and inverse-CDF exponential sampling against independent formulas. Monte Carlo uses Welford accumulation. A run can resume from an opaque state. Variance and standard error are estimates, not deterministic bounds.

Tests compare exact moments with GK15. They also compare GK15 with an independent composite Simpson implementation. A bounded tensor-product Simpson fixture checks one bivariate exact expectation. Central finite differences check all four affine coordinates in that fixture. These test oracles are not a multidimensional cubature API.

## Nonclaims

These packages do not claim arbitrary measurability, a Giry monad, universal kernel composition, or arbitrary disintegration. They do not claim certified floating bounds, multidimensional cubature, differentiation through sampling, or cross-platform bitwise reproduction. The accounting covers only the bounded affine-uniform bivariate polynomial algorithm. The numerical checks remain fixture evidence. D-070 and D-071 are `Accepted` only for the restricted exact and numerical scopes after all package, archive, compiler, and hosted gates passed. The packages do not provide continuous MDP solving, tensor semantics, or feedback.
