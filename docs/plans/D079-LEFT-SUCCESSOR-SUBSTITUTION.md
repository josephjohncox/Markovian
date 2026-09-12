# D-079 left-successor substitution contract

D-079 is Accepted within its unreleased scope. The
[decision record](../DECISIONS.md#d-079-add-exact-joint-affine-continuous-kernels)
records implementation and acceptance evidence.

## 1. Scope and source

The implementation and public declarations are in
[`Markovian.Continuous.Kernel.JointAffine.Exact`](../../packages/markovian-continuous/src/Markovian/Continuous/Kernel/JointAffine/Exact.hs).
The [original design record](https://github.com/josephjohncox/Markovian/blob/871f0eedf8a8f460b2b3a5906bec57a365d53e5d/docs/plans/D079-LEFT-SUCCESSOR-SUBSTITUTION.md)
retains baseline source references and contract-review history.

The selected operation is **left-successor substitution**: substitute the left successor into both right coordinates, returning the **right reward and right successor**. It neither retains nor accumulates the left reward. It is not temporal control, a policy operation, or a discounted-return construction.

The source family remains independent compact rational **Uniform** rows with explicitly witnessed sharing. No arbitrary compact laws, arbitrary callbacks, conditioning, point conditioning, RCP, general disintegration, continuous MDP execution, multi-step control, or discount parameter is introduced. D-070 is not widened to arbitrary measurable functions.

## 2. Exact semantics and the two modes

Write the operands as

\\[
\begin{aligned}
R_L(x,U)&=\alpha x+\beta+\sum_i p_iU_i,\\
S_L(x,U)&=ax+b+\sum_i q_iU_i,\\
R_R(y,V)&=cy+d+\sum_j r_jV_j,\\
S_R(y,V)&=ey+f+\sum_j s_jV_j.
\end{aligned}
\\]

The result is exactly

\\[
\begin{aligned}
R'(x)&=ca x+(cb+d)+\sum_i cq_iU_i+\sum_j r_jV_j,\\
S'(x)&=ea x+(eb+f)+\sum_i eq_iU_i+\sum_j s_jV_j.
\end{aligned}
\\]

The witness determines whether each right source is an explicitly identified left source or a fresh independent source. Left reward coefficients do not enter these formulas. The entire left kernel, including unused reward coefficients and zero declarations, is nevertheless admitted and accounted for. Semantically discarded values cannot erase ownership or rational history.

Offer exactly two modes, not a mixed routing language:

1. **All-shared-right:** a complete injective mapping of every right declaration into the full left declaration manifest. Each mapped pair has exactly equal intervals. It need not cover all left declarations; distinct left rows remain independent sources.
2. **All-fresh-right:** a complete injective mapping of every right declaration to proposed new numeric names, disjoint from **both full input declaration manifests**, including the right source's own original name.

The left manifest is the unchanged anchor namespace. Completeness concerns every right declaration, not a fictitious left identity map. Mixed shared/fresh right routing is outside this gate.

## 3. Public API and ownership

### 3.1 Requests, limits, result, and projections

All newly introduced constructors are hidden except the error and small enumeration constructors explicitly listed below. Existing referenced types retain their definitions. Public requests are untrusted input, not validated evidence.

The public module defines opaque `ExactSuccessorOwnerRequest`,
`ExactSuccessorSubstitutionLimits`, and `ExactSuccessorSubstitution` types.
Requests use `sharedSuccessorOwners` or `freshSuccessorOwners`;
`substituteLeftSuccessor` admits them against both actual operands. Request
owner indices and result coordinate indices are nominal.

`exactSuccessorSubstitutionLimits` takes six inclusive limits, in order: raw
entries, owner-reservation slots, output width, input-plus-result coefficient
slots, semantic work, and rational bits. Results expose reporting,
`materializeSuccessorSubstitution`, and `successorSubstitutionSupportExtrema`.
The public source linked above defines their complete signatures.

A fresh target `Natural` is an untrusted candidate name, not a local owner token or freshness proof. A separate substitution limit type avoids silently changing Gate A limit interpretation. Both projection operations are subsequent, separately requested operations, not hidden substitution subcalls. Substitution has exactly one ledger. Projection work is separate; historical bit admission is not reset (§7).

### 3.2 Sealed, non-chainable result

Do not export an underlying `ExactJointAffineKernel`, owner-number/token accessor, owner-scope-changing eliminator, unrestricted underlying-kernel continuation, result/witness constructor, or result/witness record update.

A rank-2 callback exposing the existing kernel would be insufficient: `noiseOwner` can propose any numeric name, and complete re-scoping can transfer a kernel into a caller-chosen scope. A phantom alone does not repair this authority boundary.

The sealed result permits only materialization, extrema, and reporting without returning local owner tokens. Materialization remains the existing scope-erased `ExactJointLaw RealBorel RealBorel`, which substitution does not accept as owner evidence. Coordinate phantoms remain nominal labels, not measurable-space witnesses.

**No chaining:** sealed substitution results are not accepted as further substitution operands. This gate exposes no unrestricted closed composition algebra. Chaining requires a separately frozen owner-access protocol, not a convenience accessor. Retain the private reservation manifest even though chaining is excluded; a future extension must not reconstruct ownership from filtered output rows.

### 3.3 Actual-operand-bound private witnesses

```text
-- Private declarations; constructors and eliminators unexported.
data VerifiedSharedSuccessorOwners leftOwner rightOwner
data VerifiedFreshSuccessorOwners leftOwner rightOwner
```

Each witness is created and consumed within one substitution operation and retains, directly or via immutable references:

- the exact two admitted operand manifests;
- the complete resolved right-source mapping;
- checked injectivity and selected mode;
- shared interval-equality evidence or fresh both-manifest disjointness evidence;
- its already admitted ledger plan.

No witness is returned separately, reused with another pair, or authorized by equal phantom types, counts, shape equality, or a numeric-ID cache. Execution must consume the captured actual operands/manifests. Reusing a public request is allowed only as untrusted input revalidated against the new operands; that is not witness reuse.

## 4. Declaration retention and renaming

### 4.1 Full Gate A declaration retention

The current retained table plus counts cannot recover discarded declaration identities. Future implementation must privately retain:

1. Both affine coordinates.
2. A complete canonical declaration table `(ownerNumber, sourceInterval, rewardCoefficient, successorCoefficient)`, including two-zero rows.
3. A retained-row view or index for materialization/extrema, possibly sharing row objects; this is not another logical set of coefficients.
4. Existing declaration counts and maximum admitted rational bits.
5. Existing public report.

Canonical declaration order is ascending owner number. Construction still admits the full raw spine, rejects duplicates before filtering, validates coordinates and rows in caller input order, and only then forms canonical tables and the retained view. Zero declarations remain private and charged.

### 4.2 Declaration-complete renaming

Gate B renaming uses the full declaration manifest, including zero rows. The historical Gate A implementation inspected only retained rows. Public signatures and nominal roles remain unchanged.

| Surface | Required behavior |
| --- | --- |
| Partial alpha-renaming membership | Full declaration manifest; mapping a declared zero row becomes valid. |
| Partial rename collisions | A target colliding with an unmapped zero declaration fails with `JointAffineNonInjectiveRenaming`. |
| Complete re-scoping | Complete mapping of all declarations, including zero declarations. |
| Empty re-scoping | Succeeds only with no declarations, not merely no retained owners. |
| Rename accounting | Membership, coverage, lookup, final collisions, and canonicalization use declared count, not retained count. |

Keep semantic-error order: duplicate source, duplicate target, unknown source, optional completeness, final collision.

Let `n` be declared count, `t` retained count, `m` mapping count, and `B(n)=n+n^2`. Revised reservations are

\\[
W_\alpha=B(n)+m+2m^2+2mn+2n^2,
\\]

\\[
W_{\mathrm{scope}}=B(n)+m+2m^2+3mn+2n^2.
\\]

The baseline uses `t` in corresponding scan terms. No-zero three-owner fixtures remain work `69` for alpha-renaming and `78` for complete re-scoping. Construction, materialization, and extrema retain their current public counts and formulas. Full-table canonicalization may use bounded insertion ordering: duplicate scanning plus canonical ordering takes at most `n(n-1)` comparisons within the existing `n^2` reservation. Do not add an uncharged second full-table sort.

### 4.3 Sealed result manifests

Retain separately the complete result source table (including zero result rows), nonzero executable view, reservation-name set, and immutable substitution report. The cached projection-admission maximum is the report's complete `substitutionMaximumRationalBits`, not a recomputed final-representation maximum.

The reservation-name set is the union of all left declaration names, all original right declaration names, and all fresh targets. Original right names remain reserved after freshening or sharing. Reservation-only aliases are not additional independent sources and acquire no fake interval laws.

Two operands may have the same numeric name with different intervals: fresh mode can separate their sources while retaining the old numeric spelling once in the reservation set. Projection reports concern the complete result source table. Reservation-only aliases add **no projection rows or scans**; any future owner-transfer operation must explicitly admit that sealed metadata.

## 5. Errors, canonical choice, preflight, and atomicity

### 5.1 Diagnostics

`ExactSuccessorSubstitutionError` and `SuccessorSubstitutionParticipant` have
public constructors with `Eq` and `Show` instances, defined in the public module.

Indices are one-based. Duplicate errors identify mapping positions. Unknown-source, unknown-target, and interval-mismatch errors identify mapping position. Incomplete mapping identifies canonical right declaration position. Fresh collision identifies mapping position, participant, and canonical declaration position. Interval mismatch carries the left interval then right interval. Errors expose no generated owner token.

### 5.2 Total substitution failure precedence

1. Validate configured limits in raw, owner, output, coefficient, work, rational-bit order. In each dimension, zero is invalid; a value above `maxBound :: Int` gives the existing machine-index error.
2. Read trusted cached declaration counts and admit combined input raw count.
3. Bound the mapping spine against the **remaining combined raw allowance**. Count only; do not copy, sort, inspect entries, or filter.
4. Admit owner-reservation slots, output width, coefficient slots, and the entire combined work plan, in that order. Check a computed count against machine capacity before its configured limit.
5. Duplicate mapping sources.
6. Duplicate numeric mapping targets.
7. Unknown right sources.
8. Incomplete right coverage.
9. Shared mode: unknown left targets. Fresh mode: target collisions with both complete manifests, using the canonical selection below.
10. Input rational admission: left cached maximum; left reward scale/offset, successor scale/offset; left declarations in canonical order (interval validity, lower endpoint, upper endpoint, reward coefficient, successor coefficient); then the corresponding right sequence.
11. Shared mode: exact interval equality in mapping order, lower endpoint before upper endpoint.
12. Create the private actual-operand-bound witness.
13. Execute the fixed arithmetic schedule (§6.3).
14. Check ledger invariants.
15. Filter zero executable rows; retain complete declaration and reservation manifests; construct sealed result/report; return `Right`.

Duplicate pairs are chosen lexicographically by mapping positions. Unknown sources/targets follow mapping order; missing coverage follows canonical right order. Fresh collision choice is **mapping position first, then left-before-right, then canonical declaration position**. It is not a global left scan that can report a later mapping before an earlier right collision. No semantic check substitutes retained rows for declaration manifests.

“Preflight before allocation” means before size-dependent derived mappings, candidate arrays/lists, canonical tables, reservation sets, or filtered views. Reading an existing spine with strict bounded scalar counters is admission, not an allocated working table. In particular, do not reuse the baseline allocating `boundedRenaming` unchanged.

All semantic scans occur after full work admission. All arithmetic precedes filtering and publication. Any failure returns only `Left`: no partial witness, result, law, extrema pair, or report escapes.

## 6. One combined substitution ledger

### 6.1 Dimensions

Let `n_L,n_R` be full input declaration counts, `N=n_L+n_R`, `m` admitted mapping entries, `H=N+m`, and `B(n)=n+n^2`. Success requires `m=n_R`. Preflight uses actual admitted `m` even for a subsequently rejected request.

| Dimension | Charged quantity |
| --- | ---: |
| Raw entries | `H=N+m` |
| Owner-reservation slots | `H`, a pre-union upper bound |
| Output width | `2` |
| Coefficient slots | `C=12+4N` |
| Work | `W=P+A` |
| Rational bits | Maximum admitted or generated rational size |

Coefficient reservation is `(8+2N)` input slots plus `(4+2N)` possible result slots. It is a logical reservation, not a heap-size claim. Temporary arithmetic is charged through work and rational bits. Output width is codomain width, not summed input/output widths. Sharing and cancellation do not discount reservations.

### 6.2 Structural reservation

Both modes reserve all the following semantic slots:

| Phase | Slots |
| --- | ---: |
| Stored admission baseline | `B(n_L)+B(n_R)` |
| Input rational observations and interval-order checks | `8+5N` |
| Mapping-spine admission | `m` |
| Source and target pair scans | `2m^2` |
| Right membership and complete coverage | `2mn_R` |
| Shared target membership allowance | `mn_L` |
| Fresh collision allowance against both manifests | `mN` |
| Shared endpoint equality allowance | `2m` |
| Candidate mapping lookups | `Nm` |
| Translated-candidate consistency scans | `N^2` |
| Canonical candidate ordering | `N^2` |
| Canonical row finalization and zero classification | `N` |
| Reservation-name collection | `H` |
| Reservation-name duplicate and ordering scans | `2H^2` |

\\[
\begin{aligned}
P={}&B(n_L)+B(n_R)+8+5N\\
 &+m+2m^2+2mn_R+mn_L+mN+2m\\
 &+Nm+2N^2+N+H+2H^2.
\end{aligned}
\\]

Mode-inapplicable slots remain reserved. Reports describe **reserved semantic slots**, not measured CPU comparisons. This gives deterministic exact boundaries without data-dependent discounts. Do not call public renaming/construction helpers with separate full budgets and sum reports afterward.

### 6.3 Fixed arithmetic and rational schedule

Always execute, without zero shortcuts:

1. Right reward: `c*a`; `c*b`; `(c*b)+d`.
2. Right successor: `e*a`; `e*b`; `(e*b)+f`.
3. Every full left declaration in ascending original owner order: `c*q_i`, then `e*q_i`.
4. Canonical target-owner accumulation: order candidates by target number; in a shared bucket, left before right; initialize both coefficients to zero; perform one reward addition then one successor addition for **every candidate**, including zeros.

Right row coefficients are admitted and copied, not multiplied by an invented unit factor. Thus `M=4+2n_L`, `D=2+2N`, `A=M+D=6+2n_L+2N`. Multiplication-by-zero and addition-to-zero shortcuts must not change counts or failure order.

Each rational operation checks both operands and its result. Every generated product and partial sum is checked before cancellation/discard. The maximum starts with both admitted input maxima. Rational size is the maximum of the bit counts of the absolute reduced numerator and positive denominator, with zero having at least one bit, as in Gate A.

This bounds represented `Rational` intermediates, not private integer temporaries inside normalization. Owner IDs are not rational-bit-budgeted. Semantic scan counts are not wall-clock or byte-allocation guarantees.

## 7. Projection contract — conservative historical admission

This section is the required independent-review correction. **Both `materializeSuccessorSubstitution` and `successorSubstitutionSupportExtrema` use `substitutionMaximumRationalBits` for stored rational admission and as their initial meter maximum.** It includes discarded input values (particularly left reward) and every generated intermediate, including canceled products. Separate work budgets do not authorize dropping that history or rescanning only the final representation.

For complete result declaration count `d` and executable retained count `t`:

| Projection report/admission quantity | Value |
| --- | ---: |
| Raw entries | `d` |
| Declared owners | `d` |
| Retained owners | `t` |
| Outputs | `2` |
| Coefficients | `4+2d` |
| Preflight work | `B(d)=d+d^2` |
| Materialization arithmetic | `4+8t` |
| Materialization total work | `B(d)+4+8t` |
| Extrema arithmetic | `8(t+1)` |
| Extrema total work | `B(d)+8(t+1)` |

The reports are existing `ExactJointAffineReport` values, with operation `JointAffineMaterialization` or `JointAffineSupportExtrema`. Their maximum is at least the substitution maximum and includes new projection input/endpoint observations and generated arithmetic. The immutable substitution report itself is not rewritten. Reservation-only aliases add no projection rows, scans, or coefficient slots; historical maximum remains a scalar admission fact, not a new full-manifest scan.

### Ordered admission and arithmetic

1. Validate Gate A configured limits in raw, owner, output, coefficient, work, rational-bit order, with its zero/machine rules.
2. Stored-result admission checks raw `d`, owners `d`, outputs `2`, coefficients `4+2d`, baseline work `B(d)`, then the historical rational maximum, in that order.
3. Admit complete operation work using Gate A `operationPreflight` accounting/machine checks.
4. Materialization: admit rational input; evaluate reward input then successor input (multiply then add per coordinate); reward support then successor support. For each retained row in canonical order and each coordinate: signed lower/upper endpoint products, then lower/upper running-sum additions, including zero coordinate coefficients.
5. Extrema: interval-order check, then lower/upper input endpoint rational admission; reward extrema then successor extrema. For each coordinate: signed input endpoint products, lower/upper offset additions; then per retained row signed endpoint products and lower/upper accumulation. Degenerate input intervals are rejected before endpoint bit inspection, as in Gate A.
6. Check arithmetic and total-work invariants before constructing and returning the complete law or interval pair and report. Errors remain `ExactJointAffineError`, not substitution-wrapped errors; no partial output/report escapes.

Stored baseline work failure precedes historical bit failure; historical bit failure precedes insufficient **operation-specific total** work. After stored admission, full operation work precedes projection input bits or interval validity. These are deliberately distinct precedence boundaries, not the substitution operation's work-before-input-history rule.

## 8. Report meanings

The substitution report is opaque, with non-record accessors; none performs arithmetic or exposes owner identity.

`ExactSuccessorSubstitutionMode` exposes `SharedRightOwners` and
`FreshRightOwners`. `ExactSuccessorSubstitutionReport` has a hidden constructor.

| Accessor | Value |
| --- | --- |
| `substitutionMode` | Selected mode |
| `substitutionLeftDeclaredOwners` | `n_L` |
| `substitutionRightDeclaredOwners` | `n_R` |
| `substitutionMappingEntries` | `m` |
| `substitutionRawEntries` | `H` |
| `substitutionOwnerReservationSlots` | `H` |
| `substitutionReservedNames` | Actual reservation-set cardinality |
| `substitutionSharedOwners` | `m` shared; otherwise `0` |
| `substitutionFreshOwners` | `m` fresh; otherwise `0` |
| `substitutionDeclaredResultOwners` | `n_L` shared; `N` fresh |
| `substitutionRetainedResultOwners` | Result rows with either coefficient nonzero |
| `substitutionZeroFilteredResultOwners` | Declared minus retained result rows |
| `substitutionOutputs` | `2` |
| `substitutionCoefficientSlots` | `12+4N` |
| `substitutionPreflightWork` | `P` |
| `substitutionCoefficientMultiplications` | `4+2n_L` |
| `substitutionCoefficientAdditions` | `2+2N` |
| `substitutionArithmeticWork` | `A` |
| `substitutionWork` | `P+A` |
| `substitutionMaximumRationalBits` | Complete maximum, including discarded inputs/intermediates; both projections' stored-admission/meter seed |

## 9. Required independent oracles and boundary fixtures

These are test requirements and independent expected values. Execution evidence is recorded in the decision record.

### 9.1 Sharing versus freshness and lost left reward

Left reward is `2`, left successor is `U` (owner `1`, Uniform `[0,1]`). Right reward is `y`, right successor is `V` (owner `2`, Uniform `[0,1]`). Shared request maps right `2` to left `1`; fresh request maps right `2` to candidate `3`.

| Mode | Result | Reward mean | Successor mean | Joint moment |
| --- | --- | ---: | ---: | ---: |
| Shared | `(U,U)` | `1/2` | `1/2` | `1/3` |
| Fresh | `(U,V)` | `1/2` | `1/2` | `1/4` |

Reward mean is `1/2`, **not `5/2`**. For both, `n_L=n_R=m=1`, `N=2`, `H=3`, `P=65`, `M=6`, `D=6`, `A=12`, `W=77`. Maximum rational bits is `2` from the admitted left reward. Exact substitution limits are the declaration-level fixture `exactSuccessorSubstitutionLimits 3 3 2 20 77 2`.

Shared report: one declared/retained result owner, two reserved names. Fresh report: two declared/retained result owners, three reserved names. Projection dimensions depend on result declarations, not these reservation-only aliases.

### 9.2 Exact and one-below dimensions

For §9.1, each following error is wrapped in `SuccessorSubstitutionAdmission`:

| Reduced limit | Expected admission error |
| --- | --- |
| Raw `2` | `JointAffineLimitExceeded JointAffineRawEntries 2 3` |
| Owners `2` | `JointAffineLimitExceeded JointAffineOwners 2 3` |
| Outputs `1` | `JointAffineLimitExceeded JointAffineOutputs 1 2` |
| Coefficients `19` | `JointAffineLimitExceeded JointAffineCoefficients 19 20` |
| Work `76` | `JointAffineLimitExceeded JointAffineWork 76 77` |
| Bits `1` | `JointAffineLimitExceeded JointAffineRationalBits 1 2` |

Also require zero limits in each of six positions; configured values above machine capacity; computed machine-count overflow before configured-limit comparison; bounded admission of an infinite mapping spine; ownerless inputs with empty complete mappings; declaration-only all-zero inputs; work failure before duplicate, collision, interval mismatch, or arithmetic failure; independently evaluated larger bounded count formulas.

For ownerless `n_L=n_R=m=0`, `P=8`, `M=4`, `D=2`, `A=6`, `W=14`. Positive configured limits are still required even when actual raw/owner counts are zero.

### 9.3 Intermediate cancellation and projection-history distinction

One shared Uniform `[0,1]` owner has left successor coefficient `3/2`, right reward input scale `3/2`, right reward owner coefficient `-2`, and all other coordinates/coefficients zero. The computation generates `9/4` and then retains `1/4`:

\\[
(3/2)(3/2)=9/4,\qquad 9/4-2=1/4.
\\]

Input maximum is two bits; complete final representation needs three; the discarded product needs **four**. Substitution with bit limit four succeeds and reports four. Bit limit three fails on `9/4`, even though the result would fit. Insufficient substitution work wins before this arithmetic failure.

For the successful four-bit result, `d=t=1`, raw/owners `1`, outputs `2`, coefficients `6`, baseline work `2`, materialization arithmetic `12`/total `14`, extrema arithmetic `16`/total `18`. Materialize at zero and take extrema over `[0,1]`:

| Projection limits (other dimensions sufficient) | Required result |
| --- | --- |
| Materialization work `14`, bits `3` | Stored admission fails: `JointAffineLimitExceeded JointAffineRationalBits 3 4`. |
| Extrema work `18`, bits `3` | Same stored historical-bit failure. |
| Materialization work `14`, bits `4` | Succeeds; report maximum remains `4`. |
| Extrema work `18`, bits `4` | Succeeds; report maximum remains `4`. |
| Materialization work `13`, bits `3` | Historical-bit error wins over insufficient total projection work. |
| Extrema work `17`, bits `3` | Historical-bit error wins over insufficient total projection work. |
| Materialization work `13`, bits `4` | `JointAffineLimitExceeded JointAffineWork 13 14`. |
| Extrema work `17`, bits `4` | `JointAffineLimitExceeded JointAffineWork 17 18`. |
| Either projection work `1`, bits `3` | Stored baseline work wins: `JointAffineLimitExceeded JointAffineWork 1 2`. |

Add both-projection exact/one-below dimensions, invalid limits, projection input-bit and invalid-interval precedence, and larger bounded count checks. These tests distinguish separate projection work from a forbidden historical-bit reset.

Also use ownerless inputs with a large discarded left reward and identically zero result: both projection admissions and successful report maxima must retain that discarded reward's bit requirement. Changing left reward can change admission/history without changing result semantics.

For complete cancellation, use left coefficient `3/2`, right input scale `2`, right owner coefficient `-3`. The result row becomes zero, but full declarations and original reservation names remain. A private invariant probe must inspect preservation; public retained-row count alone is insufficient. Projections charge declared dimensions and execute only retained rows.

### 9.4 Collisions, completeness, and operand-bound evidence

Every relevant fixture also has a zero-coefficient variant:

1. Missing right declaration: incomplete mapping.
2. Duplicate right source: duplicate source wins even if targets duplicate.
3. Distinct right sources with equal targets: noninjective target.
4. Unknown right source whose target also collides: unknown source first.
5. Fresh target equals its own original right name: collision.
6. Fresh target equals another right declaration: collision.
7. Fresh target equals a retained left declaration: collision.
8. Fresh target equals a zero-filtered left declaration: collision.
9. Fresh target equals a zero-filtered right declaration: collision.
10. Distinct nominal scopes with equal numeric names: no automatic freshness or sharing.
11. Shared target is a zero left declaration: permitted with complete explicit mapping and equal interval.
12. A renamed zero declaration remains in the manifest used by later substitution checks.

For the §9.1 inputs with empty mapping, `P=42`, `A=12`, `W=54`; sufficient limits yield incomplete mapping, not successful ownerless substitution. With two mapping entries and `n_L=n_R=1`, `P=96`, `A=12`, `W=108`; work `107` wins over duplicate-source detection. Lock lexicographic duplicate selection and mapping-first/left-before-right/canonical-position collision selection with combined-invalid fixtures.

Use equal phantom types and equal counts but different owner manifests or intervals to show public requests are revalidated and wrong-witness application is structurally unavailable. Private probes must check actual captured operand use, complete zero-result declarations, and reservation-only names; comparing dimensions or exposed counts is not sufficient evidence.

Gate A compatibility fixtures must cover valid renaming of declared zeros, collisions with unmapped zeros, omitted-zero complete maps, empty re-scoping of declaration-only kernels, revised exact/one-below declared-count work, unchanged no-zero `69`/`78` fixtures, constructor input-order validation, and the preserved renamer error order.

### 9.5 Exact source-law equality

Shared left Uniform `[0,1]` and right Uniform `[0,2]` fail with `SuccessorSubstitutionSharedIntervalMismatch`, even when both mapped rows have zero coefficients. Rationally equal endpoints such as `1 % 2` and `2 % 4` match. Equality is exact interval equality, not textual equality, equal means, overlapping support, or an inferred pushforward.

The same different input intervals may succeed in fresh mode with an otherwise valid fresh map, including operands with equal original numeric names. Reservation-only names must not be assigned invented laws.

### 9.6 Signed symbolic and corner oracle

Let independent `U,V` be Uniform `[-1,1]`, with

\\[
S_L=-2x+1+3U,\qquad R_R=-y+2+4V,\qquad S_R=2y-3-V.
\\]

Shared `V=U` gives `R'=2x+1+U`, `S'=-4x-1+5U`. Fresh gives `R'=2x+1-3U+4V`, `S'=-4x-1+6U-V`.

| Mode | Reward extrema over `x` in `[-2,3]` | Successor extrema |
| --- | --- | --- |
| Shared | `[-4,8]` | `[-18,12]` |
| Fresh | `[-10,14]` | `[-20,14]` |

| Mode at `x=0` | Reward mean | Successor mean | Covariance | Joint moment |
| --- | ---: | ---: | ---: | ---: |
| Shared | `1` | `-1` | `5/3` | `2/3` |
| Fresh | `1` | `-1` | `-22/3` | `-25/3` |

Enumerate one endpoint choice per **distinct witnessed source**. Shared appearances cannot choose endpoints independently. These are coordinate extrema, not a claim that joint support is their Cartesian product.

### 9.7 Independent multinomial oracle

Extend the existing test-side multinomial oracle, not production canonicalization:

1. Express both input forms directly as test data.
2. Apply the displayed substitution formulas.
3. Identify sources using the explicit shared/fresh relation.
4. Independently expand bivariate test polynomials.
5. Integrate each source power using the exact Uniform formula below.
6. Compare with materialization followed by `expectBivariatePolynomial`.

\\[
E[U^k]=\frac{b^{k+1}-a^{k+1}}{(k+1)(b-a)}.
\\]

Include `XY`, both second moments, and a mixed higher-degree polynomial. Marginal means alone cannot detect lost sharing. Oracles must not reuse production substitution, sorting, or canonicalization as their semantic reference.

### 9.8 Semantic laws and explicit non-laws

Require input-row/mapping permutations to preserve successful moments, extrema, and reports; coherent alpha-renaming to preserve exact semantics; and deterministic identity-left-successor substitution to preserve right forms modulo authorized owner renaming. With an ownerless identity left operand and nonempty right declarations, identity requires fresh mode: shared mode has no targets.

Changing the left reward does not change denotation, though admission and maximum bits may change. Substituting into right kernel `(0,y)` gives `(0,S_L)`, not the original left joint pair. Do not claim reward-preserving generic composition identity, associativity of bounded success/failure, or a category instance. Sealed results are not operands; algebraic substitution identities do not imply equal intermediate resource peaks.

## 10. Escape, positive-client, and compatibility evidence

Retain all Gate A boundary fixtures. Add separately named compile failures for:

- constructing `ExactSuccessorSubstitution`;
- constructing either private validated witness;
- coercing any result coordinate label;
- coercing either request owner parameter;
- supplying right-source mappings from the wrong nominal scope;
- mismatched intermediate coordinate labels between operands;
- materializing into phantom reward/successor spaces instead of `RealBorel`;
- extracting an underlying Gate A kernel;
- extracting a local `NoiseOwner`;
- applying `reScopeJointAffineKernel` to a sealed result;
- record-updating the opaque substitution report.

Add a successful client using both modes and both projections so missing APIs cannot masquerade as working boundaries. Extend the existing boundary harness's named-diagnostic checks: compilation failure alone does not show the intended boundary failed.

Freeze these compatibility risks explicitly:

1. Zero-row renaming changes observable behavior; its documentation, errors, exact-work tests, and boundary descriptions must agree.
2. Fresh identity renaming is rejected; injectivity is insufficient without both-source disjointness.
3. Equal numbers do not imply sharing; full explicit mapping and exact interval equality are required.
4. Combined substitution limits are not Gate A limits applied twice; individually admissible inputs may fail combined admission.
5. Discarded left reward remains admitted, and both projections inherit all substitution rational history.
6. Sealing intentionally excludes chaining; any owner-access extension needs another reviewed design.
7. Reports measure conservative semantic reservations, not exact machine cost or heap size.
8. Scope-erased materialization is retained, not widened into reusable owner evidence.
9. No version, package-edge, acceptance, or publication change is implied.
