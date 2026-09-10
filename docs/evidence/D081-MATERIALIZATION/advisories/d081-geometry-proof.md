# D081 section B — post-header pure geometry proof

## Inherited decisions

- This is read-only numerical advice, not approval of coefficients, implementation, refreeze, or acceptance.
- The source basis is `/tmp/d081-retention-v2-B0ZMyODc/source-basis/current-Internal.hs` (**I** below), attributed by the retained review to immutable585; `current-Shape.hs` is **S**. I read the controlling completion direction, both authorities, the complete v2 review, the relevant frozen §6/§8 definitions, and the source. No commands, mutations, delegation, or experiments were performed.
- The inspected `candidate.patch` changes only `finiteTensor` at I:1689–1698. It does not alter this geometry section.
- Units remain frozen logical primitives and scalar/reference/control slots. These are not GHC allocation, heap, instruction, or timing counts.
- Prefix work and materialization arrive separately. Newly materialized prefix objects remain accountable through caller-retained roots.
- This section starts **after successful full reservation**, immediately before the signed/domain checks. Formula evaluation, reservation, prefix scans, and global entry administration belong to section A. Runtime transactions and materialization belong to section C.

## Diagnosis

**The geometry is rank-linear except for permutation checks/lookups and the explicitly repeated address validation. There is no base-rank traversal after admission, no address cache, and no parent-map history retained by a derived map.**

The numerical schedules below expand the source rather than reuse frozen coefficients. They are deliberately conservative logical-machine upper bounds. Their larger call/frame accounting does **not** establish that smaller frozen coefficients are false.

Confidence: **high** in the source paths and iteration bounds; **moderate** in the complete proposed numerical envelope pending independent checking of this elementary schedule.

### 1. Parameters and interface

Write:

- \(b=r_B,\ s=r_S,\ r=r_V\);
- \(V\) for the admitted target count;
- \(P=V(V-1)/2\), with \(P=0\) for \(V=0\);
- \(D=s(s-1)/2\).

Successful transform headers establish \(s=r\), before the post-header body or raw-list semantics: I:757–766. I retain both symbols to expose which source operand each traversal visits.

All counts below include termination and conservative early-failure control. One post-body diagnostic allowance is included once per operation, not once per helper.

The incoming interface must include:

1. admitted, traversed base/source/target witnesses;
2. admitted raw-list spines of the required lengths;
3. valid publicly produced parent-map metadata for transforms;
4. the newly computed `AffineCharge` and successful `AffineUsage`.

The last two records contain nine logical slots under the conservative tag-plus-fields schedule used below:

\[
(1+4)+(1+3)=9.
\]

Their **construction belongs to section A**. Their continued retention belongs in the incoming survivor account. If section A initially classifies these nine slots as workspace, transfer them to survivors at this boundary; do not construct or charge them twice.

### 2. Elementary expansion used here

The following are expansions into frozen primitives, not new indivisible operations.

| Notation | Expanded work | Newly constructed slots |
|---|---:|---:|
| Inspect a nonempty list | tag inspection + branch + head field + tail field = **4** | none required |
| Inspect a terminating/mismatching list | tag inspection + branch = **2** | none required |
| Construct and force a numeric cons | two slot assignments + two forcing/control inspections = **4** | **2** |
| Construct a \(k\)-field tuple | \(k\) assignments + \(k\) field inspections + one control/tag assignment = **\(2k+1\)** | conservatively **\(k+1\)** |
| Construct `Right` | tag assignment + payload-reference assignment + outer-result inspection = **3** | **2** |
| Consume an `Either` bind | tag inspection + payload-field inspection + branch = **3** | no aggregate allocation assumed |
| `min` or `max` | comparison + branch + selected-result assignment = **3** | at most one result slot |
| Bounded conversion | one bounded scalar operation; separate demanded-result inspections are listed where used | bounded result |
| Dynamic closure with \(k\) references | \(k+1\) assignments + \(k\) reference inspections = **\(2k+1\)** | **\(k+1\)** |

The tuple/control overcount is intentional. It does not redefine the frozen two-slot list unit.

#### Calls and recursive activations

For an activation with \(a\) explicit logical arguments, use

\[
T(a)=
\underbrace{a+a+2+2}_{\text{argument reads/assignments, entry/return, result/control}}
+
\underbrace{(a+4)+(a+4)}_{\text{captured/control-slot assignments and inspections}}
=4a+12.
\]

Every summand is an individual read, assignment, inspection, or transition. This is **not** one \(T(a)\)-sized allocation primitive. It conservatively includes a frame with at most

\[
(a+4)+a+1=2a+5
\]

slots. Tail-recursive activations reuse the live frame; assignments still count as work/construction. Strict non-tail list constructors retain bounded continuation frames, counted separately below.

Opaque references are read, not recursively forced. In particular this convention does not demand the caller continuation or its arbitrary result.

#### Bounded diagnostics

I:304–306, 340–364, 437–438 construct the diagnostic and its wrappers. The largest geometry diagnostic has four fields.

One post-body error is bounded by

\[
\begin{aligned}
G_{\rm err}
&=9\text{ slot assignments}
 +9\text{ inspections}
 +3\text{ return controls}\\
&\quad+T(1)\text{ for `affineFailure`}
 +2\text{ for a length-sentinel increment/demand}\\
&=9+9+3+16+2=\boxed{39}.
\end{aligned}
\]

At most nine diagnostic slots survive. Successful-body envelopes reserve these 39 work units even though success does not execute the diagnostic branch.

This is the body’s single diagnostic allocation schedule, **not another global entry allowance**.

The first demand of the signed bound constants is separately overbounded by six primitives: two scalar operations, two result assignments, and two result inspections. `affineMachineMaximum` has already been used by admission. This gives the common body allowance \(G_{\rm const}=6\).

### 3. Scalar checks and strict copies

Source: I:599–661; S:25–28 supplies the singleton constructor/dictionary fields.

Bounds in this table exclude the single \(G_{\rm err}\), added only at operation level.

| Function | Fixed work | Per visited element | Complete bound |
|---|---:|---:|---:|
| `affineDimensions` | \(T(1)+2=18\) | \(16+4+4+1+1+4=30\) | **\(18+30n\)** |
| `affineSignedInput` | \(T(3)+1+1+3\) | — | **29** |
| `affineSignedInputs` | \(T(2)+3+[T(2)+2+3]=48\) | \(20+4+29+3+1+1=58\) | **\(48+58n\)** |
| `affineSignedResult` | \(T(3)+3+2+2+3\) | — | **34** |
| `affineRawEmpty` | \(T(2)+2+[T(1)+2+3]=43\) | \(16+4+1+1=22\) | **\(43+22n\)** |
| `affineIntegerInputs` | \(T(1)+2=18\) | \(16+4+1+1+1+4=27\) | **\(18+27n\)** |
| `affineNormalize` | \(T(3)+4=28\) | \(24+8+1+1+1+1+1+1+4=42\) | **\(28+42n\)** |
| `affineZipper` | \(T(2)+1+[T(3)+4]=49\) | \(24+8+5+1+4=42\) | **\(49+42n\)** |

Expansion details:

- The four dimension-extraction primitives are proxy access, dictionary-reference access, extraction of the already admitted bounded numeric value, and `fromInteger`. This does not charge a whole `KnownShape` producer as one operation. Production and retained witness tails belong to the prefix schedule.
- `affineSignedInput` checks `minBound` before any normalization.
- `affineSignedResult` expands the conversions/negation, two comparisons, short-circuit branches, and result construction. Failed lower-bound checks execute less than the two-comparison path.
- `affineRawEmpty` rejects nonzero offset without inspecting a stride. Otherwise it visits at most the admitted stride length.
- `affineNormalize` inspects both spines. Its per-axis allowance includes the empty branch, singleton comparison, selected-value assignment/demand, recursive-result demand, and cons construction.
- Even when normalization selects zero, matching its stride argument can demand the strict `affineIntegerInputs` producer. That producer is therefore counted separately.
- `affineZipper` constructs a reversed list and pair per axis: conservatively **five slots per axis**, not merely a shared traversal.

Strict copies force their terminating tails before return. No extra uncharged post-result list-forcing traversal is assumed.

### 4. Extrema and contiguity

#### `affineExtrema`

Source: I:663–674.

Fixed work:

\[
T(3)+3+T(5)+4+5+3=24+3+32+4+5+3=\boxed{71}.
\]

Per dimension:

\[
\begin{aligned}
&32 &&\text{loop activation}\\
+&8 &&\text{two-list inspection}\\
+&3 &&\text{dimension conversion, subtraction, multiplication}\\
+&3(34) &&\text{three `affineSignedResult` calls}\\
+&3(3) &&\text{their binds}\\
+&2(3) &&\text{`min` and `max`}\\
+&2 &&\text{low/high additions}\\
+&2 &&\text{axis increment and demand}\\
=&\boxed{164}.
\end{aligned}
\]

Thus:

\[
E(n)=\boxed{71+164n}.
\]

A dimension/stride mismatch stops at its first unmatched constructor; sentinel arithmetic and diagnostic construction are covered by \(G_{\rm err}\).

#### `affineContiguous`

Source: I:676–681.

\[
C(n)=
\underbrace{16+2+24+2}_{44}
+n\underbrace{(24+4+2+2+1+2+3+1+1)}_{40}
=\boxed{44+40n}.
\]

The step includes the pair fields, bounded dimension conversion/product, dimension and stride comparisons, Boolean short-circuit controls, selected Boolean assignment, and demands.

**Empty maps do not run this function:** I:710. For nonempty admitted dimensions every dimension is positive and each suffix product is at most \(V\). Thus the strict `expected` product remains bounded; an irrelevant empty stride product is not silently evaluated.

### 5. Coordinate kernel: both passes expanded

Source: I:685–708.

The input zipper is already built once by validation. A call performs a complete digit pass and a complete forward dot pass.

#### Digit pass

`affineDigits` has three arguments.

For each axis:

\[
\begin{aligned}
45={}&T(3)+4+2\\
&+2\text{ bounded division/remainder operations}\\
&+5\text{ quotient/remainder pair construction}\\
&+2\text{ pair-field inspections}\\
&+2\text{ scalar demands}\\
&+4\text{ digit-cons construction/forcing}.
\end{aligned}
\]

The terminal activation costs \(T(3)+2=26\).

This explicitly expands `quotRem`; it is not one opaque library-allocation coupon.

The pass constructs \(2r\) digit-list slots. Quotient/remainder tuple and arithmetic temporaries are transient. Every dimension is positive on this path, so there is no division by zero.

#### Dot pass

For each axis:

\[
\begin{aligned}
51={}&T(4)+8\\
&+1\text{ conversion}+1\text{ multiplication}+1\text{ term demand}\\
&+2\text{ term comparisons}+2\text{ branches}\\
&+1\text{ addition}+1\text{ next-address demand}\\
&+2\text{ next comparisons}+2\text{ branches}\\
&+1\text{ axis increment}+1\text{ demand}.
\end{aligned}
\]

The successful terminal costs

\[
T(4)+4+2+1+2+1+3=\boxed{41}.
\]

It includes both terminal-spine checks, lower/upper address tests, bound/result conversions, branches, and `Right`.

The wrapper, digit demand, initial dot axis, and captured-bound closure cost

\[
T(5)+1+1+3=37.
\]

Consequently:

\[
K(r)=37+26+41+(45+51)r
=\boxed{104+96r}.
\]

The local `address zipper` adapter in `affineValidate` is charged another \(T(2)=20\) per saturated use:

\[
A(r)=\boxed{124+96r}.
\]

This is a source-level call expansion, not an optimizer-dependent removal of that adapter.

#### Numeric bounds

- Quotients and digits lie in the admitted nonnegative machine domain.
- A digit/stride product can require a widened two-slot temporary before checking.
- A checked product or next address is in \([-M,M]\); an unchecked sum of two admitted signed values is bounded in the corresponding widened domain.
- The terminal conversion to `Natural` follows the nonnegative/bounds checks.
- Only one digit workspace is needed at a time. The first result address of a pair remains as one bounded scalar while the second kernel runs.

### 6. Whole validator, including every loop terminal

Source: I:710–742.

The empty branch costs:

\[
T(5)+2+7+3=\boxed{44}.
\]

It constructs `(0,0,True)` and `Right`; it calls no extrema, zipper, address, pair, or contiguity traversal.

For nonempty validation:

| Component | Bound |
|---|---:|
| Validator activation and count branch | 34 |
| Extrema | \(71+164r\) |
| Extrema-result bind and pair-field reads | 5 |
| Extrema bounds, conversions, branches, unit result/bind | 11 |
| Zipper and demand | \(50+42r\) |
| Dynamic local closure setup | 22 |
| Address-loop terminating activation | 25 |
| One ordinary address-loop iteration | \(26+A(r)\) |
| Pair-outer terminating activation | 25 |
| Each outer pair iteration, including its suffix terminal | 58 |
| Each suffix pair iteration | \(36+2A(r)\) |
| Binds after the address and pair traversals | 6 |
| Contiguity and demand | \(45+40r\) |
| Final triple and `Right` | 10 |

The closure setup is

\[
7_{\rm address}+5_{\rm addresses}+5_{\rm pairs}+5_{\rm suffix}=22.
\]

The outer-pair coefficient is

\[
20+2+4+3+\underbrace{(24+2+3)}_{\text{suffix terminal}}=58.
\]

The suffix pair coefficient is

\[
24+2+6+2+2=36
\]

apart from its two expanded address calls.

Adding the displayed rows:

\[
\boxed{
\operatorname{Validate}(r,V,P)
\le
304+246r
+V(208+96r)
+P(284+192r).
}
\]

This also upperbounds the empty branch.

#### Exact traversal multiplicities

| Traversal | Bound |
|---|---:|
| Extrema | \(r\) nonempty steps and one terminal |
| Zipper | \(r\) steps and one terminal |
| Ordinary address loop | \(V\) kernel calls and one terminal |
| Pair outer loop | \(V\) iterations and one terminal |
| Pair suffix loops | \(P\) iterations and **\(V\) separate terminals** |
| Pair kernels | \(2P\) calls |
| Contiguity | \(r\) steps and one terminal |
| All digit passes combined | \(r(V+2P)\) steps |
| All dot passes combined | at most \(r(V+2P)\) steps |

The implementation recomputes `address first` inside **every pair**. The schedule therefore counts \(V+2P\), not \(V\), address kernels.

The pair order is exactly:

\[
(0,1),(0,2),\ldots,(0,V-1),(1,2),\ldots,(V-2,V-1).
\]

An overlap failure truncates that lexicographic schedule. No sorting, set, address array, or cached first address is present.

### 7. Transform helper expansion

Sources: I:768–825 and I:844–914.

#### Lookup, ranges, and duplicate pairs

| Function | Displayed expansion | Bound |
|---|---|---:|
| `affineAxisRange` | \(T(2)+2\) conversions \(+2\) comparisons \(+2\) branches \(+3\) | **29** |
| `affinePick` | fixed \(T(3)+1+5+[T(2)+2+2]=54\); each visit \(20+4+1+1+1+1+3=31\) | **\(54+31n\)** |
| `affinePermutationRanges` | nil \(20+2+3=25\); each \(20+4+29+3=56\) | **\(25+56s\)** |
| `affinePermutationDuplicates` | setup/nil \(16+1+25=42\); outer \(20+4+4+3+33=64\); inner \(28+4+1+1+1+1=36\) | **\(42+64s+36D\)** |

`affinePick requested` visits at most `requested+1` cons cells on success. Range checks establish `requested < s`; using \(s\) for every lookup is conservative.

The duplicate routine walks suffixes directly. It performs exactly \(D\) equality comparisons when duplicate-free. It does not perform an additional indexing traversal per comparison.

#### Permutation dimension and stride construction

`affinePermutationDimensions`:

- fixed: outer activation 24, initial axis 1, captured-source closure 3, terminal activation/result 31;
- per target axis, excluding lookup:

\[
24+8+1+3+1+1+1+1=40.
\]

Hence

\[
\boxed{59+r(94+31s)}.
\]

`affinePermutationStrides`:

- nil: \(20+2+3=25\);
- per target axis:

\[
20+4+1+(54+31s)+3+1+3+1+4+3
=94+31s.
\]

Hence

\[
\boxed{25+r(94+31s)}.
\]

The two lookup families therefore contribute at most \(2rs\) visited source-list positions, rather than a cubic traversal. The stride result has a new \(2r\)-slot spine even when all selected scalar values are shared.

#### Reverse and direct copy

I:844–857:

\[
\operatorname{CopyStrides}(n)
=(16+2)+n(16+4+1+1+4)
=\boxed{18+26n}.
\]

For a valid selected axis \(j\), `affineReverseStrides` visits \(j+1\) elements itself, then invokes `affineCopyStrides` on the suffix. Its own per-axis ceiling is

\[
20+4+2+2+1+1+1+1+4=36.
\]

Thus

\[
22+36(j+1)+18+26(r-j-1)
\le\boxed{40+36r}.
\]

This is **one total stride-copy traversal**, not a fresh traversal for every axis. The selected negation is safe because parent strides are in \([-M,M]\), excluding `minBound`.

For a nonempty reverse, two lookups and two checked results additionally cost

\[
(54+31r)+(54+31s)+2(34).
\]

For an empty reverse these lookups and arithmetic checks are skipped, but the axis signed/range checks and full stride construction/normalization remain.

#### Slice helpers

| Function | Fixed sum | Per-axis sum | Bound |
|---|---:|---:|---:|
| `affineNonzeroSteps` | \(16+1+20+2+3=42\) | \(20+4+1+1+1+1=28\) | **\(42+28s\)** |
| `affineSliceDomains` | \(28+1+32+8+3=72\) | below | **\(72+152s\)** |
| `affineSliceOffset` | \(24+1+28+4+3=60\) | \(28+8+3+68+6+2=113\) | **\(60+113s\)** |
| `affineSliceStrides` | \(20+1+24+4+3=52\) | \(24+8+2+34+3+1+3+1+2+4+3=85\) | **\(52+85s\)** |

The domain step is expanded as

\[
\begin{aligned}
152={}&32 &&\text{activation}\\
&+16 &&\text{four-list inspection}\\
&+2 &&\text{zero-count decision}\\
&+4 &&\text{count/step conversions, subtraction, multiplication}\\
&+68 &&\text{two signed-result helpers}\\
&+6 &&\text{their internal binds}\\
&+2 &&\text{start conversion and endpoint addition}\\
&+8 &&\text{two further start conversions and min/max}\\
&+7 &&\text{domain comparisons/conversion/branches}\\
&+2 &&\text{axis increment/demand}\\
&+3 &&\text{outer endpoint-result bind}\\
&+2 &&\text{low/high demands}.
\end{aligned}
\]

On a zero-count axis the endpoint is the converted start; the two endpoint arithmetic checks are skipped. Domain validation still runs.

The slice order is:

1. all signed starts;
2. all signed steps;
3. all nonzero-step checks;
4. all per-axis parent-domain checks;
5. nonempty offset derivation;
6. nonempty stride derivation;
7. normalization;
8. full target validation.

Thus a later signed failure can precede an earlier zero step, and an empty result does not waive parent-domain checks.

### 8. Flattened result/reference construction

Source: I:579–594, 744–751; report/budget declarations at I:367–411.

`affineFinish` has thirteen explicit arguments:

\[
T(13)=64.
\]

Its non-validator work is:

| Construction/control | Work | New surviving slots |
|---|---:|---:|
| `AffineMap`: eleven fields plus conservative tag | 24 | 12 |
| `AffineBudget`: two fields plus tag | 6 | 3 |
| `AffineMapReport`: two fields plus tag | 6 | 3 |
| Pending continuation application: continuation plus three supplied objects and control | 10 | 5 |
| Outer `Right` | 3 | 2 |
| Validator bind and triple-field reads | 6 | transient |
| Three strict-binding checks | 3 | transient |
| Activation | 64 | transient |

Therefore:

\[
\operatorname{FinishOverhead}=64+24+6+6+10+3+6+3=\boxed{122},
\]

and new fixed survivors are

\[
12+3+3+5+2=\boxed{25}.
\]

Including the normalized stride spine:

\[
\boxed{R_B=25+2r}.
\]

The existing base/view witnesses are shared through **new charged fields**. A derived witness contains neither a parent-map field nor an operation-history field. It retains the original base, target witness, flattened offset/strides, and numeric facts.

The pending continuation application is explicitly counted. Arbitrary computation or allocation performed by that continuation is excluded; the engine does not force its `value` to produce the outer `Right`.

### 9. Complete post-body work polynomials

Define the common enumeration polynomial

\[
J(r,V,P)=V(208+96r)+P(284+192r).
\]

The following sum schedules count helpers even on branches that skip them, where noted. This is overreservation, not a claim that the skipped traversal executes.

#### Initial map

The displayed sum is

\[
\begin{aligned}
W_{\rm new}\le{}&
6+39+15\\
&+29+(48+58r)+(43+22r)\\
&+(18+30r)+(18+27r)+(28+42r)\\
&+122+(304+246r)+J.
\end{aligned}
\]

The 15 local wrapper primitives are three binds (9), two strict-list checks (2), the raw-empty condition (2), the normalization count comparison (1), and offset conversion (1).

Thus

\[
\boxed{W_{\rm new}=670+425r+J(r,V,P)}.
\]

There is no \(b\) term: I:738–755 does **not** construct base dimensions after the header.

For \(V=0\), replacing the validator ceiling by its actual empty branch gives the tighter available bound

\[
\boxed{366+179r}.
\]

#### Permutation

\[
\begin{aligned}
W_{\rm perm}\le{}&
6+39+22\\
&+(48+58s)+(25+56s)+(42+64s+36D)\\
&+(18+30s)+(18+30r)\\
&+[59+r(94+31s)]+[25+r(94+31s)]\\
&+(28+42r)+122+(304+246r)+J.
\end{aligned}
\]

The 22 wrapper primitives are five binds (15), four strict-binding checks (4), and three empty-normalization/offset controls (3).

Before expanding \(D\):

\[
756+208s+506r+62sr+36D+J.
\]

Equivalently:

\[
\boxed{
W_{\rm perm}
=756+190s+506r+18s^2+62sr+J.
}
\]

Using the established \(s=r\):

\[
W_{\rm perm}=756+696s+80s^2+J(s,V,P).
\]

#### Reverse

\[
\begin{aligned}
W_{\rm rev}\le{}&
6+39+35+29+29\\
&+(18+30r)+(54+31r)+(54+31s)+68\\
&+(40+36r)+(28+42r)+122\\
&+(304+246r)+J.
\end{aligned}
\]

The 35 wrapper primitives comprise binds (18), five axis conversions (5), four derivation arithmetic operations (4), empty controls (3), strict-list checks (2), and an overreserved empty `Right` (3).

Thus

\[
\boxed{W_{\rm rev}=826+31s+385r+J}.
\]

Here \(s=r\), giving \(826+416r+J\). The common polynomial conservatively includes the nonempty lookup/offset path even for empty reverse.

#### Slice

\[
\begin{aligned}
W_{\rm slice}\le{}&
6+39+32+2(48+58s)+(42+28s)\\
&+(18+30s)+(18+30r)+(72+152s)\\
&+(60+113s)+(52+85s)\\
&+(28+42r)+122+(304+246r)+J.
\end{aligned}
\]

The 32 wrapper primitives are six binds (18), five empty controls (5), three strict-list checks (3), and both empty `Right` alternatives (6), conservatively counted together.

Therefore:

\[
\boxed{W_{\rm slice}=889+524s+318r+J}.
\]

No quadratic slice term is necessary for this source. Its offset, stride, domain, and normalization loops are all linear.

#### A common transform envelope, if integration requires one row

Since \(s=r\) after admission,

\[
\begin{aligned}
W_{\rm perm}&=756+696s+80s^2+J,\\
W_{\rm rev}&=826+416s+J,\\
W_{\rm slice}&=889+842s+J.
\end{aligned}
\]

Consequently the displayed coefficientwise domination gives

\[
\boxed{
W_{\rm transform}=889+842s+80s^2+J(s,V,P).
}
\]

This is derived from the individual sums, not a proposed policy-authoritative replacement.

### 10. Constructed, live, and surviving slots

#### Why construction is bounded numerically

Every counted bulk constructor has been expanded into slot assignments. Arithmetic operates on admitted bounded operands or explicitly widened temporaries. Calls and frames are expanded into individual assignments and transitions.

- A slot assignment constructs at most one assigned slot.
- Bounded arithmetic needs at most the frozen widened temporary/result allowance.
- A control transition is split from its argument/frame-field assignments.
- Lists, tuples, closures, maps, reports, and diagnostics are not counted as single allocation primitives.
- Neither `quotRem`, a recursive copy, nor an entire address traversal is an elementary allocation event.

Thus the frozen per-primitive eight-slot ceiling applies to this displayed schedule:

\[
\boxed{C_B\le 8W_B}.
\]

This statement follows from the expansion above; it is not inferred merely from defining \(L=8W+Q\).

Every list construction has an independent size inventory:

| Newly constructed list/object family | Total during a successful body |
|---|---:|
| Initial view dimensions | \(2r\) |
| Initial integer-input copy | \(2r\) |
| Initial/derived normalized strides | \(2r\) |
| Permutation source and target dimensions | \(2s+2r\) |
| Permutation selected strides | \(2r\) |
| Reverse pre-normalized copy | \(2r\) |
| Slice parents and target dimensions | \(2s+2r\) |
| Nonempty slice derived strides | \(2s=2r\) |
| Nonempty validator zipper | \(5r\) |
| Digit lists over all validator calls | \(2r(V+2P)\) |
| Final fixed result/reference packet | 25 |

These are **total constructed objects**, not simultaneously live totals.

#### Fixed simultaneous storage

The following deliberately allows mutually exclusive fixed phases to overlap, so it does not rely on opportunistic collection:

| Fixed live family | Slots |
|---|---:|
| Body roots/registers: policy, usage, charge, old usage, base/source/target, two raw lists, parent offset/strides, continuation, three ranks, two counts, and three control/result roots | 20 |
| `affineFinish` frame | 31 |
| Validator frame, or smaller signed-check frame in derivation | 15 |
| Active derivation/helper frame | 15 |
| Address-kernel frame | 15 |
| Digit frame | 11 |
| Dot frame | 13 |
| Address adapter | 9 |
| Ordinary address loop | 9 |
| Pair outer loop | 9 |
| Pair suffix loop | 11 |
| `affineFailure` frame | 7 |
| Diagnostic packet | 9 |
| Bounded numeric temporaries, expanded below | 56 |
| Dynamic helper closure slots | 19 |
| Intermediate tuples and `Either` references | 24 |

The sum is

\[
20+31+15+15+15+11+13+9+9+9+11+7+9+56+19+24
=\boxed{273}.
\]

The numeric 56 is itself the sum

\[
18_{\rm derivation}
+4_{\rm extrema\ outputs}
+6_{\rm contiguity}
+20_{\rm kernel}
+8_{\rm pair}
=56,
\]

using two slots for each listed potentially widened temporary. The intermediate-object 24 is extrema pair/result 5, validation triple/result 6, five bounded helper `Either` packets 10, and one quotient/remainder tuple 3.

This fixed workspace is distinct from the 25-slot final survivor packet.

#### Variable overlapping lifetimes

In the tables below, normalized output strides are assigned to \(R_B\), not charged a second time to workspace.

**Initial map**

| Phase | Live rank-dependent objects/frames | Workspace bound |
|---|---|---:|
| Dimension copy | dimension output \(2r\), strict-copy frames \(7r\) | \(9r\) |
| Integer-input copy | dimensions \(2r\), integer copy \(2r\), copy frames \(7r\) | \(11r\) |
| Normalization | dimensions \(2r\), integer copy \(2r\), normalization frames \(11r\); normalized output separately \(2r\) | \(15r\) |
| Extrema | dimensions \(2r\), normalized output \(2r\); bounded registers | \(2r\) |
| Address/pair validation | dimensions \(2r\), zipper \(5r\), one digit list \(2r\); output separately \(2r\) | \(9r\) |
| Return preparation | dimensions may remain \(2r\); output packet/strides survive | \(2r\) |

The integer-copy spine is not referenced by the fully constructed normalized result. It can cease being live before validation; selected scalar values remain represented in the charged output element slots.

Therefore:

\[
\boxed{H_{\rm new}=273+15r}.
\]

**Permutation**

| Phase | Workspace bound |
|---|---:|
| Source dimension construction | \(9s\) |
| Target dimension construction while source dimensions remain | \(2s+9r\) |
| Selected-stride construction, including \(9r\) strict frames | \(2s+13r\) |
| Normalization, including \(11r\) frames | \(2s+15r\) |
| Validation, even retaining selected-stride spine conservatively | \(2s+11r\) |

A direct domination of these displayed rows is

\[
\boxed{H_{\rm perm}=273+9s+15r}.
\]

**Reverse**

| Phase | Workspace bound |
|---|---:|
| Dimension copy | \(9r\) |
| Reverse/copy construction: dimensions, raw result, and at most \(9r\) frames | \(13r\) |
| Normalization | \(15r\) |
| Validation | \(9r\) |

Hence:

\[
\boxed{H_{\rm rev}=273+15r}.
\]

**Slice**

| Phase | Workspace bound |
|---|---:|
| Parent copy | \(9s\) |
| Target copy | \(2s+9r\) |
| Domain/offset traversal | \(2s+2r\), plus fixed registers |
| Derived strides: parent/target dimensions, \(2s\) output and \(11s\) frames | \(15s+2r\) |
| Normalization | \(2s+15r\) |
| Validation, conservatively retaining derived spine | \(2s+11r\) |

Therefore:

\[
\boxed{H_{\rm slice}=273+15s+15r}.
\]

All four have:

\[
\boxed{R_B=25+2r}.
\]

No \(V\), \(P\), or \(s^2\) survivor/peak term is needed: these traversals are tail-recursive or run sequentially, and do not retain their histories.

### 11. Complete body reservation table

These are numerical advisory envelopes, not authorized reservation formulas.

| Body | \(W_B\) | \(H_B\) | \(R_B\) | \(Q_B=H_B+R_B\) |
|---|---|---|---|---|
| New | \(670+425r+J\) | \(273+15r\) | \(25+2r\) | \(298+17r\) |
| Permute | \(756+190s+506r+18s^2+62sr+J\) | \(273+9s+15r\) | \(25+2r\) | \(298+9s+17r\) |
| Reverse | \(826+31s+385r+J\) | \(273+15r\) | \(25+2r\) | \(298+17r\) |
| Slice | \(889+524s+318r+J\) | \(273+15s+15r\) | \(25+2r\) | \(298+15s+17r\) |

For explicit constructed-slot bounds, multiply the work rows by eight. Writing

\[
8J=V(1664+768r)+P(2272+1536r),
\]

gives:

\[
\begin{aligned}
C_{\rm new}&\le5360+3400r+8J,\\
C_{\rm perm}&\le6048+1520s+4048r+144s^2+496sr+8J,\\
C_{\rm rev}&\le6608+248s+3080r+8J,\\
C_{\rm slice}&\le7112+4192s+2544r+8J.
\end{aligned}
\]

If the integrator uses the frozen \(L_B=8W_B+Q_B\) form, the resulting complete body polynomials are:

\[
\begin{aligned}
L_{\rm new}&=5658+3417r+8J,\\
L_{\rm perm}&=6346+1529s+4065r+144s^2+496sr+8J,\\
L_{\rm rev}&=6906+248s+3097r+8J,\\
L_{\rm slice}&=7410+4207s+2561r+8J.
\end{aligned}
\]

The added \(Q_B\) is conservative reservation surplus; construction was already bounded independently by the primitive schedule.

### 12. Prefix composition and failure cutoffs

Let \(W_p,H_p,R_p\) cover section A through the successful reservation boundary, including the nine incoming charge/usage slots in \(R_p\).

A conservative composition that assumes **all** prefix workspace remains live is:

\[
\boxed{
W=W_p+W_B,\qquad
H=H_p+H_B,\qquad
R=R_p+R_B.
}
\]

Then

\[
\boxed{Q=H_p+R_p+H_B+R_B}
\]

covers both the earlier prefix peak and later simultaneous storage. This is conservative simultaneous storage **within one operation**, not addition of high-water deltas across operations.

If section A supplies a smaller proved live-at-boundary workspace \(h_{p,\rm body}\), use instead:

\[
Q=\max\!\left(Q_p,\ R_p+h_{p,\rm body}+H_B+R_B\right).
\]

Do not substitute a smaller local body \(Q_B\) for the previous prefix peak.

For a post-admission checked failure:

- work and constructed slots are bounded by that operation’s complete body row;
- only one diagnostic packet is produced;
- no new map, budget, or report is returned;
- newly materialized caller-retained prefix objects still survive;
- transient body copies are not reachable from the returned diagnostic;
- newly retained diagnostic slots are at most nine.

Thus the body’s failure survivor bound is

\[
R_{B,\rm failure}\le9\le25+2r.
\]

Its live bound remains the complete body \(Q_B\), because failure can occur after substantial temporary construction. Failed immutable-budget attempts do not erase prefix memoization.

## Drift / contradiction check

1. **Do not import a post-header base-copy slope.** The actual new-map body copies view dimensions only. Base-rank production/retention belongs to the admitted prefix, not an invented `affineDimensions base` call.
2. **Do not replace \(V+2P\) kernels with \(V\).** The source recomputes both addresses for every pair.
3. **Do not omit suffix terminals.** There are \(V\) suffix-loop terminations even though there are \(P\) pair iterations.
4. **Do not count selected/derived strides as mere sharing.** Their scalar referents may be shared, but their new list slots are constructed and temporarily live.
5. **Do not exempt the lazy continuation-application closure.** Its engine-created references are included; arbitrary continuation execution is not.
6. **Do not charge parent history.** The derived map is flattened; parent-map referents are pre-existing, and no parent-map field survives.
7. **No new concrete unmapped safe-public-path operation was found.** The retained review’s missing geometry expansion was unfinished work. The schedule above supplies an expansion rather than declaring size alone a contract contradiction.

## Recommendation

Use these per-function schedules and individual operation rows as section B’s integration input. Independently audit the arithmetic and primitive accounting before choosing final coefficients.

Keep the prefix survivor transfer explicit, preserve the full earlier prefix peak, and reconcile any overlap in fixed diagnostic accounting rather than adding two global terminal allowances.

## Risks

- These envelopes assume the public opaque-parent invariant: parent stride length equals source rank and its numeric metadata was completed by the prior successful map construction. A privately forged arbitrarily long parent stride list is not covered by the rank-only reverse-copy bound.
- The `natVal` reread is of admitted scalar evidence. Production of fresh witness tails/dictionaries must remain in section A’s account.
- The logical-frame model is not a bound on compiler-dependent closure layouts or physical stack/heap behavior.
- No source execution, coefficient acceptance, or preservation-hash recomputation was performed.

## Need from main agent

No new product/API arbitration is required by this section. The integrator must align the nine incoming charge/usage slots with section A and independently verify the assembled numerical theorem.

## Suggested execution prompt

**No executor handoff is warranted.** This is read-only proof advice; implementation and policy changes remain unauthorized.