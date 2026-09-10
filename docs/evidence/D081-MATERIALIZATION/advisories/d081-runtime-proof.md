# Section C — runtime and producer-materialization schedule

## Inherited decisions

- This is read-only numerical advice, not coefficient approval, implementation authority, or a refreeze recommendation.
- The v2 authority replaces constant nonzero-prefix live credit with prospectively charged, scan-dependent credit.
- Newly produced framework metadata counts even when retained only through caller-held entry arguments.
- The prospective `finiteTensor` correction completes its existing validation decision before IO return.
- Prefix costs and coordinate kernels belong to other lanes. They are imported below, not charged again.
- The committed-registry invariant remains mandatory. Its single-buffer append-and-force allowance is **`24n+24`, exactly once**.
- Completed producer results are distinct from entirely pending caller-composed producer applications.

**Confidence:** high in the demand, retention, ordering, and publication analysis; moderate in the conservative elementary-machine schedules below. These schedules require integration review, particularly agreement on the expanded library-combinator lowering. They are not GHC allocation counts.

## Source basis and notation

`I` denotes the immutable585 source:

`/tmp/d081-guarded-implementation-uxc2eza6/blocked-handoff/source/packages/markovian-tensor/src/Markovian/Tensor/Internal.hs`

`S` denotes its adjacent `Shape.hs`.

The corresponding v2 `source-basis/current-Internal.hs` and `current-Shape.hs` supplied the remaining numbered source context. The refinement correction is from:

`/tmp/d081-retention-v2-B0ZMyODc/candidate/Markovian/Tensor/Internal.hs:1691–1702`

I read the completion direction first, both authorities, and the complete v2 review. No commands, mutations, executions, or delegation were performed.

Write:

- `rb`, `rv`: admitted base and view ranks.
- `B`, `V`: admitted logical counts.
- `n`: existing committed buffer count.
- `bB = [B>0]`, `bV = [V>0]`.
- `j(r)=max(r-1,0)`.
- `K_W(r)`, `K_Q(r)`: the other lane’s complete work and scratch bounds for **one** `affineAddress` invocation.

The prefix interface must provide:

- `W_A`, `C_A`: work and construction through successful full reservation.
- `Q_A`: earlier local peak.
- `U_A`: newly constructed prefix/admission storage still live when the body starts, including surviving frames and references.
- `S_A`: prefix/admission storage surviving the operation’s cutoff.

`U_A` must not silently be replaced by only witness-list survivors. Conversely, no second global entry allowance is added here.

---

## 1. Elementary mapping

The following uses frozen §6.1/§8.1 units:

- A field/tag inspection, bounded comparison/arithmetic operation, assignment, branch/control transition, or peek/poke is an elementary operation.
- A list cell and element/reference slot cost two slots.
- A widened numerical temporary costs two slots.
- A continuation is represented by its two control slots and separately assigned saved arguments.
- Records are assigned field by field. For conservatism, the tables also allow a constructor/tag slot.
- Newtype wrappers may be overcounted as two logical slots; no physical allocation claim follows.

Consequently:

| Elementary action | Newly constructed slots bounded by |
|---|---:|
| Inspection/comparison/branch | 2 |
| Scalar result assignment | 2 |
| Widened arithmetic, including retained previous result | 4 |
| Control transition, frame, and bounded argument/result registers | 8 |
| Record/list construction | One assignment per charged slot |

No row below treats `scanr`, `drop`, a traversal, or a bulk constructor as one work unit.

For a schedule with work `w`, this mapping gives `constructed ≤ 8w`. The displayed survivor/workspace schedules independently establish live storage. The integrator may therefore use the more conservative frozen form `8W+Q`; that identity is not the proof of the live bound.

### Imported-combinator qualification

The `scanr`/`drop` counts below expand their lazy list reductions, including suspended arithmetic and intermediate references. I did **not** inspect a separate compiler-library source snapshot of Prelude’s implementation. Thus the concrete remaining verification obligation is to match that expansion to the integrator’s selected frozen logical lowering—not to substitute a demand marker or assume a whole library call costs one unit. This is a specific callee-lowering verification issue, not an unspecified missing rank coefficient.

---

## 2. Deferred contiguous metadata

### 2.1 Witness and dimension production

Sources: `S:25–50`; `I:1235–1243`.

A completed stock `knownShape` constructor does not complete its tail. A completed IO constructor can also retain lazily packaged shape/layout output. Reified shape preparation finishes its recursive validation before batch allocation, but that does not require every subsequently used `SCons` field to have been materialized.

A conservative expansion is:

| Producer | Per-axis work expansion | Work |
|---|---|---:|
| Stock/reified witness constructor completion | 3 dictionary/proxy/tail reads + 4 constructor assignments + 3 tail-recipe assignments + 2 transitions | `12r+4` |
| `shapeDimensions` spine | 1 shape match + 3 evidence/tail reads + 2 cons assignments + 4 dimension-recipe assignments + 3 tail-recipe assignments + 2 transitions | `15r+4` |
| Dimension value completion | 2 evidence reads + 1 return + 1 conversion + 2 result assignments + 2 force/control operations | `8r+4` |

Thus complete dimension spine/value production, **excluding witness production**, costs:

\[
D^{producer}_W(r)=23r+8.
\]

These dimensions are bounded because the corresponding shape has been admitted. This does not authorize an arbitrary caller-written `KnownShape` implementation.

The conservative storage schedule is:

| Component | Potential survivors | Additional frames/scratch |
|---|---:|---:|
| Newly completed witness nodes and associated recipes | `9r+2` | `4r+8` |
| Dimension cells, recipes, and completed numeric results | `11r+4` | `4r+12` |

The survivor ceilings deliberately retain both a recipe reservation and its eventual bounded result. Actual completed dimension-list storage is smaller: two list/reference slots plus, conservatively, two numeric slots per axis.

This overreservation must not be mistaken for two actual copies.

### 2.2 Actual `scanr`/`drop`

Source:

```haskell
contiguousStrides dimensions = case dimensions of
    [] -> []
    _ -> drop 1 (scanr ((*) . toInteger) 1 dimensions)
```

For positive rank, expand:

```text
scan []       = [1]
scan (d:rest) = let suffix = scan rest
                   value  = toInteger d * head suffix
               in value : suffix
drop 1 (_:xs) = xs
```

A lazy-pair implementation of the scan additionally carries the pair/selectors represented in the following allowance.

Per input axis:

| Primitive group | Work |
|---|---:|
| Input match and fields | `1+2` |
| Tail recipe assignments | `3` |
| Conversion/product/head-selection recipes | `10` |
| Output cons assignments | `2` |
| Lazy pair assignments | `3` |
| Pair-selector recipes | `6` |
| Saved frame/arguments | `4` |
| Dispatch/return controls | `4` |
| **Sum before rounding** | **35** |
| Used upper bound | **40** |

The terminal/drop schedule is bounded by:

```text
input-empty test                         1
scan setup                               3
terminal [1] assignments                 2
terminal pair                            3
drop test/match/select/decrement          4
dispatch/return                          5
fixed reference/control assignments      8
                                        --
                                        26 ≤ 32
```

A demanded suffix multiplication costs at most:

```text
dimension conversion                     1
suffix/reference inspection              2
multiplication                           1
numeric-result assignment                2
forcing/return/control                   3
                                         -
                                         9 ≤ 12
```

Therefore:

\[
T_W(r,b)=40r+32+12b\,j(r).
\]

The scalar branch executes no `scanr`: the formula remains a conservative bound.

#### Exact production/demand schedule

| Case | Scan output cells constructed | Returned stride cells | Numeric multiplication demands |
|---|---:|---:|---:|
| `r=0` | `0` | `0` | `0` |
| `r>0`, spine only | `r+1` | `r` | `0` |
| `r>0`, nonempty, every stride value demanded | `r+1` | `r` | `r-1` |

For `r>0`:

- The full-product head is constructed and discarded by `drop`.
- The terminal `[1]` cell is part of the returned list.
- The discarded full-product arithmetic is never required.
- On empty descriptors, the other `r-1` product recipes remain suspended.
- On nonempty descriptors, those suffix products are bounded by the admitted positive total product.

Thus `[M,M,0]` does not require computing `M*M` merely to establish either list spine. This is a production proof, not just a poisoned-element nonforcing claim.

A conservative retained-reference allowance per input axis is:

```text
returned/list reference reservation       2
arithmetic/selection recipes              10
pair                                     3
selectors                                6
tail recipe                              3
completed numeric result                 2
                                        --
                                        26
```

Hence use:

\[
T_R(r)=26r+8,\qquad T_H(r)=4r+16.
\]

The actual discarded prefix does **not** survive; its reservation remains in these deliberately conservative upper bounds. Transient scan/control frames are separately in `T_H`.

### 2.3 Complete per-tensor producer envelope

Adding witness, dimensions, stride production, and `CheckedLayout`:

\[
\begin{aligned}
P^{layout}_W
 &= (12r+4)+(23r+8)+(40r+32+12b\,j(r))+12\\
 &=75r+56+12b\,j(r),\\
P^{layout}_R
 &=(9r+2)+(11r+4)+(26r+8)+5\\
 &=46r+19,\\
P^{layout}_H
 &=(4r+8)+(4r+12)+(4r+16)+4\\
 &=12r+40.
\end{aligned}
\]

A single-item completion allowance of 48 work covers:

- at most 24 newly assigned wrapper/result/reference slots;
- at most 24 item, tuple, list-shape, and branch/control inspections.

Its retained and transient allowances are respectively 24 and 24 slots.

Successful pure reshape needs an additional record update: old-layout construction/reads, new field assignments, and control total at most 16 work; reserve another 12 transient and 4 potential-survivor slots.

The uniform bound covering these producer forms is therefore:

\[
\boxed{
\begin{aligned}
P_W(r,b)&=75r+120+12b\,j(r),\\
P_R(r)&=46r+47,\\
P_H(r)&=12r+76,\\
P_Q(r)&=58r+123.
\end{aligned}}
\]

For nonempty descriptors, `P_W ≤ 87r+120`; empty descriptors use `75r+120`.

This is **per newly demanded tensor item**, not per batch, producer history, or arbitrary pending pipeline.

---

## 3. Completed-producer table

Sources: `I:1552–1586`, `1593–1787`, `1796–1939`, `1949–2081`.

| Successful producer boundary | Work completed before boundary | Remaining materialization bound |
|---|---|---|
| `hostTensorFromList`, `finiteTensorFromList`, `makeFinite` IO return | Shape/count, payload preflight, input/finite checks as applicable, allocation, initialization, registry publication | One `P(r,b)` |
| Dynamic batch IO return | All plans, arity check, allocation-result spine, **entire output spine** | One `P(r_i,b_i)` for each subsequently demanded item; no additional `k` traversal |
| `makeTwo` IO return | Both initializers and common admitted count | At most `2P(r,b)` if both items are demanded |
| `makeTwoShapes` IO return | Both initializers and each operand’s admitted count | `P(rL,bL)+P(rR,bR)` |
| Corrected `finiteTensor` IO return | All payload reads and the existing finite-validation decision | No remaining `B`-sized refinement traversal; only remaining metadata already described by `P` |
| Observed `Right` from reshape | `checkedShape`, contiguity test, and `shapeElements` equality | Target `P(rTarget,b)`; no source-rank traversal is deferred by its offset-field selection |
| Completed direct transpose | Rank-two input pattern and strict outer result/layout fields; reverse spines needed for those fields | Remaining value/recipe debt is covered by `P(2,b)` |
| `ownTensor`, owner association | Fixed references only | No deep owner String or tensor traversal |
| `AddTape` VJP | Shares seed twice; no allocation publication | Fixed packaging only; seed’s own latent descriptor debt remains attributable to that seed |
| Unary/binary tape packaging | Fixed tuple and tape references | No traversal of retained primal referents merely because a tape references them |
| Other primitives/copy/VJPs | Payload computation reaches `makeFinite`, `makeTwo`, or `makeTwoShapes` before return | Corresponding rows above |

Important boundaries:

1. `makeFinite`, `makeTwo`, and `makeTwoShapes` use the admitted `count`, `leftCount`, and `rightCount` in their deferred wrappers. No post-return `shapeElements` pass is present.
2. Batch `zipWith makeDynamic` has its complete **outer** spine forced before IO return (`I:1608–1619`). Selecting one item cannot charge another traversal of all `k` plans or output cells.
3. `shapeElements = product . shapeDimensions` can perform substantial legacy work, including large intermediate products on a subsequently empty shape. For an **observed successful reshape**, that invocation has completed. It must not be imported into bind’s body.
4. An entirely pending reshape/primitive/pipeline invocation is a different caller boundary. Its exclusion does not excuse latent layout work inside a completed result.

---

## 4. Runtime validators and helper expansion

Sources: `I:594–709`, `994–1077`.

The following helper schedules are included to make the numerical body tables concrete. They describe the **calls from Section C**. If lane B supplies a different reviewed schedule for the same helper, substitute it; do not add both copies.

### 4.1 Shared geometry helpers used here

| Helper | Elementary work bound | Result slots | Scratch bound |
|---|---:|---:|---:|
| `affineDimensions` | `16r+8` | `4r` | `6r+8` |
| `affineNormalize` | `20r+8` | `4r` | `6r+8` |
| `affineZipper` | `16r+8` | `5r` | `8` |
| `affineSignedResult` | `20` | bounded scalar/Either | `16` |
| `affineExtrema` | `88r+16` | bounded pair/Either | total scratch/result `40` |
| `affineContiguous` | `20r+8` | one Bool | `16` |

Displayed per-axis expansions:

- `affineDimensions`: match/read/evidence/conversion `7`, forcing/frame `6`, cons/return `3`: **16**.
- Normalize: two matches `2`, fields `4`, empty/singleton decisions and selected value `4`, saved frame `4`, cons `2`, call/return `2`: **18 ≤ 20**.
- Zipper: matches `2`, fields `4`, pair `3`, cons `2`, argument assignments `3`, transition/branch `2`: **16**.
- Signed result: inspection `1`, conversions/negation `3`, comparisons `2`, branches `3`, maximum bounded error assignments `7`, returns `4`: **20**.
- Extrema: local match/field/arithmetic/register/bind/transition schedule **28**, plus **three** signed-result checks: `28+3×20=88`.
- Contiguous: match/pair/fields `5`, conversion/product `2`, comparisons `2`, Boolean control `3`, register/control/frame work `5`: **17 ≤ 20**.

Strict dimension/normalization recursion retains saved values and return frames. The zipper is accumulated tail-recursively. Extrema and contiguity return their recursive call after each local decision; they do not accumulate an `r`-element result history.

### 4.2 Metadata scanners

| Function | Iteration bound | Work |
|---|---|---:|
| `affineMetadataLength` | At most `r` accepted cons, then nil or first excess cons | `12r+24` |
| `affineMetadataDimensions` | At most `r` paired dimensions; stops at first mismatch | `16r+24` |
| `affineMetadataStrides` | At most `r` values | `10r+12+r×20` |

Length per accepted cons:

```text
match                                    1
field/cursor reads                       2
comparison                               1
branch                                   1
increment                                1
register assignments                     2
call/return                              2
frame/control assignments                2
                                        --
                                        12
```

The fixed 24 covers initialization, nil/excess inspection, bounded sentinel arithmetic, maximum diagnostic fields, and return control. A long list is rejected at its first excess cons; its value and tail are not demanded.

Dimension comparison uses two matches, four head/tail reads, comparison/branch, increment, three register assignments, and four frame/transition operations: **16**. Mismatch saturation and diagnostic construction fit the fixed 24.

Stride scanning has ten local match/field/index/control operations plus one fully expanded signed-result call.

### 4.3 Descriptor check

Let `D_c(r,i,b)` denote `affineCheckDescriptor`/`affineCheckLayout`, where `i` is the interval flag.

The non-traversal allowance is:

```text
wrapper/layout access and argument setup      20
count/capacity guard operations                12
stored-count check                            20
list/field selections                          6
offset conversion/range operations            14
empty decision/bounds                          6
extrema-result checks                         12
interval tests                                14
contiguity selection                           4
13 bounded do/case interfaces × 4              52
                                             ---
                                             160
```

Then:

\[
\begin{aligned}
D_c={}&160+2(12r+24)+(16r+24)\\
 &+b\{10r+12+20r+(88r+16)\\
 &\qquad+i[(16r+8)+(20r+8)]\}.
\end{aligned}
\]

Therefore:

| Branch | Work |
|---|---:|
| Empty | `232+40r` |
| Nonempty, no interval | `260+158r` |
| Nonempty, interval | `276+194r` |

A conservative scratch bound is:

\[
32+16+16+16+40+(5r+8)+16
=144+5r\le160+5r.
\]

Producer storage is **not** included in this scratch bound; it is charged separately by `P`.

Source order remains:

1. count/capacity;
2. stored count;
3. dimension length;
4. stride length;
5. dimension values;
6. offset;
7. empty physical bound **or** stride values/extrema/interval/contiguity.

### 4.4 Shifted view

For `affineShiftLayout`, the fixed wrapper, offset, count/bounds, construction, branch, and return schedule totals 88 operations; use 96.

\[
\begin{aligned}
D_s(r,b)={}&96+(12r+24)+20+(20r+8)\\
 &+b[(10r+12)+20r+(88r+16)\\
 &\qquad +(16r+8)+(20r+8)].
\end{aligned}
\]

Thus:

| Branch | Work |
|---|---:|
| Empty | `148+32r` |
| Nonempty | `192+186r` |

Its result is the normalized stride list plus a five-slot layout reservation. Its scratch bound is:

\[
24+16+16+(6r+8)+40+(5r+8)+16
=128+11r.
\]

The dimensions are the admitted copy supplied by the caller. There is no second dimension/range scan while constructing the result.

---

## 5. Bind body

Source: `I:1080–1114`.

### Call schedule

After full reservation:

1. Complete/access the actual supplied base: one `P(rb,bB)`.
2. Check contiguity and cached counts.
3. Construct **two** dimension copies: base and view.
4. Validate the actual base descriptor with interval checking.
5. Construct the shifted view.
6. Construct binding, reports, updated state, and result.
7. Publish the affine-usage update; no allocation or registry traversal occurs.

Fixed body work is:

```text
base wrapper access                         5
require-contiguous                          7
two cached-count checks                    40
copy-call setup                             4
base tensor field access                    8
view HostTensor/FiniteTensor               10
binding                                     4
nested reports                              9
affine report                               4
state copy                                  8
result/candidate packaging                 10
fixed result/record forcing                49
five do interfaces × 4                     20
post-prepare publication/delivery          10
local reference assignments                 4
                                          ---
                                          192
```

Therefore:

\[
\begin{aligned}
W_{bind,C}={}&192+P_W(rb,bB)\\
 &+(16rb+8)+(16rv+8)\\
 &+D_c(rb,1,bB)+D_s(rv,bV).
\end{aligned}
\]

Expanding:

\[
\boxed{
\begin{aligned}
W_{bind,C}={}&708+131rb+48rv\\
 &+bB(44+154rb+12j(rb))\\
 &+bV(44+154rv).
\end{aligned}}
\]

A branch-independent ceiling is:

\[
\boxed{W_{bind,C}\le796+297rb+202rv.}
\]

### Survivors and overlapping workspace

The supplied base is retained by:

```haskell
OwnedAffineView supplied witness view
```

Its new metadata therefore survives whether or not some other caller root also retains it.

Survivors:

```text
actual-base producer                 P_R(rb)
view dimensions                      4rv
normalized view strides              4rv
layout/result/report/state fields     45
```

Hence:

\[
\boxed{R_{bind,C}=92+46rb+8rv.}
\]

The base dimension copy does **not** belong to the returned binding; it is workspace.

Use:

\[
\begin{aligned}
H_{bind,C}={}&64+P_H(rb)\\
 &+(6rb+8)+4rb+(6rv+8)\\
 &+(160+5rb)+(128+11rv)\\
={}&\boxed{444+27rb+17rv}.
\end{aligned}
\]

The fixed 64 is the displayed sum of 26 local references/registers, 12 control-frame slots, 12 saved arguments, eight bounded temporaries, and six lock/handler controls.

Therefore:

\[
\boxed{Q_{bind,C}=536+73rb+25rv.}
\]

Using the uniform work ceiling:

\[
\boxed{8W_{bind,C}+Q_{bind,C}
\le6904+2449rb+1641rv.}
\]

These are body-local schedules, not proposed public row coefficients.

---

## 6. Pullback body

Source: `I:1117–1220`.

### 6.1 Relevant producer debt

A successfully returned binding has already completed all base/view metadata that subsequent bind/pullback descriptor checks require:

- dimension values and both spines;
- all nonempty stride values;
- normalized bound-view output.

For an empty base, irrelevant old product recipes can remain suspended, but pullback does not demand them either.

Thus the materialization debt specific to a subsequent pullback is the **independent actual seed**, conservatively one `P(rv,bV)`. The base and bound view are still checked again; their prior producer work is not executed again.

This does not exempt an independently caller-retained seed. Its new metadata is included in `R`.

### 6.2 Canonical gradient strides

Source: `I:1117–1131`.

| Branch | Per-axis schedule | Work |
|---|---|---:|
| `B=0` | match/fields `3`, saved recursion `4`, call/return `2`, cons/value assignments `3` | `12rb+8` |
| `B>0` | match/fields `3`, frame/control `6`, returned pair access `3`, singleton decision/value `3`, conversion/product/result `4`, cons/pair `5`, forcing `3`: **27 ≤ 32** | `32rb+12` |

Unlike stock `scanr/drop`, the nonempty canonical builder forces **`rb` products**, including its final `nextSpan`. All are bounded by the admitted nonempty product.

Use:

\[
G_R(rb)=4rb+4,\qquad G_H(rb)=7rb+16.
\]

The `7rb` scratch term comprises saved frames/arguments and intermediate build-pair slots. The output list and numeric values are separately in `G_R`.

Empty canonical gradients construct zeros directly: no suspended multiplication recipes.

### 6.3 Zero/scatter

Source: `I:1135–1153`.

Zero loop per element:

```text
bound comparison/branch                  2
index conversion                         1
pointer arithmetic/poke                  2
increment                                1
register/argument assignments            3
IO/loop transitions                      3
                                        --
                                        12
```

Scatter per element, excluding coordinate kernels:

```text
bound test/branch                        2
two checked-Either success interfaces    6
kernel argument/setup transitions       13
conversions, pointer arithmetic,
peek and poke                            6
increment/register/loop controls         5
                                        --
                                        32
```

Fixed pointer/loop setup totals 37; use 40.

Therefore:

\[
\boxed{
W_{init}=40+12B+V(32+2K_W(rv)).
}
\]

Exactly:

- `B` destination writes of positive zero;
- then, if `V>0`, `V` seed reads and `V` destination writes;
- two address calls per scatter iteration;
- no destination or seed-value list.

Only one coordinate workspace is live at a time. The source address scalar survives the target kernel, and the read scalar survives until its poke. Those bounded scalars are in fixed workspace.

### 6.4 Staging and publication

Sources: `I:1157–1190`, `1453–1533`.

Single-allocation staging shell:

```text
mask/setup                                  8
two pending-list inspection interfaces      20
allocation try/evaluate/case shell          20
initializer try/case shell                  20
advance/staged-cons/arguments               12
reverse singleton and nil                  12
force fresh singleton and nil               8
prepare try/evaluate/result shell          20
failure-handler roots/control              24
                                           ---
                                           144
```

Allocator execution is excluded; the initializer body is counted separately.

The pullback-specific fixed prepare/result schedule totals **334**:

```text
binding/base/view accesses                  24
contiguity                                  7
cached-count checks                        40
dimension-call setup                        4
initializer capture                        10
prepared tuple/forcing/elimination          27
initializer request                         7
pointer-list callback selection             6
tensor/layout/owner/ID fields               20
counter arithmetic and state fields        15
reports and output packaging               21
fixed field forcing                        51
owner access                                4
bounded do/case interfaces                  24
local registers and closure assignments    62
publication/delivery                       12
                                           ---
                                           334
```

Registry work, using the preserved invariant:

\[
16n+8+8(n+1+1)=\boxed{24n+24}.
\]

This appears once. It is not accompanied by another fixed terminal 24 or old-history normalization.

Combining fixed administration, staging, initializer setup, and registry terminal:

\[
334+144+40+24=542\le544.
\]

### 6.5 Numerical work table

The successful body sum is:

\[
\begin{aligned}
W^{success}_{pull,C}={}&544+24n+P_W(rv,bV)\\
 &+(16rb+8)+(16rv+8)\\
 &+D_c(rb,1,bB)+2D_c(rv,0,bV)\\
 &+2bV(16rv+8)+G_W(rb,bB)\\
 &+12B+32V+2VK_W(rv).
\end{aligned}
\]

Expanding:

\[
\boxed{
\begin{aligned}
W^{success}_{pull,C}={}&1384+68rb+171rv+24n\\
 &+bB(48+174rb)\\
 &+bV(72+268rv+12j(rv))\\
 &+12B+32V+2VK_W(rv).
\end{aligned}}
\]

A uniform successful ceiling is:

\[
1504+242rb+451rv+12B+32V+2VK_W(rv)+24n.
\]

Finite synchronous failure bookkeeping needs a separate prepaid margin. Excluding callback/String execution, its displayed ceiling is:

```text
cleanup-order reversal                 12
one finalizer-driver shell             15
terminal driver                         4
bounded cleanup-list combination       18
throw/cleanup selection                  9
allocation-error/result selection      14
rollback publication shell              8
classification and diagnostic refs     10
                                       --
                                       90 ≤ 96
```

Thus a body envelope covering both successful preparation and finite failure prefixes is:

\[
\boxed{
W_{pull,C}
=1600+242rb+451rv+12B+32V+2VK_W(rv)+24n.
}
\]

This does not claim a finite bound for repeated asynchronous cleanup or external diagnostic execution.

### 6.6 Survivors

At successful cutoff:

```text
independently retained seed metadata       P_R(rv)
gradient dimensions                       4rb
canonical gradient strides                4rb+4
new registry spine/references              4(n+1)
gradient/report/state/result fields         46
```

Therefore:

\[
\boxed{R_{pull,C}=101+8rb+46rv+4n.}
\]

The seed term survives through the caller’s entry root even though the returned gradient need not reference the seed.

The 46 fixed fields are:

```text
layout5 + StorageId2 + HostTensor8 + FiniteTensor2
+ OwnedTensor3 + memory5 + nested report4
+ affine report4 + state8 + output tuple3 + Right2.
```

### 6.7 Simultaneous workspace

A conservative sum—not reuse of the same slack for several concurrent objects—is:

\[
\begin{aligned}
H_{pull,C}={}&128+P_H(rv)\\
 &+(6rb+8)+(6rv+8)+4rv\\
 &+(160+5rb)+2(160+5rv)\\
 &+2(5rv+8)+(7rb+16)\\
 &+bV K_Q(rv)+4n\\
={}&\boxed{732+18rb+42rv+4n+bV K_Q(rv)}.
\end{aligned}
\]

The fixed 128 comprises:

- 48 local references/registers;
- 32 bounded frame/argument slots;
- nine request/staged/fresh-list slots;
- 19 initializer/prepared-tuple slots;
- 20 bounded numeric, diagnostic, and cleanup-list slots.

This deliberately sums some sequential scratch maxima. It does not omit the simultaneously live view-dimension copy or either zipper.

Therefore:

\[
\boxed{
Q_{pull,C}=833+26rb+88rv+8n+bV K_Q(rv).
}
\]

The corresponding conservative construction reservation is:

\[
\boxed{
\begin{aligned}
8W_{pull,C}+Q_{pull,C}
={}&13633+1962rb+3696rv\\
 &+96B+256V+16VK_W(rv)\\
 &+200n+bV K_Q(rv).
\end{aligned}}
\]

The coordinate terms remain symbolic and appear once per actual kernel call.

---

## 7. Phase/lifetime composition

| Phase | Newly materialized inputs retained | Other simultaneously live objects |
|---|---|---|
| After header/full reservation | `U_A` | Admitted ranks/counts, charge/usage, remaining preparation frames |
| Bind: actual-base completion | Base `P_R` | Producer frames plus both dimension-copy preparation contexts |
| Bind: descriptor validation | Base metadata | Base/view dimension copies; descriptor zipper/extrema scratch |
| Bind: shifted-view construction | Base metadata | Both dimension copies; normalized result; shift zipper/frames |
| Bind: publication | Base metadata and returned view | New state/report/result fields; final fixed controls |
| Pullback: descriptor checks | Independent seed metadata | Base/view dimension copies; descriptor scratch |
| Pullback: initializer preparation | Seed metadata | Both reusable zippers, both dimension copies, initializer closure, prepared tuple |
| Pullback: zero/scatter | Seed metadata | Both zippers, dimension copies, staged pointer, one coordinate workspace, bounded address/value scalars |
| Pullback: gradient construction | Seed metadata | Gradient dimensions/strides; initializer/preparation roots conservatively retained |
| Pullback: registry preparation | Seed metadata and gradient | New registry copies, up to `4n` append frames, staged/fresh lists, prepared result/state |
| After actual put | Seed metadata through caller root; gradient and registry through result/state | No staged rollback ownership |

The whole-operation composition is:

\[
\begin{aligned}
W_{full}&=W_A+W_C,\\
C_{full}&\le C_A+8W_C,\\
Q_{full}&\le\max(Q_A,\ U_A+Q_C),\\
R_{full}&\le S_A+R_C.
\end{aligned}
\]

This explicitly covers the earlier prefix peak and later overlap. It does not add successful peaks across operations.

---

## 8. Finite refinement and failure schedules

### Corrected `finiteTensor`

Candidate `I:1691–1702`:

```haskell
values <- tensorToList tensor
case validateFiniteInput 0 values of
    Left problem -> pure (Left problem)
    Right () -> pure (Right (FiniteTensor tensor))
```

The existing read action completes before validation begins. Choosing the returned IO branch then completes the validation decision.

For `t` inspected values, expand `finite` into bounded IEEE classification/comparison and Boolean control. A conservative per-value schedule is:

```text
list match/fields                  3
finite classification/control      6
guard                              1
increment                          1
registers                          2
tail transition                    1
                                  --
                                  14
```

Hence:

\[
W_{validate}\le14t+16.
\]

- Success: `t=B`, followed by validator nil.
- First nonfinite at index `j`: `t=j+1`, no validator nil.
- All `B` payload reads still precede either outcome.
- The finite validator creates no `B`-sized result list.
- Its input list was created by `tensorToList` and is transient to the refinement action.
- Successful return leaves **zero** pending validation iterations.
- The returned refinement shares the actual tensor; it does not substitute a new layout or payload.

The old `pure $ case ...` has a concrete unmapped-to-rank-only operation: as many as `B` deferred validator iterations. The authorized candidate removes that debt at the selected IO boundary. No `B` term is added to bind.

### Failure cutoffs

| Failure | Finite Section-C consequence |
|---|---|
| Closed/disabled/counter, header, old payload, full-reservation failure | No actual base/seed producer or Section-C descriptor work has begun. Use lane A only. |
| Contiguity/cached-count failure | A finite prefix of actual-base materialization may survive through the supplied base. |
| Metadata length/dimension/stride/physical failure | Prior producer output and copies have already been materialized; unchanged ledgers do not undo them. |
| Allocation failure | No gradient publication. Preserve caller-retained seed metadata; old state remains authoritative. |
| Initializer/address failure | Zero/scatter finite prefix is bounded by the prepaid initializer schedule; staged disclosed pointer remains cleanup-owned. |
| Gradient/registry/pre-put exception | Candidate metadata and copies may coexist with cleanup bookkeeping; no counters or storage ID commit. |
| Exception after put | State, counters, ID, registry, and ownership remain committed even if result delivery fails. |
| Repeated async cleanup or external String/finalizer execution | No total finite work/storage claim; do not relabel it a bounded engine failure. |

Normal bounded diagnostics fit the result/diagnostic storage ceilings above. On failed attempts, newly materialized input metadata can survive; unsuccessful candidate result/registry objects are not thereby published.

---

## Diagnosis

The missing base-rank bind retention and independent-seed pullback retention are real. They are not repaired by changing only the reported `R` constant or by borrowing already-used coordinate workspace.

The source also supports two useful non-duplication facts:

1. A completed binding has discharged the relevant base/view materialization before a later pullback.
2. Completed batch collection work and completed finite refinement do not become hidden rank-only consumer debt.

The numerical schedules above make those distinctions explicit.

## Drift / contradiction check

No new product/API contradiction was established.

The following would contradict the inherited contract:

- omitting `P_R(rb)` from bind because the base entered through an existing reference;
- omitting `P_R(rv)` from pullback because its result discards the seed;
- counting only returned stride cells while omitting the scan prefix, recipes, and frames;
- charging both `24n+24` and a second registry terminal allowance;
- admitting old append-history or batch-spine debt under the producer coefficient;
- treating corrected `finiteTensor` as permission to force all metadata eagerly;
- replacing `max(Q_A,U_A+Q_C)` with only the later body peak.

## Recommendation

Use the call equations and lifetime composition as Section C’s integration input. Reconcile the shared helper/closure lowering with lanes A and B, then substitute their prefix and coordinate schedules and independently recalculate complete reservations and fixtures.

The numerical ceilings here receive **no policy authority** from this advice.

## Risks

- The expanded `scanr`/`drop` logical lowering needs explicit integration agreement; demand controls alone do not certify its allocation mapping.
- Shared helper schedules must be substituted, not added twice.
- Prefix admission frames must remain in `U_A` until their discharge is source-established.
- These deliberately conservative survivor ceilings are not tight allocation measurements.
- External callbacks, arbitrary pending pipelines, and repeated asynchronous cleanup remain outside the finite theorem.

## Need from main agent

No new scope or API decision. Numerical integration and independent review remain necessary before any coefficient adoption.

## Suggested execution prompt

No executor handoff is warranted from this read-only advisory.