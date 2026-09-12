# D081 materialization addendum

D-081 is Accepted within its [unreleased host-F64 scope](../evidence/D081-AFFINE-IMPLEMENTATION.md).
This addendum replaces the resource, producer, failure, and fixture requirements
in §§6–9 and 11 of the [affine contract](D081-AFFINE-VIEWS.md) where specified.
Geometry, public signatures, roles, and other unaffected requirements retain
their original meaning. Section 8 consolidates the diagnostic, order, header,
count-helper, and batch clarifications.

The [source proof](../evidence/D081-MATERIALIZATION/SOURCE-PROOF.md) reconciles
the detailed derivations. Its substitutions take precedence over preliminary
coefficients. The [operation table](../evidence/D081-MATERIALIZATION/FIXTURES-OPERATIVE.md),
[fixture inputs](../evidence/D081-MATERIALIZATION/FIXTURE-INPUTS.md), and
[failure table](../evidence/D081-MATERIALIZATION/FAILURE-FIXTURES-FINAL.md)
define expected results. See the [source basis](../evidence/D081-MATERIALIZATION/README.md#source-basis)
for the historical line references used in the derivations.

## 1. Replacement §6.1: units, roots, completion and cutoffs

A cell is a logical bounded scalar, reference or control slot. Cons/link plus element/reference occupies two slots. Count any newly constructed numeric referent separately from a reference slot; the inventories conservatively allow two slots per newly represented bounded number. Existing referents are not recursively copied or charged. Widened arithmetic temporaries occupy at most two slots. A work unit is an individual field/tag inspection, bounded comparison/arithmetic, assignment, control transition, peek or poke. Every aggregate is expanded field-by-field; neither a function call, list traversal, dictionary computation, nor `scanr` is one primitive. Counts concern this logical machine, **not GHC closures/heap/stack, CPU, GC, bit complexity or physical reclamation**.

At an attempt's entry, roots comprise the supplied arguments, retained caller input roots, current policy/ledger/session state, and already-existing library roots. A node is pre-existing only if constructed before entry. A pre-existing suspension/recipe does not make the framework output it later produces pre-existing. Newly produced stock SShape nodes, dimension/value lists, lazy stride machinery, wrappers, scalar referents and referencing slots count wherever demand occurs. New library CAF output is also counted if first materialized during the attempt. Reaching the same object through multiple roots does not duplicate the object; separate newly assigned references do count separately.

Additional live storage is the union of newly constructed identities reachable from any active engine continuation/register, returned/result/error/state root, retained caller entry root, or library-held root at the observation point. Potential-survivor capacity R covers result/input/state/diagnostic identities from construction time, including candidates that may later be discarded; actual identities reachable at cutoff from surviving roots are bounded by R. Workspace H covers the disjoint remaining workspace identities. On rollback a candidate need not survive to have occupied its reserved R capacity; H is not a separate bound on every object ultimately discarded by a failed trace. Numeric objects can transfer from transient to surviving classification when installed in an output/diagnostic; this is not another construction. Conservative maxima may reserve both alternative lifetimes, without asserting two actual copies.

Assume the caller retains every supplied input. In particular, bind counts the supplied-base producer and pullback counts an independent seed producer even when no result references the seed. Discarding a seed cannot change a reservation. Earlier completed/materialized output is old input at a later attempt, but no refund or global retry allowance follows.

Completed IO producers mean their IO action has returned after the required checks/publication; no additional caller observation is required for finite validation. Completed pure producers mean the successful result constructor required by the API has been observed (for reshape, `Right`). These boundaries can leave the specific rank-bounded metadata debt in §6. An entirely pending caller-composed reshape/primitive pipeline or arbitrary user instance computation is different and excluded as caller computation. Do not extend that exclusion to latent framework output inside completed results. Stock `knownShape = SCons Proxy knownShape` is expressly in scope; outer WHNF is not complete witness normalization.

Pure success cutoff is the produced outer `Right`, before arbitrary continuation execution. Charge the engine-created continuation application and captured references, not arbitrary work performed by that continuation. Pure checked failure cutoff is its outer `Left`. Runtime successful state cutoff is **actual `putMVar`**, not preparation or delivery. An exception after put leaves ownership/IDs/counters/registry committed. A finite pre-put failure restores old state; caller-retained memoized output can nevertheless survive. Allocator/finalizer execution, exception rendering/String assembly and repeated asynchronous cleanup remain excluded from a total finite theorem. Supported region-dependent work, including observations/refinement, must complete inside the session callback with all-exit child joins; nothing here adds liveness enforcement or safe post-finalization observation.

## 2. Fixed entry, startup and prefix admission

Use M=`maxBound :: Int`, converted without overflow. Enabled startup is:

| W0 | H0 | R0 | Q0 | L0 |
|---:|---:|---:|---:|---:|
|512|448|128|576|4672|

Initial successful usage is `(4672,512,576)`. Disabled affine usage is `(0,0,0)`. `affineLimits` checks six negatives in rank, dimension, elements, cells, work, live order, then minima **4672,512,576** in cells/work/live order. Rank/dimension/elements may be zero. Update policy minima, initialization and runtime counter minima together. No getter adds a ledger event.

For e admitted node inspections, the complete common pre-body/failure envelope is:

```
WA(e) = 2048 + 384e
HA(e) = 448 + 40e
RA(e) = 128 + 12e
QA(e) = 576 + 52e
LA(e) = 16960 + 3124e
```

Fixed F includes common policy/entry/formula/reservation/terminal administration **once**, including runtime transaction controls and optional fixed ownership-record/outer-witness completion when applicable. Owner completion is post-admission, forces no owner-witness tail, and has a separate14-slot fixed survivor allowance within RA's fixed128 (not borrowed from an earlier operation or startup charge); P covers the underlying tensor item, not that ownership packet. Failed attempts may spend this fixed independent allowance even with no remaining successful-path credit, as in the preserved contract. Thus a local failure W bound is not a promise to fit beneath the configured cumulative successful-work cap. There is no rank-, history- or retry-dependent free allowance.

Start local meter `(oldCells,oldWork,576)`. Before the next **cons or nil**, in order:

1. `nextCells = satAdd(M, localCells, 3124)`; compare with cells cap.
2. `nextWork = satAdd(M, localWork, 384)`; compare with work cap.
3. `nextLocalQ = satAdd(M, localQ, 52)`; compare **max(oldSuccessfulPeak,nextLocalQ)** with live cap.
4. Only on success return `(nextCells,nextWork,nextLocalQ)` and inspect the node.

The historical peak is a comparator, not the local-Q accumulator. The rejected node, raw element and tail are uninspected; one rejected debit is covered by F, not an admitted coupon. Do not reset the local meter between scans. Required limit diagnostics are cap+1, never a huge exact rejected requirement.

For valid starting usage the number of credit-admissible inspections is at most the minimum of the nonnegative floors `(cellCap-oldCells)/3124`, `(workCap-oldWork)/384`, `(liveCap-576)/52`, also requiring oldPeak≤liveCap. Semantic/rank/length exits can stop earlier. Growing local live includes fresh witnesses reachable only from caller roots; it is not an input-normalization prerequisite.

### Actual scan counts and order

| Operation | Scans | complete e |
|---|---|---:|
|New map|base, view, raw strides|b+2r+3|
|Permute|base, source, target, permutation|b+2s+r+4|
|Reverse|base, source, source again|b+s+r+3|
|Slice|base, source, target, starts, steps|b+3s+r+5|
|Bind|retained map base, retained map view|b+r+2|
|Pullback|retained map base, retained map view, **same map view again**|b+2r+3|

Transforms compare target/source ranks after the three shape scans, before lists. No actual base/view/seed wrapper or descriptor is demanded by the runtime header. Valid opaque maps supply previously admitted witnesses; rescans still consume every specified coupon. Initial/new targets can materialize stock witness output.

Cons: old effective rank → affine rank → old effective dimension → affine dimension. Rank failure precedes natVal. No raw signed element is inspected by the length scan. Nil: old effective elements (runtime only) → affine elements → machine F64 count. Product starts1, saturates at M+1 independently of configured element caps, and a late zero changes it to0; every later dimension and the terminating nil remain checked/charged. Zero precedes division in saturated multiplication. Old effective cap is min(oldCap,M). Machine failure is exactly `Left (TensorShapeError (MachineIndexOverflow (M+1)))` when count>M div8, without forming an oversized byte product. Scalar nil checks count1.

## 3. Replacement §8: all successful operation rows

Let b=rB, s=rS, r=rV, B=admitted logical base count, V=admitted target count, n=existing committed buffer count. For transforms s=r after their header. Define P=V(V−1)/2 (P=0 at V=0) and

`J = V(208+96r) + P(284+192r)`.

The exact proposed reservations are:

| Operation | W | H | R |
|---|---|---|---|
|New map|3870+384b+1202r+J|841+40b+99r|195+12b+28r|
|Every transform|4857+384b+2012s+384r+80s²+J|921+40b+139s+59r|219+12b+36s+16r|
|Bind|3920+852b+767r|1126+70b+63r|246+60b+20r|
|Pullback|5168+785b+1486r+12B+V(240+192r)+24n|1556+59b+129r+4n|277+20b+72r+4n|

Q=H+R and L=8W+Q give the fully expanded remaining rows:

| Operation | Q | L |
|---|---|---|
|New map|1036+52b+127r|31996+3124b+9743r+8J|
|Every transform|1140+52b+175s+75r|39996+3124b+16271s+3147r+640s²+8J|
|Bind|1372+130b+83r|32732+6946b+6219r|
|Pullback|1833+79b+201r+8n|43177+6359b+12089r+96B+V(1920+1536r)+200n|

The transform row overreserves the slice maximum prefix and the common geometry envelope. Permute/reverse do **not** execute fictitious slice inspections. Charge full rows exactly, not measured resource consumption, and do not add actual header coupons a second time.

After complete header and (runtime) old payload preflight, calculate full rows with saturated arithmetic capped at M, then check `oldCells+L`, `oldWork+W`, `max(oldPeak,Q)` in that order. Successful usage stores those two sums and the **maximum**, never a sum of peaks or peak deltas. Divide the even pair factor before multiplying; do not construct an overflowing P or byte product. Positive polynomial saturation is equivalent to min(M+1, exact polynomial); zero-factor special cases remain explicit. Formula overflow cannot underestimate L or invert cells-before-work precedence.

## 4. Proof composition and simultaneous storage

The detailed elementary expansions are in `SOURCE-PROOF.md` and the captured advisory appendices, with the reconciliation there controlling every overlap. This section states the completed proof interface; no unspecified coefficient remains.

Body envelopes, excluding the common fixed/prefix lane:

| Body | WB | HB | RB |
|---|---|---|---|
|New|670+434r+J|273+19r|31+4r|
|Permute|756+199s+515r+18s²+62sr+J|273+11s+19r|31+4r|
|Reverse|826+31s+394r+J|273+19r|31+4r|
|Slice|889+533s+327r+J|273+19s+19r|31+4r|
|Common transform (s=r)|889+860s+80s²+J|273+19s+19r|31+4r|
|Bind|1104+468b+383r|598+30b+23r|94+48b+8r|
|Pullback|1968+401b+718r+12B+V(240+192r)+24n|988+19b+49r+4n|113+8b+48r+4n|

For new/bind/pullback use their actual complete e. For the shared transform row use e*=b+3s+r+5≥actual e. Compose **W=WA(e*)+WB, H=HA(e*)+HB, R=RA(e*)+RB**. This deliberately retains the whole prefix workspace capacity throughout the body, rather than claiming it vanishes. Therefore Q covers both the earlier header peak and the later prefix/body union. The scalar coefficients in §3 are algebraic expansions of these sums, independently checked over2304 parameter cases; they are not inferred from native reports.

Simultaneous lifetimes (numeric referents included):

* Prefix j: constant common arena≤448, accumulated frames/header objects/numeric temporaries≤40j, potential witnesses/views≤12j plus fixed survivors≤128. A rejected debit produces no j+1 witness. Existing referents are excluded, new references are not.
* New derivation maxima: dimension-copy output+frames11r; dimensions+integer copy+frames15r; normalization19r. Output normalized list/numbers4r belongs to RB. Validation needs dimensions4r+zipper5r+one digit list/numbers4r=13r, below19r. No list of all addresses exists.
* Permute: source copy11s; then4s+11r; stride selection4s+17r; normalization4s+19r; validation at most4s+17r. HB=273+11s+19r dominates each. Reverse maxima11r,17r,19r,13r. Slice maxima11s,4s+11r,19s+4r,4s+19r,4s+17r, dominated by273+19s+19r.
* Bind: base producer, both expected dimension copies, base descriptor scratch and shifted-layout scratch are separately reserved simultaneously. Producer survivors stay reachable via the supplied base and returned binding; the base expected-dimension copy is transient. View dimensions/normalized strides are new survivors.
* Pullback: independent seed producer, base/view dimension copies, both reusable zippers, initializer/prepared tuple, one coordinate workspace, staged/fresh lists and registry preparation can coexist. Each appears once in HB/RB. Destination payload is excluded; there is no destination-value list. Canonical gradient dimensions/strides and new registry survive; caller-held seed metadata survives independently of returned gradient reachability.
* Each coordinate call has work104+96r and scratch64+4r; validation's local address adapter adds20 work, runtime direct calls do not. Calls run sequentially. In a pair, retain the first bounded address while the second kernel runs; fixed scratch covers that scalar. No V/P/s² live-history term is needed.

Every counted aggregate construction is decomposed into assignments. Each inspection/branch constructs at most two control/result slots; bounded/widened arithmetic including its current result uses at most four; dispatch/return has at most eight only after argument/frame fields are separately assigned. Lists, records, tuples, closures and memo updates never become bulk one-step allocations. Thus independently `c≤8w≤8W`; disjoint identity/lifetime inventories independently give q≤H+R and cutoff survivors≤R. Adding Q to obtain L is conservative reservation surplus, **not** either proof. No physical allocation theorem follows.

## 5. Replacement §7: complete failure envelopes

For any pre-body exit after e admitted inspections use **WA,HA,RA,QA,LA of §2**; constructed cells are more tightly ≤8WA. This covers fixed entry/policy/runtime-counter rejection e0, one rejected debit, all cons/nil/length/rank/dimension failures, transform-rank mismatch, old payload preflight and full-affine reservation failure. A failed candidate's next Q is an admission requirement, not materialized storage.

| Exit | e / cutoff |
|---|---|
|Policy, closed, disabled, counter, first debit|0; no variable input demand|
|Rank/dimension failure at zero-based axis k|earlier completed scans+k+1; rank failure leaves dimension unforced|
|Nil element/machine failure|earlier scans+rank+1|
|Short list t<expected|earlier scans+t+1|
|Long list expected t|earlier scans+t+1, excess element/tail unforced|
|Transform rank disagreement|b+s+r+3, before any raw list|
|Credit failure partway|only prior admitted nodes|
|Old payload / full affine rejection|actual complete header e, no actual tensor/seed demand|
|Post-admission geometry/descriptor failure|entire applicable W/H/R/Q/L row is a prepaid upper bound; immutable successful ledger unchanged|
|Allocation/init/address/pre-put failure|pullback row, including finite staging/cleanup and one bounded diagnostic; old state authoritative, disclosed pointer staged-owned|
|After actual put / before delivery|successful pullback/bind state, IDs and ownership remain committed; no rollback of the old state|
|Repeated async cleanup / external callback/String execution|no total finite bound; not relabelled a bounded engine failure|

Pure body failure can retain up to17 diagnostic/associated numeric slots (nine record/reference slots plus at most four two-slot numeric referents), not merely the advisory's nine reference slots. It retains no dimension/stride **spine** through the diagnostic. 17≤31+4r. Runtime failure survivors are bounded by the actual-input producer R plus17 and the common prefix survivors; this is below the full runtime R rows. Fault metadata does not establish the true size/provenance of a forged pointer; only safe opaque production supplies that invariant.

## 6. Replacement/addendum §9: producer, descriptor and publication contract

For one completed tensor item of rank r, b+=[logical count>0], j=max(r−1,0), the completed producer envelope is

```
PW = 87r + 120 + 12 b+ j
PR = 48r + 47
PH = 14r + 76
PQ = 62r + 123
```

It includes stock/reified witness output, dimensions/values, actual contiguous scanr/drop machinery, layout/wrapper references, single-item packaging and the successful reshape record update. It does not add a batch/history traversal. Empty stride numeric products stay suspended. Spine production still counts the discarded scan prefix, recipes, selectors and control frames. Nonempty suffix products remain bounded by the admitted positive total.

| Producer boundary | Completed work / residual item debt |
|---|---|
|hostTensorFromList, finiteTensorFromList, makeFinite IO return|shape/count/preflight/input or finite validation, allocation/init/publication complete; one P item|
|hostTensorBatchFromLists IO return|all plans, allocation-result arity and complete outer output spine done; only P(ri) for a demanded item, not k additional work|
|makeTwo IO return|both outputs initialized; ≤2P if both demanded, using the one admitted count|
|makeTwoShapes IO return|both initialized; P(left)+P(right), operand-specific admitted counts|
|Corrected finiteTensor IO return|all original payload reads then existing finite-validation decision completed; no B-sized validation debt in bind|
|Observed successful reshape/finite reshape|target checks, contiguity and shapeElements equality done; target P remains, not another source-rank traversal|
|Completed transpose/finite transpose|rank-two pattern and required strict result/layout fields/reverse spines done; remaining rank-two value/recipe debt≤P(2)|
|ownTensor, owners, bound-view accessors|fixed references plus at most one pending owner-witness outer constructor, priced in the common fixed pool; no owner String rendering, witness-tail or deep primal traversal|
|contiguousCopy, binaryElementwise/add/multiply, negate, tanh, sumAll, matmul, fillLike|payload computations reach makeFinite before return; its one-item debt|
|tanhWithTape, sumWithTape, addWithTape, multiplyWithTape, matmulWithTape|fixed tuple/tape packaging; retained primal referents are not copied or traversed|
|TanhTape/SumTape VJP|makeFinite/fillLike boundary; no additional pending primitive execution|
|MultiplyTape VJP|makeTwo boundary|
|MatMulTape VJP|makeTwoShapes boundary|
|AddTape VJP|fixed sharing of actual seed twice, no allocation; that seed's own metadata debt stays with it|
|Pure affine result / completed binding / affine gradient|normalized bounded fields/spines completed by their producer; no later recurrence of discharged relevant debt|

The three helpers use `count,count`, or `leftCount,leftCount` / `rightCount,rightCount`, not deferred `shapeElements`. Completed batch collection work cannot hide in selected-item P. An entirely pending caller-selected pipeline is not a completed item. No general eager metadata normalization is introduced.

`finiteTensor` alone has the already-authorized prospective strictness correction:

```haskell
values <- tensorToList tensor
case validateFiniteInput 0 values of
    Left problem -> pure (Left problem)
    Right () -> pure (Right (FiniteTensor tensor))
```

Keep the read action/order, original tensor, first nonfinite diagnostic, reports/counters and allocation/finalization behavior. Do not force layouts deeply or render errors. Existing validation costs≤14t+16 for t visited values; success visits B then nil, failure at j visits j+1, but **all B reads precede validation**. Its transient read-list is not retained by successful refinement. This is a producer-boundary proof, not an extra B term in bind.

Runtime order remains closed → enabled → counter ranges → retained-witness header → old single/fresh/buffer/scalar preflight → full affine L/W/Q → actual wrappers/metadata. Contiguous original base first, cached B then V, then descriptors base/view/seed as applicable. Each descriptor checks count then capacity≤M div8, count agreement, dimension length, stride length, dimension values, offset, and only then relevant nonempty stride/extrema/physical checks. Do not require seed/view contiguity or substitute offset+logicalCount for signed extrema. Empty descriptors still check every dimension and both spines, but no stride value/product/coordinate/count−1; anchor0≤offset≤capacity remains.

Bind shares storage, reads no payload and performs no registry walk/allocation. Pullback validates the actual independent seed, writes B positive zeros before any seed read, then does V seed reads and V destination assignments via **two** checked coordinate calls per element. Preserve signed zero, actual original-base owner/storage relation and nominal/opaque guarantees.

All allocating paths, including affine-disabled legacy paths, maintain the fully forced committed registry spine. For one new buffer, charge **16n+8+8(n+2)=24n+24 once**. No old append-history normalization, registry-only proof of all metadata, destination cache, address cache or deep pointer force is allowed. Staging extends through initialization, gradient metadata, candidate registry/result/state forcing; only `prepareSessionCommit`'s actual put transfers ownership. Pre-put finite cleanup and post-put delivery failures have different cutoffs as in §5.

## 7. Replacement §11 fixtures and required controls

`FIXTURES-OPERATIVE.md` gives the literal replacement table, and `FIXTURE-INPUTS.md` supplies its parameters. Each of the 19 paths requires exact admission and all seven nonempty subsets of individually one-below cells/work/live. They include signed/empty/scalar/singleton/raw-empty/overlap maps; all three transforms; bind; signed/empty pullback; mixed1024 and [512,0,512] histories; rank512 base/scalar view; independent rank512 seed; and post-admission descriptor failures.

A semantic failure's exact table is its admission threshold, **not a ledger commit**. Runtime paths start the separate session account at4672/512/576; they do not import pure planning charges. Peaks are maxima. The high-base bind→pullback fixture deliberately has an earlier dominating peak: lowering whole-path live by one rejects that **earlier bind**, potentially before a later cumulative cells/work shortage. Resource precedence is within each admission event, not a global sorting of eventual shortages.

Prefix replacements (starting4672/512/576, rank512 all-one first shape):

| lane | cells/work/live caps | admitted e | error |
|---|---|---:|---|
|work-first|1610408 /197120 /27304|512|AffineWork197120 required197121|
|live-first|1610408 /197888 /27199|511|AffineLiveCells27199 required27200|
|cells-first|1604160 /197888 /27304|512|AffineConstructedCells1604160 required1604161|

All errors are TensorAffineError-wrapped. The next cons/nil is uninspected; the fresh-stock/preforced distinction is supported by retained v2 evidence, while new demand-r2 controls count credited/requested inspections under these numerical coupons. Model counts are not native allocation counts.

`FAILURE-FIXTURES-FINAL.md` specifies 24 count/prefix/sentinel cases, including the uncredited-T-nil and startup-only scalar boundaries. For every old count/rank/dimension/list fixture retain its input, error spelling and admitted-e index, but replace failure W/H/R/Q/L using §2 and exact prefix policy with `(4672+3124e,512+384e,576+52e)`. At e0 the failure envelope is2048/448/128/576/16960; at e1 it is2432/488/140/628/20084; at e2 it is2816/528/152/680/23208; at e4 it is3584/608/176/784/29456. One less in a prefix cap rejects before that selected inspection; if credits permit, old-element→affine-element→machine and old-rank/dimension→affine priorities are unchanged. The retained b64 T=M div8+1 machine literals remain9223372036854775808 and the effective cap+1 spellings, not old work/cell/live literals. Six-negative-before-minima and individual minimum one-below controls are required.

These tables define expected outcomes. Current implementation checks and their limits are listed in the [implementation record](../evidence/D081-AFFINE-IMPLEMENTATION.md).

## 8. Self-contained declaration/error/order consolidation

## Exact five-clarification consolidation — unchanged scopes

This section is self-contained as to the selected changes. It does not reproduce unrelated original declarations; the public set remains exactly the four opaque types AffineLimits, AffineBudget, AffineMap, OwnedAffineView with existing signatures/nominal roles and original public diagnostic/report constructors. No new error constructor, report meaning, helper API, input identity rule or numerical coefficient is introduced by these five clarifications themselves; the separate operative retention amendment proposes its explicit replacement coefficients.

### C1. NEW scanner rank/dimension errors

Let M=maxBound Int and effective old cap E=min(oldCap,M). Required payload for exceeded rank/dimension cap C is **C+1**, computed in Natural/Integer, never original supplied dimension:

- Pure/new affine: `TensorAffineError (AffineLimitExceeded AffineRank C (C+1))` or corresponding AffineDimension.
- Runtime old cap: `TensorShapeError (RankLimitExceeded E (E+1))` or corresponding DimensionLimitExceeded.

Cons order remains debit-before-inspection → old rank → affine rank → old dimension → affine dimension. Rank rejection precedes natVal; nil is not rank overflow; dimension errors do not wait for later rank/zero. Old matching failure wins over affine matching failure. Thus999/cap5 yields required6 in the new scanner, but legacy checkedShape retains actual999. No helper rewrite or additional pass. Original nil product capM, late-zero rule and old-element → affine-element → F64-machine order remain. Exact machine spelling remains `TensorShapeError (MachineIndexOverflow (M+1))`.

### C2. POST-admission private metadata

All checks remain after header, old payload and full affine L/W/Q admission. Set F=M div8. Count then capacity exceeding F yields exactly `TensorShapeError (MachineIndexOverflow (M+1))`; do not multiply oversized values or use counter overflow. Count disagreement uses `TensorAffineError (AffineShapeDisagreement input 0 expectedCount actualCount)`.

List rank mismatch uses the same shape-disagreement constructor with axis0, expectedRank, observedRank: short exact; long first-excess expectedRank+1. At most expectedRank+1 cons nodes, no elements forced merely for length. Dimension mismatch at axis i uses expectedDimension and min(actualDimension,M+1); actual999 remains999 if bounded. Base input tag is AffineBaseShape; stored view and seed both AffineViewShape. Axis0 also denotes count/rank sentinel; no extra discriminator.

Bind and pullback first require original-base conservative contiguity; never require seed/view contiguity. Cached map B then V count fields are checked before use against admitted headers. Descriptor order is original base → stored view (pullback) → actual seed (pullback). Per descriptor: count/capacity ranges → count agreement → dimension length → stride length → all dimensions → offset range → relevant nonempty stride range/extrema/physical checks. Negative or >M offset is `AffineArithmeticOverflow AffineOffset 0`; relevant stride outside [-M,M] is `AffineArithmeticOverflow AffineStrides i`. Range-valid bad interval uses `AffinePhysicalBounds offset count capacity` under TensorAffineError. Noncontiguous seed/view uses signed extrema, **not** offset+count; offset4/count3/capacity5/stride-2 remains valid.

Empty descriptors still check both bounded spines and **every dimension value**. Do not evaluate stride values/products/extrema/count-1/coordinates; only anchor0≤offset≤capacity. An expensive runtime-owned spine producer must be accounted or escalated, not excluded as a caller thunk. Fault metadata cannot certify forged pointer provenance/actual capacity; no unsafe probe is authorized.

### C3. Runtime header witnesses

Bind scans retained map base then map view. Pullback scans retained map base, map view, **map view again** for seed-view. Preserve every nil/sentinel/policy check/coupon; no identity deduplication. These witnesses must have the complete admitted/forced invariant established by successful map construction. Accessing them must not demand old tensor wrappers. This clarification does not establish an initial caller-SShape normalization theorem.

After full admission demand actual supplied base/stored view/actual seed and compare stored counts/layout dimensions; keep actual owner, storage, offset/capacity and actual seed values. Witness equivalence never substitutes a tensor, pointer, owner or payload. Post-admission library demand still needs prepaid bounds; no hidden free normalization.

### C4. Three count helpers only

`makeFinite`: bind checkedShape's `(count,bytes)`, use count for logical count and capacity. `makeTwo`: same count in both fields of each same-shaped result. `makeTwoShapes`: leftCount twice on left, rightCount twice on right. Keep original checkedShape calls, operand failure precedence, admitted bytes, preflight/report/payload/allocation/finalization order. No second shape check, global shapeElements change or deep post-commit wrapper force. Wrappers stay post-commit/session-owned. Separate empty-stride production and other metadata still require accounting. Existing late-zero/O0/O2 mutant evidence is historical, not rerun here.

### C5. Batch arity/spine completion, lazy items

Before `hostTensorBatchFromLists` IO return, inspect allocated and execute the **existing** arity comparison with exact Left propagation/internal allocation-count error. On success complete the OUTER `zipWith makeDynamic plans payloads` spine through nil. Do **not** force DynamicHostTensor/HostTensor items, layouts or payloads. Keep prepared identified-allocation invariant and admitted counts; no batch-sized deferred debt may remain in an individual selected item.

All this remains after actual put. Session owns disclosed buffers; post-commit exceptions cannot restore old counters, finalize staged pointers again or extend staging rollback coverage. Preserve empty/singleton/large batch order and old reports/IDs. Caller selection/traversal after completed outer collection is external work, but deferred library arity/spine/item production cannot become external by calling it elimination. Bounded synchronous post-commit controls do not prove universal asynchronous callback counts or reclamation.

