# D-081 affine-view contract

D-081 is Accepted within its [unreleased host-F64 scope](../evidence/D081-AFFINE-IMPLEMENTATION.md).
This contract specifies geometry, ownership, error precedence, and validation.
The [materialization addendum](D081-MATERIALIZATION-ADDENDUM.md) specifies the
current resource model and overrides the requirements below where stated.
Section numbers are retained for references from that addendum.

## 1. Scope

The API adds immutable signed affine views and fresh original-base pullbacks.
Affine policy is optional and disabled by default; the existing seven-argument
session-limits constructor and old payload/scalar reports retain their meanings.
Signed layout arithmetic requires separate logical element counts and backing
storage capacities. There is no new tape alternative, general map composition,
device lowering, or runtime lifetime enforcement.

## 2. Source and evidence

- [Public API](../../packages/markovian-tensor/src/Markovian/Tensor/Affine.hs).
- [Private representation and runtime](../../packages/markovian-tensor/src/Markovian/Tensor/Internal.hs).
- [Implementation checks](../evidence/D081-AFFINE-IMPLEMENTATION.md).
- [Resource proof and fixtures](../evidence/D081-MATERIALIZATION/README.md).

The [original design record](https://github.com/josephjohncox/Markovian/blob/871f0eedf8a8f460b2b3a5906bec57a365d53e5d/docs/plans/D081-AFFINE-VIEWS.md)
contains the review history, baseline source inventory, superseded coefficients,
and preliminary fixture calculations.

## 3. Meaning, ownership and allowed reuse

For declared logical base count B and view dimensions d with count V and rank r:

`a(c) = o + sum_i c_i*s_i`, with `0 <= c_i < d_i`; `y[c] = x[a(c)]`.

Thus `a` maps view coordinates to flat **logical base** coordinates; primal gather goes base-to-view. A nonempty initial map accepts any bounded signed flat-affine descriptor with in-range addresses and injectivity. It need not be a rectangular slice of base dimensions. There is no overlap, broadcasting, mutation, dtype conversion, device lowering, arbitrary affine composition, or copy fallback.

The pure witness contains no tensor, owner, region, pointer or StorageId. Compilation can precede construction of the base. It constructs bounded Haskell metadata, not tensor payload; it is not allocation-free.

Binding explicitly supplies `OwnedTensor region owner 'F64 base`. Its layout must have the conservative increasing-row-major contiguity bit and a valid logical interval in actual backing storage. For physical offset b and capacity C, a nonempty base requires `0 <= b`, `b+B <= C`; an empty base permits `0 <= b <= C`. Physical view addresses are `b+a(c)`. Binding retains the **actual supplied** original base, actual owner witness, map, and shared finite-view wrapper. It does not infer identity from shape, values, owner strings, or storage equality.

Maps can deliberately be reused/rebound to different explicitly supplied compatible contiguous bases. A map from a different planning budget is also usable; current work is charged without importing its history. Equal-shaped valid cotangent seeds are intentionally accepted even if their storage/values/semantic origin differ from the primal. Seeds do not certify base identity or map identity. The binding, not the seed, fixes the pullback's original base and map.

`TensorOwner` is semantic evidence; two explicit keys can inhabit the same phantom owner type. Nominal roles forbid representational index changes, not deliberate use of the existing public owner constructor. Pullback returns the stored binding owner and a fresh **logical base-shaped** tensor, not a view-shaped tensor or capacity-sized allocation. No public operation accepts a substitute owner key or map at pullback time.

For finite seed λ and injective a, output is λ[c] at k=a(c), otherwise positive `0.0`. Direct assignment, not addition, is correct. Selected negative zero is preserved. Base payload is never read by bind or pullback.

## 4. Public API

`Markovian.Tensor.Affine` exports the four opaque types `AffineLimits`,
`AffineBudget`, `AffineMap`, and `OwnedAffineView`. Map and binding indices are
nominal. Their declarations, functions, and diagnostic/report types are defined
in `Markovian.Tensor.Internal` and reexported by the public module linked above.
Caller-constructed reports are not witnesses; there is no arbitrary-charge
commit API. The umbrella module exports affine diagnostics through `TensorError`
without reexporting all affine operations.

Each rank-2 continuation receives a fresh abstract nominal tag. Before calling it, fully force the admitted shape/descriptor spines and numeric facts, usage and report. A strict list field alone is insufficient. Caller continuation computation is outside engine cost: producing `Right value` must not require forcing arbitrary `value`. An exception/divergence in the continuation is not a checked validation failure. Existential map packaging and budget branching are allowed.

## 5. Arithmetic and precisely admitted transformations

Let `b = finiteBitSize (0 :: Int)` and `M = maxBound :: Int = 2^(b-1)-1`. The validation platform uses b=64, GHC9.14.1/base4.22.0.0. Correctness is parameterized by b, not silently hard-coded to 64.

Signed public inputs are Int. Accepted offsets/coefficients and every derived signed intermediate lie in `[-M,M]`; reject `minBound`. Convert to Integer before negation/multiplication. No `abs minBound`, unchecked signed conversion, or Natural subtraction. Checked multiply temporaries require at most 2b bits; bounded sentinels at most b+1. New counters are at most M. Comparing an already supplied huge Natural/Integer against a bounded cap is one **logical comparison**, not a claim about its bit complexity, GHC allocation, or thunk evaluation. Do not construct a giant converted copy/product before this comparison.

Every dimension is checked, including after a zero. If any dimension is zero, count is zero; otherwise bounded product, with empty product one. B,V must fit both affine element cap and `M div 8`. Header scanner details and the revised precedence are in §6. After admission, retained new numeric metadata is machine bounded. This does not retroactively bound legacy empty layout metadata.

Exact-length signed lists inspect at most expected+1 spine nodes. Nil is an inspected node. A too-long sentinel forces only the cons constructor, not its value/tail. Shape rank sentinel similarly does not force the excess dimension. No coefficient values are forced during length admission; all signed checks happen after full reservation, in argument/axis order.

**Initial empty rule:** after all signed-input checks, if V=0 require the ORIGINAL offset=0 and EVERY ORIGINAL supplied stride=0. Only then may singleton coefficients be discarded. `[0,1]` with strides `[0,7]` is rejected even though normalization would give zeros. For V>0, singleton strides normalize to zero after signed checks.

Nonempty extrema: in axis order compute `t_i=(d_i-1)*s_i` with checked multiplication, then `lo=o+sum min(0,t_i)` and `hi=o+sum max(0,t_i)` with checked partial sums. Arithmetic overflow precedes address bounds. Require `0<=lo<=hi<B`. The complete target validator then checks every address in logical order, and every collision pair in lexicographic `(i,j)` order, `0<=i<j<V`. First equal pair yields `AffineOverlap i j a`. No address list/table/set/sort is retained.

Coordinate kernel: build a reversed dimension/stride zipper once after admission; for each linear j, traverse it right-to-left using bounded `quotRem`, prepend digits to a temporary forward digit list, then traverse digits/forward strides left-to-right to perform checked dot product. Bounds precede Int conversion/pointer access. Scalar uses no digits; empty enumerates nothing and never evaluates V-1 or a zero-axis endpoint. The pair loop computes the two addresses sequentially, releasing digit workspaces between calls. §8 charges these copies/frames.

Only these three transformations exist; each flattened result is relative to the same **original logical base**, and retains no unbounded parent chain:

- **Permute:** source and target ranks agree; p has r entries. Check all axis ranges, then duplicates by `(j,k)` lexicographic order, then `targetDim[j]=sourceDim[p[j]]`. New offset is old, stride j is old stride p[j]. Use explicit charged scans, not uncharged `(!!)`. Duplicate loop walks suffixes rather than repeated indexing that would become cubic. Rank-zero `[]` is identity.
- **Reverse:** scalar axis must satisfy `0<=k<r`, including for empty tensors; rank zero always fails. For nonempty, `o'=o+(d_k-1)*s_k`, `s'_k=-s_k`; then singleton normalization. For a globally empty map validate the axis and canonicalize without computing any endpoint. This is a parent-coordinate bijection.
- **Slice:** target rank equals source. Starts and steps each have source rank entries. All signed checks precede all zero-step checks, then per-axis domain checks in order. Every step is nonzero, including empty/singleton axes. For target count n>0, checked `end=start+(n-1)*step` must have `min(start,end)>=0` and `max(start,end)<sourceDim`. For n=0, admit `0<=start<=sourceDim`, no endpoint calculation. Every other axis is still checked. Only after **all** source-domain checks, derive nonempty offset `o+sum start_j*s_j` and stride `s_j*step_j`, checked in axis order, then normalize. Empty result canonicalizes without irrelevant products. Coordinate inclusion cannot be replaced by backing-capacity checks.

After derivation all transforms rerun target signed/extrema/address/injectivity validation and pay the same conservative transform reservation; inherited injectivity does not earn a refund. Derived empty maps may canonicalize after their own axis/range/domain validations. There is no general `composeAffineMap`, reshape-map composition, absolute-descriptor replacement of a parent, or noncontiguous-base flat reindexing.

## 6. Ledger, constant control costs, and credit-before-prefix algorithm

### 6.1 Units and startup

The [materialization addendum, §§1–2](D081-MATERIALIZATION-ADDENDUM.md), defines
logical cells/work, retained roots, completion cutoffs, policy minima, startup,
and the fixed failure allowance. Those definitions include framework output
materialized while inspecting caller-retained inputs. Getters add no ledger event.

### 6.2 Planning path versus runtime

Successful pure operations return new budget usage `usedCells+L`, `usedWork+W`, `max(oldPeak,Q)`. The old immutable budget remains reusable: branching, discarded attempts, and retries are permitted. Failed attempts return only Left; no budget/report/witness is published and no successful cumulative usage is changed. Their bounded local computation is real but not a global retry account. D-082 must explicitly thread/add the path invocations it plans; nominal roles do not provide linearity or charge all retained branches automatically.

Runtime binding/pullback start from the session affine usage and update it under the existing MVar transaction. They do not import pure history or double-charge compilation. Existing operations do not consume the affine ledger and keep old scalar/payload report meanings. No global CPU/heap/linear-resource claim follows from either ledger or their combination.

### 6.3 Streaming header meter

Use the debit sequence, scan order, and prefix bounds in the
[materialization addendum, §2](D081-MATERIALIZATION-ADDENDUM.md). Debit before
every cons or nil inspection; a failed debit inspects neither the node nor its
element or tail. The local meter spans all argument scans. Runtime headers scan
the retained map witnesses; they do not demand the actual base or seed wrapper.

A length scan retains only the original head, cursor, and count. At the expected
count, a cons reports `expected + 1` without forcing its element or tail. All
list-length checks precede signed-value checks. Transform target/source rank
comparison follows the three shape scans and precedes list scans; disagreement
uses `AffineShapeDisagreement AffineTargetShape 0 expectedRank actualRank`, with
axis zero as the rank sentinel. Infinite defined spines reject by credit or
length; arbitrary caller thunks are outside the finite-work contract.

#### Count diagnostics

For **both** pure and runtime affine scans the precise public F64 machine-count rejection is:

```haskell
TensorShapeError (MachineIndexOverflow (fromIntegral (maxBound :: Int) + 1))
```

Equivalently `TensorShapeError (MachineIndexOverflow (M+1))`, with M+1 computed in Natural/Integer, never overflowing Int. This occurs at the **charged terminating SNil**, after all dimensions and zero handling, after the configured element checks below, when count>`M div 8`. It is not `TensorAffineError`, an unbounded raw product, an unspecified `machine` tag, or `MachineIndexOverflow count`. Accepted count and `8*count` are machine-compatible; oversized rejected byte products are not formed.

Let A be the affine element cap, and runtime E=`min(oldSessionElementCap,M)`. Pure nil checks A then machine; runtime nil checks **E first, A second, machine third**. On an element violation emit exactly:

- Pure or runtime affine cap: `TensorAffineError (AffineLimitExceeded AffineElements A (A+1))`.
- Runtime old/session cap: `TensorShapeError (ElementLimitExceeded E (E+1))`.
- Machine count/bytes (only after both applicable element caps pass): `TensorShapeError (MachineIndexOverflow (M+1))`.

These are **new-API** semantics only. Do not rewrite legacy `checkedShape`, `prepareDynamicShape`, capped-product errors or their existing numeric precedence. Runtime `oldCap>M` has effective E=M for this new path; diagnostics use E, not the huge oldCap. Closed/disabled/counter checks and header prefix credit retain their earlier precedence. Each dimension is still checked before accumulating its product, and dimension/rank errors can precede these nil diagnostics. Within a runtime cons, old rank → affine rank → old dimension → affine dimension remains the selected order. The new machine-count spelling adds no extra pass.

**Accumulator implementation is fixed, not left ambiguous:** in pure and runtime scans use product cap **M**, independently of A/E. Initialize product1 and zero=False. At each admitted nonzero dimension d, if zero remains False, return product sentinel M+1 when product>M or `product>M div d`; otherwise multiply. Dimension0 sets zero=True and product0; subsequent nonzero dimensions cannot unset zero, but their dimension checks still run. Thus the stored product is exact when≤M and otherwise exactly M+1 (with late zero overriding it). The invariant is `p=min(M+1, product of scanned dimensions)`; zero flag records the reason for zero and forbids division by zero. For scalar[], p=1 and the first charged nil performs the same checks.

Capping at `min(E,A)` is forbidden: it loses ordered diagnostic information. For runtime E5,A1, shape[10], the correct result is `TensorShapeError (ElementLimitExceeded 5 6)`. A min-cap accumulator would store2, incorrectly pass E5 and blame A1. CapM preserves10; if the product is larger than M, M+1 exceeds **both** possible caps, so the first applicable configured error is still known exactly. No unbounded multiplication is needed; even `[M,M,0]` stores M, then M+1, then0.

The [current failure fixtures](../evidence/D081-MATERIALIZATION/FAILURE-FIXTURES-FINAL.md)
cover machine counts, competing element caps, scalar zero limits, late zeros,
and credit exhaustion before the terminating nil. Coupons from earlier scans
remain part of each failure bound.

After all header checks, compute the full §8 plan using cached bounded ranks/counts only. Check cumulative **L, W, Q**, in that order. Header coupons are a subset of the complete plan, not added twice. On success the remaining local reservation is the full plan less already debited header work/cells; all subsequent loops are prepaid. There is no refund and no success report based merely on e. An early malformed header returns without needing the full semantic reservation.

### 6.4 Capped arithmetic

All successful formula values and counters must fit M. `satAdd(c,x,y)` returns c+1 if either exceeds c or `x>c-y`, else x+y. `satMul` first handles zero, then over-cap inputs or `x>c div y`, else product. Evaluate W,H,R,Q,L with cap **M**, not configured work cap, then apply each cumulative cap. This prevents an underestimated L from reversing cells-before-work priority. P=V(V-1)/2 uses V=0 guard and divides the even factor first. Formula overflow consequently rejects the cell reservation (L exceeds M); no oversized exact product or success report is built. Error fields are bounded sentinels, not huge rejected totals.

## 7. Failure bounds and error order

Use the complete failure bounds in the [materialization addendum, §5](D081-MATERIALIZATION-ADDENDUM.md)
and the [failure table](../evidence/D081-MATERIALIZATION/FAILURE-FIXTURES-FINAL.md).
The semantic order after admission is defined below.

### Ordered semantic suffix after admitted plan

All `AffineProblem` diagnostics use `TensorAffineError`. The exact machine-count and old effective session-shape cap errors instead use `TensorShapeError` as explicitly specified in §6.3. Existing runtime/session/payload error constructors retain their roles. No checked result is promised for bottom, caller exceptions, process OOM or asynchronous interruption.

1. Signed input checks in argument order and axis order: offset then strides; permutation entries; reverse scalar axis; starts then steps. Reject minBound even when empty/singleton. A later minBound therefore beats an earlier noncanonical-empty coefficient or zero step.
2. New map: raw empty canonicality first; then singleton normalization; nonempty checked extrema arithmetic → extrema bounds → per-address checks → lexicographic collisions → fully forced result.
3. Permute: ranges → duplicate pairs → target dimensions → descriptor derivation → empty/singleton canonicalization → full target validator → result.
4. Reverse: axis range → empty canonicalization or checked offset term/add/negation → singleton normalization → full target validator → result.
5. Slice: all zero steps → each axis's endpoint arithmetic/domain check → all offset derivation products/sums → stride products → canonicalization → full target validator → result.

`AffineAddressBounds lo hi B` reports checked extrema; redundant per-address failure uses that address for both bounds. `AffineOverlap i j a` reports first lexicographic pair. `AffineArithmeticOverflow field axis` identifies offset/stride derivation or extent axis; `AffineParentDomain axis low high sourceDimension` uses the checked endpoints or empty anchor for low/high. `AffinePhysicalBounds offset count capacity` is for nonnegative interval data; invalid negative/overflowing private signed metadata uses `AffineArithmeticOverflow AffineOffset 0` before any conversion. Safe public constructors cannot forge metadata, but private faults must have defined behavior on this new path.

## 8. Successful reservations

The [materialization addendum, §3](D081-MATERIALIZATION-ADDENDUM.md) defines all
successful `W`, `H`, `R`, `Q`, and `L` rows. The
[source proof](../evidence/D081-MATERIALIZATION/SOURCE-PROOF.md) gives the helper
ceilings, call sums, storage inventories, and coefficient comparisons.
Charge each full row exactly, including its header allowance, without refund.

### 8.4 Nested old reports

Bind: `TensorOperationReport "affine/bind" 0 (TensorMemoryReport 0 0 0 0)`.

Pullback: `TensorOperationReport "vjp/affine-base" (B+V) (TensorMemoryReport (8*B) (8*B) 0 1)`.

B zero writes and V cotangent copies give the old scalar field; W separately covers complete affine logical engine work. Scratch managed payload bytes0 does not mean no Haskell workspace. All existing operations and their rendered reports stay unchanged.

## 9. Runtime transaction, representation and exact ownership handoff

### 9.0 Committed registry and publication

Every committed session registry has a fully evaluated list **spine through its terminating `[]`**. This applies to all sessions created by that library, with affine policy enabled or disabled, and to every successful allocation commit through legacy APIs as well as affine pullback. No claim is made about a session created by an older compiled unit. A strict list field or strict state record is insufficient. No deep `ForeignPtr` forcing, finalizer execution, payload dereference, or arbitrary referent evaluation is required to establish this invariant.

For an **open** session, registry length equals `stateBuffers`, entries are in successful allocation order, and each committed pointer is represented once. At close, counters remain cumulative while registry becomes `[]`; do not assert the length equality on closed states. The detached old list remains fully spine-evaluated and becomes close-cleanup's responsibility. Rejecting a transaction preserves the previously established state/invariant. Affine entry does not normalize an old registry or charge another operation's deferred history.

Every allocating path uses the same protected preparation and publication
sequence, including legacy constructors, copies, primitive outputs, and
multi-output VJPs. Empty batches still check the session and budgets, share the
old registry, and allocate no buffer or ID. Private work-only commits preserve
their declared scalar charge. Preflight, observers, views, and the sharing
addition VJP introduce no registry traversal or new commit.

The transaction controls the actual state publication:

1. Enter an outer `mask`, acquire the MVar (interruptible until acquired), and establish an exception-safe restoration duty for its empty slot. Existing legacy pre-staging validation/old rejection computations may run under `restore`, before any pointer exists, with original precedence. New affine entry/header follows its existing §9.2 schedule. Any such failure restores old state.
2. After admission, enter the protected preparation phase **without restoring masking**. Install staging/cleanup ownership before the allocator can disclose a pointer; allocator owns undisclosed partial resources on failure. Each disclosed pointer is added to the staged set before initializer/metadata work. Existing per-allocation/init checked-failure/async behavior is preserved. An interruptible allocator can still deliver async exceptions under masking and must be caught by this staging protocol.
3. Helper-internal allocation/init failure cleans its own staged set using the existing order/protocol and does not transfer it. On success, while still masked, helper transfers a known staged pointer list to the enclosing preparation owner. Force its reversal/list spine while that owner can still enumerate all disclosed pointers for rollback; never discard the original staged ownership list while forcing a candidate representation. No unmasked return gap. For k=0 there are no pointers but state-restoration duty remains.
4. Under the same preparation owner, append a fully spine-known fresh list to the already-normal old list and force the **entire candidate spine**. Old pointers are shared references, not newly rollback-owned. Prepare/force the allocator's runtime-owned `(StorageId,ForeignPtr)` result spine, report/counter/state records and the return pair to the specified level before publication. Do not deep-force ForeignPtrs or arbitrary caller strings. For new affine, additionally force all §9.3 new affine tensor/layout/binding/result metadata here.
5. Legacy tensor wrappers made by `hostTensorFromList`, `makeFinite`, etc. **after** `allocatePayloads` returns remain post-commit computations as in the current API. This amendment does not silently change them to deep eager evaluation. The allocator's prepared result/report/state is protected; once committed, any later wrapper/action exception leaves registered buffers owned by session cleanup, not staged rollback. The stronger fully-forced affine result requirement remains local to new affine APIs.
6. Make preparation yield either a checked failure with old state or an entirely prepared candidate `(newState,result)`. All callbacks, full-spine forcing and other synchronous throw-capable work are inside the pre-commit cleanup handler. The actual `putMVar` into the transaction-owned **empty** slot is the single success linearization/ownership-transfer point. Execute it masked with already evaluated arguments; under the empty-slot invariant it is nonblocking. There is no restore, callback, additional forcing, or allocation between successful preparation and this put. Do not treat return from a `modifyMVar` callback as the commit point. The transaction abstraction must own/control the real put, not hand a still-staged value to an unmasked outer wrapper.
7. That put is outside the lexical staged-cleanup handler; after it, no such handler can finalize or restore the staged set/old state. Post-commit exceptions go to caller/runner, whose session registry now owns the new entries. A queued async exception delivered on unmask after the put may prevent the caller receiving a result, but does not roll back a completed commit. Registry-force/metadata/pre-put exceptions attempt staged cleanup and restore old state, even if cleanup reporting itself throws. Lock restoration must be in a guaranteed outer finally/handler; restore **never** runs after a successful put.

Existing synchronous allocator/initializer failures keep `HostAllocationFailure` with their current allocation index, primary and cleanup diagnostic composition. Unexpected synchronous post-initialization engine/forcing exceptions retain their ordinary exception channel after attempted staged cleanup (`throwAfterCleanup` conventions); do not invent a budget failure or new checked error constructor. This extends cleanup coverage, not a new public report/error meaning. Async exceptions retain existing rethrow/cleanup-interruption protocol. Callback retries can occur; no fixed invocation count or physical-release guarantee is introduced.

The registry invariant holds inductively: initial and closed registries are
empty; rejected and zero-buffer commits preserve the old spine; an allocating
commit forces the entire candidate spine before publication. No previous append
history remains deferred to a later affine operation.

For normal old/fresh spines of lengths `n` and `k`, append performs `n` copies
and one terminal reduction; the output walk inspects `n+k+1` nodes without
forcing pointer heads. Its work allowance is `24n+8k+16`. For affine `k=1`,
the fixed 24 units are included once in the current resource model. Workspace
and survivors, including every new registry copy, are accounted in the
[source proof](../evidence/D081-MATERIALIZATION/SOURCE-PROOF.md).
The [implementation record](../evidence/D081-AFFINE-IMPLEMENTATION.md) identifies
the mixed-history, batch, strictness, and publication fault controls.

### 9.1 Private representation

The current field declarations are in `Markovian.Tensor.Internal`.

Private fields are `limitAffine :: !(Maybe AffineLimits)` and `stateAffineUsage :: !AffineUsage`. Disabled startup is zero; enabled startup as §6. The public seven-argument `tensorSessionLimits` still constructs a disabled policy. Preserve nominal roles on layout/storage/region/owner/finite/owned wrappers. No public constructor changes.

Constructors/copies initialize count=capacity=logical shape product. Views retain original capacity/pointer/ID and update logical count/shape. `tensorElementCount` continues to mean logical count. `logicalOffsets` is an unchecked private observer of established layout invariants, not an admission helper; return checked nonnegative Int addresses for nonempty reads. New pullback does not call this list-producing helper.

Old empty layouts may contain huge irrelevant Natural dimensions/positive strides; converting private signed representation must not globally reject/canonicalize them or change legacy descriptions. Empty observers enumerate no addresses. New affine scanner applies its own machine/policy bounds before reading/converting relevant layout numbers. No irrelevant empty-stride arithmetic. `layoutDescription` preserves existing strings for nonnegative layouts and renders new negative integers normally.

New nonempty affine contiguity is canonical increasing row-major equality on extent>1 axes; singleton strides ignored. Empty/scalar are contiguous. Transpose continues to set False, even degenerate or twice-transposed cases. No implicit copy to pass binding.

`reshapeContiguous` preserves physical offset/capacity/ID/pointer, target count uses logical count, error precedence remains current: checked target-shape admission, conservative contiguity rejection, then target-count mismatch. Build target canonical strides without resetting the old offset; empty reshape preserves a valid anchor. It does not change the captured base/map of an existing binding or create a tracked map composition.

### 9.2 Runtime failure/admission order under one MVar

1. Acquire session transaction ownership; closed → `TensorSessionClosed`; absent policy → `AffineDisabled`.
2. Before variable scans, check old payload/buffer/scalar/next-ID counters <=M, and affine counters invariant. Pullback additionally requires buffer count and next storage number <=M-1. Failure → `AffineRuntimeCounterOverflow`. Bind does not require a spare ID. These are bounded comparisons; do not traverse registry. Legacy old limits are compared/capped, not copied into enormous arithmetic.
3. Run metered runtime header scans (§6), including all old/new shape checks. No payload or registry traversal.
4. Compute fresh plan from cached ranks/counts and n. Old payload preflight first: bind sizes[]/scalar0; pullback size8B, buffers1, scalarB+V. Check single, cumulative fresh, buffers, scalar work, in that order using effective `min(oldLimit,M)` caps and bounded limit+1 required sentinels. Then full affine L,W,Q admission, in that order. No seed read, allocation, or variable physical-descriptor inspection has happened.
5. After plan admission, bind checks conservative contiguity first, then base logical interval and shifted extrema/signed physical layout. Pullback validates stored base/view/seed logical counts/physical metadata, including noncontiguous seed's checked bounds. Original base type/map/view and seed shape relations come from nominal public types, not runtime string/value equality. Private metadata corruption gets bounded arithmetic/physical errors before dereference.
6. Bind constructs/forces view/binding/report/new state and publishes only affine counters; no payload read, new buffer, ID, old counter change, or registry copy.
7. Pullback proceeds to the ownership schedule below. Reservations are held by this transaction, not an earlier unlocked preflight. Final state only commits on successful publication.

Physical semantic checks follow full affine admission so variable descriptor inspection is prepaid. Existing operations retain their failure order.

### 9.3 Staging ownership at every throw-capable step

An enclosing masked rollback bracket spans staging through publication, as in
§9.0. Legacy callers' post-commit tensor wrappers retain their existing timing.
New affine result metadata must be forced before publication. The private
`allocateStagedInitializers` helper supplies staged buffers to that bracket.

No public callback; affine passes exactly one trusted zero/scatter initializer. Existing list staging can wrap it without changing old reports. The helper owns pointers until successful return, then the enclosing masked transaction immediately owns the returned staged list; that handoff is inside the caller's catch/rollback scope. Helper-internal failure cleans its owned list and returns/throws **without** transferring it; caller must not finalize that list a second time.

| Step / masking / possible throw | Resource owner and required recovery |
|---|---|
| Enter outer `mask`; interruptible MVar acquisition | Session owns old registry. If acquisition is interrupted, no staged pointer exists. Once taken, transaction owns restoration/publication duty for that empty lock. |
| Header, reservation, physical validation, bounded plan forcing | No fresh pointer. Checked failure returns original state; synchronous/async exception restores original state and propagates per existing protocol. |
| Allocator callback, under inherited masked staging discipline | Before returning a pointer, allocator owns any hidden partial allocation and must clean it if it throws/returns Left; runtime cannot finalize an undisclosed pointer. Once `Right pointer` is received, staging owns it immediately, **before** pointer/initializer/metadata forcing. |
| Zero loop, then scatter loop, inside protected `withForeignPtr` brackets | Staging owns fresh pointer; session still owns old/seed/base pointers. Any peek/poke/arithmetic/FFI/bracket exception attempts staged cleanup, no registry/ID/counter publication. Initialize B positive zeros first; only then read V seed scalars and write a(j). |
| Helper success / list result forcing | Outer caller was already masked with rollback handler installed around the helper return and continuation. Ownership transfers from helper to caller rollback set without an unmasked gap. A latent pointer/result spine exception is caught while caller owns the set. For this API the list has one entry; do not force a hidden arbitrary callback value. |
| Fresh canonical shape/layout/HostTensor/FiniteTensor/OwnedTensor, StorageId, affine/old reports, new counters/state, return tuple construction | **Caller rollback set still owns fresh pointer.** Force every new bounded numeric/list/state/result field here. Only share existing owner String/SShape/map references; do not execute arbitrary referents. Synchronous fault after initialization must trigger rollback just like initializer fault. |
| Registry append in original allocation order and full new-spine forcing | Caller owns fresh pointer; old registry remains owned by session and is not finalized on transaction failure. New candidate registry is not live session state yet. Exception discards candidate spine, finalizes only fresh staged pointer, restores old state/ID/counters. n traversal is charged. |
| Final publish | All throw-capable allocation/forcing is complete, return result and new state already evaluated to their required normal forms. While masked and holding the empty MVar, publish new state with a nonblocking put into that owned empty lock. **This put is the sole ownership-transfer/commit point.** It must be a primitive nonthrowing publication under the lock-ownership invariant; do not insert callbacks/forcing/allocations here. |
| After successful put | Session owns fresh pointer through committed registry. Caller rollback disarmed by lexical control flow: no catch whose handler finalizes staged pointers may cover post-commit continuation. Return the already prepared result. A later async exception/callback failure leads to session cleanup, not staging cleanup. |
| Rollback before put | Attempt finalization of exactly caller/helper-owned staged pointers using existing `finalizeAll`/`throwAfterCleanup` conventions; restore old state even if cleanup reports/fails/retains interruption. Use an enclosing restoration handler/finally so neither cleanup exception nor String-rendering exception strands the empty MVar. Never restore old state **after** successful put. |

The state MVar publication invariant, not a separately mutable ownership flag with its own throwing gap, separates rollback from committed cleanup. Masking alone is not a synchronous-exception handler. Any implementation using `modifyMVar` must preserve the same ownership boundary at its actual internal state put, rather than declaring success when a callback merely returns a lazy `(state,result)` pair. This is a required source-level review seam.

Allocation happens before seed reads; all zero writes precede scatter. No base read, seed list, destination list, addition, cast-changing copy, mutation of old payloads, or second managed scratch allocation. Empty destination still calls allocator length0 once, consumes one buffer and fresh ID, and registers one ForeignPtr; no pointer dereference. The allocator may physically allocate administration or share a zero-length address; StorageId, not pointer equality, establishes freshness.

Preserve existing synchronous `HostAllocationFailure`/cleanup diagnostic aggregation and async rethrow retention. Existing `finalizeAll` retries the same callback after AsyncException before proceeding; **no finite retry count**, unconditional exactly-one callback invocation, successful physical release, or bounded String cost is promised. Once a cleanup attempt has logically discharged a rollback set under the inherited protocol, do not add an independent second finalization owner/retry policy. Failing callback side effects cannot be undone.

### 9.4 Explicit cost exclusions

Finite W/L/Q covers the specified engine algorithm through admitted pointer calls, fixed diagnostic **records**, and publication. It excludes actual allocator/finalizer execution, external callbacks, system/GC/OS allocation, MVar wait time, arbitrary caller thunks/continuations, owner String contents, and exception `displayException`, `show`, concatenation, `unwords`, traversal/forcing of arbitrary callback Strings or accumulated async diagnostics. The runtime has unbounded String-producing paths and unbounded repeated async cleanup. Preserving that protocol is **not** a bound on complete failure handling, time, heap, physical reclamation, or exception rendering.

## 10. Session lifetime

Supported region-dependent observation, refinement, primitive execution and allocation must **execute and complete** inside the runner callback's live interval. Never invoke escaped region-dependent IO actions or observe existentially retained tensors after exit. The caller must join or cancel-and-join dependent children **before callback exit on success, Left, synchronous exception and asynchronous interruption**. The runner does not do those joins. A mere start within the callback, a returned pending action, or a happy-path-only join is insufficient. Returning copied ordinary data whose reads completed inside the callback is permitted.

Nominal/direct-index protection is not temporal/effect confinement. Positive source-import types include an escaped `IO [Double]` and an existential `Packed :: HostTensor region 'F64 '[] -> Packed`. Current `tensorToList` has no closed check; primitives may release snapshot preflight before reading. A single closed-bit check would leave a check/use race. This contract adds no observer token/lease, synchronization, error-channel change, or automatic child cleanup.

The public module and runner Haddocks document this supported-use discipline.
Finalization attempts and logical cleanup do not imply prompt physical
reclamation. Retaining default GC-pinned storage does not authorize later use.

## 11. Fixtures and failure competition

### 11.1 Signed base-5 map

Base `[5]`, view `[3]`, offset 4, stride -2 gives addresses `[4,2,0]`.
Seed `[10,20,30]` gives the full base gradient `[30,+0,20,+0,10]`.
Constructing base and seed uses 64 bytes, two buffers, and eight scalar units.
Pullback adds 40 bytes, one buffer, and eight scalar units: cumulative old
counters are 104 bytes, three buffers, and 16 scalar units.

Test the old single-allocation boundary 40/39 with a private preflight fixture:
a public session limited to 39 cannot construct the 40-byte base. Failed
preflight must leave allocator/seed-read counts, both ledgers, IDs, and registry
unchanged relative to the post-binding snapshot.

### 11.2 Resource fixtures

The [operative table](../evidence/D081-MATERIALIZATION/FIXTURES-OPERATIVE.md),
[inputs](../evidence/D081-MATERIALIZATION/FIXTURE-INPUTS.md), and
[failure table](../evidence/D081-MATERIALIZATION/FAILURE-FIXTURES-FINAL.md)
define current reservations and exact/one-below boundaries. Exact reservation
for malformed geometry reaches the semantic rejection; one-below rejects the
reservation first. Failed attempts commit no would-be totals.

### 11.3 Geometry/error discriminators

- Overlap logical addresses `[0,1,1,2]`: first equal pair(1,2), address1, `AffineOverlap 1 2 1`. One cell below the full reservation rejects before collision work.
- Negative range `[5]→[3]`,o0,[-1] gives extrema[-2,0], bounds error before pair work. o0,[2],count3 valid `[0,2,4]`; count4 fails hi6.
- o=M,stride1,count2: checked partial extent sum overflows before bounds; no Int wrapping. Any minBound input fails signed validation even if singleton/empty and would later be discarded.
- Raw empty `[5]→[0,1]`,o0,[0,7] fails `AffineEmptyDescriptor` at full reservation. o1,[0,minBound] fails `AffineSignedRange AffineStrides 1` first. All signed ranges are checked before raw canonicality. Offset minBound wins before strides. Length error/credit failure can precede all these.
- Parent base10 view3 o0 stride2 has image[0,2,4]. Target3,start1,step1 requests parent[1,2,3] and is rejected at endpoint3 although physical[2,4,6] fits B10. Target2,start1,step-1 is valid image[2,0]. For parent `[4,2,0]`, start0,count2,step-1 requests [0,-1] and fails domain.
- Reverse signed example gives[0,2,4]; reverse again restores[4,2,0] but incurs another transform reservation. Child pullback still has original base5, not parent view3.
- Empty slice count0,start=sourceDim,nonzero step is valid; start beyond it fails; count1 at that anchor fails. count1,start2,step-4 in source3 is valid singleton; zero step always fails, even globally empty.
- Empty base/view `[0]`: dimension/element caps0 may admit; pullback one length0 allocation, fresh ID/buffer, no dereference, nested work0. B0→scalar[] has V1 and fails bounds if scalar shape admitted; element cap0 rejects scalar earlier.
- Singleton o2,stride-7 normalizes to0, selects only address2; seed9 gives `[0,0,9,0,0]`.
- Offset reshape: `[11,22,33,44,55]`, target[2],o2,stride1 gives[33,44]; reshape[1,2] must retain[33,44], offset2, capacity5, same ID. Resetting offset gives wrong[11,22].
- Explicitly redeclare that selected contiguous length2 tensor as a **new logical base** at physical offset2/capacity5; logical map[1],o1 selects physical3. Pullback is length2 `[0,9]`, not length5. This is allowed rebinding/redeclaration, not composition that changes an existing binding's original base.
- Distinct bases with equal shape/values may bind the same pure map intentionally. Same storage may have different semantic owners; returned gradient uses the actual binding owner. Same-shaped finite cotangent seeds of independent origin remain valid.
- Failed old payload preflight competes before full affine reservation; failed full affine reservation competes before physical/noncontiguous semantics. Header credit can precede both. Existing operations are not changed to this new ordering.

### 11.4 Independent exact base-coordinate oracle

For x=[1,2,3,4,5], `f(x)=10*x[4]+20*x[2]+30*x[0]=140`, h=1/1024. The oracle uses explicit base indexing independently of the address kernel.

| Base i | f(x+h e_i) | f(x-h e_i) | Central difference |
|---|---|---|---:|
|0|71695/512|71665/512|30|
|1|140|140|0|
|2|35845/256|35835/256|20|
|3|140|140|0|
|4|71685/512|71675/512|10|

These dyadic values also fit exact F64 arithmetic for this fixed fixture.

Independent fixed matrix A has rows `[0,0,0,0,1]`, `[0,0,1,0,0]`, `[1,0,0,0,0]`; `A^T*[10,20,30]=[30,0,20,0,10]`. For z=[2,-1,3,4,-2], Az=[-2,3,2]; both pairings100. Full vector equality, all five base differences, and matrix/pairing are required; pairing alone is insufficient.

Additional independent quadratic `q(x)=x[4]^2+2*x[2]^2+3*x[0]^2` gives all-five-coordinate differences `[6,0,12,0,10]`; gathered seed `[10,12,6]` must yield that full vector. Runtime nonlinear checks retain existing host-F64 tolerance policy where arithmetic is not exact.

Fixed fixture rational scope: normalized inputs <=64-bit numerator/denominator, widened products<=128 bits, explicit 15 matrix entries and five central differences per objective. No unbounded Rational AD or rational runtime API is proposed.

## 12. Implementation audit and test requirements

| Seam | Required evidence at implementation review |
|---|---|
| All HostTensor constructors/patterns and tensor shape/dtype/layout/count/ID observers | Count/capacity split everywhere; constructors/copies equal, views retain capacity; existing public signatures/nominal roles unchanged. |
| Logical observers/refinement/transpose/reshape | Signed logical count enumeration, empty zero reads, scalar one, offset retention, conservative contiguity/count precedence. `tensorToList` ordinary allocation/lifetime limitations remain explicit. |
| Primitives/contiguousCopy/closed VJPs | Signed/sliced/offset inputs versus independent materialized logical references; rectangular signed matrix and noncontiguous seed; all old report goldens unchanged; addition sharing unchanged. No tracked map relation inferred from exposed transpose/reshape. |
| SafeTensors encode/decode/raw batch | Encode logical V and 8V, not backing C/prefix; signed/offset/empty round trips become contiguous logical tensors; decode count=capacity; owner/map not serialized; atomic multi-buffer rollback/wire format preserved. |
| New `test/AffineContractTests.hs` | Full constants/prefix sentinels, zero/tiny policy, malformed/full-plan competition, exact/one-below, exact R2 machine/competing-cap wrappers, M edges and independent fixtures. Mixed1024 legacy-empty-constructor/first-affine-pullback fixture with **no private registry observation** and mutation-sensitive execution counters as §9.0. |
| `test-fault/Main.hs` | No reads/allocation on each failed admission; snapshot counters/ID/registry; first allocation/zero/scatter failure; **post-initialization metadata forcing and registry forcing failure before commit**; successful transfer followed by action exception finalized only by session. Cleanup String exception cannot strand lock. Fault hooks private. |
| Existing shared staging tests | Rerun first/second allocation, multi-output rollback, init, registry forcing/preput and afterput, cleanup failure/async retry, all session exits. Include reachable empty-batch/work-only commits and universal spine-invariant mutation. Affine still has one destination. Physical release not guaranteed when callback fails. |
| Public opacity/indices | Reject all three map-index and all five binding-index coercions, constructor access, wrong bind/seed shapes, direct fixed-region escape, and separate continuation-tag misuse. |
| Positive allowed freedoms | Explicit compatible map rebinding, independent same-shaped seeds, semantic owners sharing storage, existential/closure typeability consistent with temporal precondition. Never execute default post-finalization reads as a lifetime test. |
| Freshness/zeros | New StorageId differs from base/view/seed and prior pullbacks, including empty destination; omitted positive zeros, selected negative zero; pointer equality not a test. |
| Installed unit and archive consumer | Modern explicit intended package unit, isolated package environment, positive import of Affine, individually identified negatives; source imports alone do not prove package exposure. Cabal/snapshot equality and tensor/SafeTensors affected archive closure checks; direct sdist not release admission. |
| Documentation/package scope | Exact §10 lifetime qualifications; F3 mutable snapshot only; no versions/dependency graph/released evidence/status/tape expansion. Historical transpose claims remain bounded. |
