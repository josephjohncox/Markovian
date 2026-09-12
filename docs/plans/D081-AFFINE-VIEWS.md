# D-081 affine-view contract

D-081 is Accepted within its [unreleased host-F64 scope](../evidence/D081-AFFINE-IMPLEMENTATION.md).
The [materialization addendum](D081-MATERIALIZATION-ADDENDUM.md) supersedes the
resource, producer, failure, and fixture requirements in §§6–9 and 11 where
specified. Read those documents together; the original resource coefficients
below are retained to explain the derivation's revisions.

The numbered contract describes geometry, signatures, nominal roles, error
precedence, ownership, and validation requirements. Source line references use
the design baseline `78669c3613302249c499eba99a959f75c4c59edc`. Temporary model
and review artifacts cited in the original contract are historical evidence;
they are not distributed tests. Current checks are listed in the implementation
record linked above.

## 1. Exhaustive correction manifest

### Review960aa9b5 / parent-selected R1–R3

| Finding | Concrete correction | Evidence / prospective gate |
|---|---|---|
| R1: inherited lazy registry history violates linear coupon | **Every** future allocation commit, legacy and affine, establishes a fully forced registry spine before publication; initial/closed registry[], rejection/no-allocation updates preserve it. No entry normalization. | §9.0 full source/publication/masking trace, induction, strictness-sensitive native Haskell -O0/-O2 and independent memoized history evaluator. |
| R1: source-owned deferred tails are not caller thunks | Reproduce WHNF-only history with522753 pending copies at n1024; include batches and zero-buffer/no-op history. Actual-affine copy demand of mutant523777 exceeds whole W71168. | `RegistryHistory.hs`, `models.py`, logs; synthetic execution, not real legacy runtime. |
| R1:24n requires prerequisites/terminal budget | Derive24n+8k+16 from evaluated append/output-spine events. Affine k1 constant24 is drawn from fixed1536, leaving1512; all prior successful constants fit unchanged. | §9.0 itemized proof, 720 event-fed full-coupon cases, mixed fixture totals631184/76544/17560. |
| R1: extend ownership to actual publication on old paths | Common protected staged interval through registry force and allocator result/state preparation, no unprotected evaluate after allocateStaged. Actual empty-MVar put is linearization. Old wrappers after commit remain session-owned. | Source trace plus future init/registry/preput/afterput/cleanup fault schedule, symbolic token model. |
| R2: exact machine error and cap competition | Charged nil emits `TensorShapeError (MachineIndexOverflow (M+1))`, after old effective E then affine A (pure only A). Product capped at M, never min(E,A), preserves ordered error information. | §6.3 exact constructors/literals; `CountDiagnostics.hs`,18688 exact oracle comparisons and boundary/credit examples. |
| R3: opaque type count | Exactly **four**: AffineLimits, AffineBudget, AffineMap, OwnedAffineView. No added type/API. | §4 declarations and fresh declaration-only typechecks. |

The following F1–F4 matrix and seven selected amendments remain part of this full contract; R1/R2 complete the additional prospective obligations rather than changing lifetime/geometry/release scope.


| Finding/subissue | Resolution in this contract | Discriminator/evidence | Disposition |
|---|---|---|---|
| F1.1 unnamed pre-plan envelope | §6 specifies fixed startup/terminal envelope plus metered prefix coupons; §7 gives numerical bounds for every exit class. | Zero remaining credit forces zero input nodes; two coupons force at most two nodes, regardless of rank allowance. | Replaced, not deferred. |
| F1.2 variable scan before credit | Debit one local meter before **each** shape/list spine inspection, including nil/rank/length sentinels. No full-rank free allowance. | 168 prefix/credit cases; poisoned next-prefix supplier; fully defined 100,000-node tail with only two work coupons. | New-API failure order explicitly revised. |
| F1.3 checkedShape copying | New APIs do not call `checkedShape`, `boundedDimensions`, `shapeDimensions`, `shapeRank`, or `shapeElements` as pre-plan helpers. Bounded no-copy singleton traversal; full metadata construction only after reservation. | Actual `Internal.hs:398–435` copies dimensions and has separate zero/product passes. §6 defines compatibility and differences. | No uncharged reuse. |
| F1.4 cells/microsteps/forcing | §8 itemizes scalar/field/list/frame/coordinate/descriptor/report/state/result/registry coupons; success constants increased. | 12,000 finite coupon-envelope cases plus coefficientwise inequalities. | Logical model only, not generated-code proof. |
| F1.5 old constants and fixtures | Startup becomes `(2432,256,384)`; all successful W/H/R/L/Q and dependent boundaries replaced. | §11 full traces and fixed exact/one-below literals. | Old numbers are not current expectations. |
| F1.6 tiny/invalid policies and failure competition | Six negative checks, then explicit startup minima; constant policy failure envelope; streaming errors and reservation ordering specified. | Startup-only, malformed rank/dimension/scalar, late zero, length sentinel, raw-empty/work and overlap/cells competition. | No global failed-attempt ledger claimed. |
| F1.7 machine arithmetic | Cap at M before configured comparisons; divide even pair factor first; bounded operands and rejected sentinels. | Small caps and 64-bit edge truth tables. | No arbitrary large rejected product. |
| F1.8 callbacks/Strings | Exclude allocator, finalizer, repeated async cleanup, arbitrary thunk execution, exception rendering and String assembly from finite engine-work/space claims. | Actual `allocationFailure`, `finalizeAll`, `displayException`, `unwords`, `(++)` source. | No numeric callback-diagnostic bound. |
| F2.1 lifetime selection | Parent-selected A: complete region-dependent work inside callback, join or cancel-and-join children on **all** exits. | §10 exact future public prose and source/probe attribution. | No observer/liveness repair. |
| F2.2 role versus effect confinement | Direct nominal/index rejection remains; existential tensors and IO closures typecheck. | Fresh modern declaration-only checks of existing public lifetime types; no escaped default read executed. | No general escape-prevention theorem. |
| F2.3 GC/finalization distinction | Default pinned/NoFinalizers retention is source inference, not supported post-finalization use; no current-default freed-buffer/UAF established. | Compiler-local evidence cited in §10; retained-live-payload probes are inherited and explicitly limited. | No universal closed rejection, race safety, prompt reclamation, or runner child joins promised. |
| F2.4 public claims | Exact replacement prose scheduled for Internal Haddock, public module Haddock, README, architecture, and exception diagnostic wording. | §10 locations and replacements; historical/tagged evidence untouched. | Future documentation edits only. |
| F3 module snapshot | Add `Markovian.Tensor.Affine` after umbrella module in Cabal **and** current-development snapshot. | `release_tool.py:1152–1165`, `release/README.md:3–7`. | No package/version/dependency/released-membership change. |
| F4 raw-empty order | Header admission → full reservation → all signed range checks → raw initial-empty canonicality → singleton normalization. | Base `[5]`, view `[0,1]`, offset0, `[0,7]` fails `AffineEmptyDescriptor`; later minBound wins over raw-empty if credit permits. | Strict parent choice implemented in proposal. |

### Seven amendments, with no silent alternatives

1. Pure opaque nominal shape maps and reusable, explicitly threaded planning budgets retained. Atomic session affine accounting is separate. No IO planner account, linearity or global quota.
2. Signed private layout arithmetic **and** logical-count/storage-capacity split selected together. Legacy empty layouts retain compatibility.
3. Reshape preserves offset/capacity/pointer/ID; preserve actual current order: target-shape admission, conservative contiguity rejection, then target-count mismatch.
4. Existing seven-argument limits constructor remains; optional validated affine policy is additive and default-disabled; diagnostics/counters/reports are additive; F3 snapshot scheduled.
5. Direct staged zero/scatter selected; §9 extends rollback ownership through metadata forcing, registry construction, and actual state publication. Additional explicit R1 private amendment: **all** allocation commits, including legacy APIs, establish the fully forced registry-spine invariant in the same protected publication interval. No new old-report/counter meaning, uncharged entry normalization, destination list, or initializer-only gap.
6. Raw canonical initial empty input, subsequent singleton normalization, derived-empty canonicalization after domain checks; one fresh zero-length managed buffer and storage ID retained.
7. Supported-use temporal discipline A selected, including all-exit child joins; future public claims qualified. No hidden runtime liveness patch.

**Remaining parent decisions:** accept or revise this concrete revised API/accounting/error/staging contract after independent review, and decide whether to freeze it. There is no unresolved choose-A/B scope branch in this draft. D-083 placement, D-082 implementation, enforced liveness, new releases, and arbitrary map composition are not approved here.

## 2. Source basis and scope

Source anchors below refer to unchanged runtime code at the inspected HEAD, not nonexistent affine implementation:

- `packages/markovian-tensor/src/Markovian/Tensor/Internal.hs:43–76`: seven limits, cumulative session state, allocator capability, nominal session.
- `Internal.hs:99–204`: runner, default allocator, close, masked finalization/retry, exception and String diagnostics.
- `Internal.hs:206–242`: logical payload/scalar reports, not RSS/heap/time.
- `Internal.hs:247–344`: existing errors, Natural layout offset/strides, StorageId, single-count HostTensor.
- `Internal.hs:398–435`: `checkedShape` constructs a dimension list before its dimension/product passes. Rank-zero element-limit-zero is not checked by its empty-list product base case. New APIs must correct their own scalar admission without collateral old-API changes.
- `Internal.hs:448–555`: preflight is a snapshot, not a reservation; allocation rechecks under MVar; list-based staging currently protects allocation/initialization, with later result/registry construction outside that staging helper.
- `Internal.hs:602–698`: constructors, logical enumeration and observers. `tensorToList` reads via `withForeignPtr` with no session lock/closed check.
- `Internal.hs:703–731`: transpose retains offset and marks False; reshape currently resets offset. `:736–763`: semantic ownership, not unique authenticated owner IDs.
- `Internal.hs:768–1078`: primitive gathering and tapes. Addition's VJP intentionally shares its seed (`:981–984`). No tape alternative is extended by this proposal.
- `Markovian/Tensor/Shape.hs:25–71`: singleton GADT, nominal shape, list-producing `shapeDimensions`, unchecked `shapeRank`/`shapeElements` helpers.
- `packages/markovian-safetensors/src/Markovian/Tensor/SafeTensors.hs:117–126,469–482,512–578`: dynamic observation, atomic decode constructor, logical values/dimensions in encoding.
- `packages/markovian-tensor/test-fault/Main.hs:34–245`: existing fault protocol evidence to preserve and eventually rerun, not a new test run here.
- `packages/markovian-tensor/markovian-tensor.cabal:46–100`, `release/exposed-modules/markovian-tensor.txt`, `scripts/release_tool.py:1152–1165`: public/private/module-snapshot seams.
- `docs/evidence/D081-TRANSPOSE-REVERSE-EQUIVALENCE.md` and `test/TensorLawLaboratory.hs:40–69` are historical, bounded transpose/logical-coordinate evidence, not evidence for signed views or this new API.

Index/LSP extension tools were unavailable in the allowed tool set. Supplied exact anchors and bounded reads were used; no index-ready or LSP-success assertion is made. No installer, global selector, HLS, device, runner, GitHub, release, stage, commit, or delegation action occurred.

## 3. Meaning, ownership and allowed reuse

For declared logical base count B and view dimensions d with count V and rank r:

`a(c) = o + sum_i c_i*s_i`, with `0 <= c_i < d_i`; `y[c] = x[a(c)]`.

Thus `a` maps view coordinates to flat **logical base** coordinates; primal gather goes base-to-view. A nonempty initial map accepts any bounded signed flat-affine descriptor with in-range addresses and injectivity. It need not be a rectangular slice of base dimensions. There is no overlap, broadcasting, mutation, dtype conversion, device lowering, arbitrary affine composition, or copy fallback.

The pure witness contains no tensor, owner, region, pointer or StorageId. Compilation can precede construction of the base. It constructs bounded Haskell metadata, not tensor payload; it is not allocation-free.

Binding explicitly supplies `OwnedTensor region owner 'F64 base`. Its layout must have the conservative increasing-row-major contiguity bit and a valid logical interval in actual backing storage. For physical offset b and capacity C, a nonempty base requires `0 <= b`, `b+B <= C`; an empty base permits `0 <= b <= C`. Physical view addresses are `b+a(c)`. Binding retains the **actual supplied** original base, actual owner witness, map, and shared finite-view wrapper. It does not infer identity from shape, values, owner strings, or storage equality.

Maps can deliberately be reused/rebound to different explicitly supplied compatible contiguous bases. A map from a different planning budget is also usable; current work is charged without importing its history. Equal-shaped valid cotangent seeds are intentionally accepted even if their storage/values/semantic origin differ from the primal. Seeds do not certify base identity or map identity. The binding, not the seed, fixes the pullback's original base and map.

`TensorOwner` is semantic evidence; two explicit keys can inhabit the same phantom owner type. Nominal roles forbid representational index changes, not deliberate use of the existing public owner constructor. Pullback returns the stored binding owner and a fresh **logical base-shaped** tensor, not a view-shaped tensor or capacity-sized allocation. No public operation accepts a substitute owner key or map at pullback time.

For finite seed λ and injective a, output is λ[c] at k=a(c), otherwise positive `0.0`. Direct assignment, not addition, is correct. Selected negative zero is preserved. Base payload is never read by bind or pullback.

## 4. Complete proposed public declarations and module schedule

All declarations below are provisional new API, except imported existing types. Opaque constructors are not exported; do not derive representation-recovering `Read`, `Generic`, `Data`, or public witness serialization. No public initializer callbacks, raw pointers, unsafe constructors or tag-eliminating casts.

`Markovian.Tensor.Affine` exports exactly the opaque names, functions, and diagnostic/report constructors shown below (no opaque constructors):

```haskell
-- GHC2021; DataKinds, GADTs, RankNTypes, RoleAnnotations, KindSignatures
import Data.Kind (Type)
import GHC.TypeLits (Nat)
import Numeric.Natural (Natural)
import Markovian.Tensor
  ( SShape, SessionLimits, TensorSession, TensorError
  , DType(F64), FiniteTensor, TensorOperationReport )
import Markovian.Tensor.Ownership (OwnedTensor)

data AffineLimits
data AffineBudget
data AffineMap (map :: Type) (base :: [Nat]) (view :: [Nat])
type role AffineMap nominal nominal nominal
data OwnedAffineView (region :: Type) (owner :: Type) (map :: Type)
                     (base :: [Nat]) (view :: [Nat])
type role OwnedAffineView nominal nominal nominal nominal nominal

data AffineLimit
  = AffineRank | AffineDimension | AffineElements
  | AffineConstructedCells | AffineWork | AffineLiveCells
  deriving (Eq, Show)
data AffineInput
  = AffineBaseShape | AffineViewShape | AffineTargetShape
  | AffineOffset | AffineStrides | AffinePermutation
  | AffineAxis | AffineStarts | AffineSteps
  deriving (Eq, Show)
data AffineProblem
  = AffineDisabled
  | AffineInvalidLimit !AffineLimit !Int
  | AffineLimitTooSmall !AffineLimit !Natural !Natural
  | AffineLimitExceeded !AffineLimit !Natural !Natural
  | AffineListLength !AffineInput !Natural !Natural
  | AffineSignedRange !AffineInput !Natural
  | AffineArithmeticOverflow !AffineInput !Natural
  | AffineShapeDisagreement !AffineInput !Natural !Natural !Natural
  | AffineAxisOutOfRange !Int !Natural
  | AffineDuplicateAxis !Natural !Natural
  | AffineZeroStep !Natural
  | AffineParentDomain !Natural !Integer !Integer !Natural
  | AffineEmptyDescriptor
  | AffineAddressBounds !Integer !Integer !Natural
  | AffineOverlap !Natural !Natural !Natural
  | AffineNonContiguousBase
  | AffinePhysicalBounds !Natural !Natural !Natural
  | AffineRuntimeCounterOverflow
  deriving (Eq, Show)
-- Add to the existing Internal-defined TensorError:
-- | TensorAffineError !AffineProblem

data AffineUsage = AffineUsage
  { affineUsedCells :: !Natural
  , affineUsedWork :: !Natural
  , affineHighWaterCells :: !Natural
  } deriving (Eq, Show)
data AffineCharge = AffineCharge
  { affineConstructedCells :: !Natural
  , affineWork :: !Natural
  , affineLiveCells :: !Natural
  , affineRetainedCells :: !Natural
  } deriving (Eq, Show)
data AffineMapReport = AffineMapReport
  { affineMapCharge :: !AffineCharge
  , affineMapCumulative :: !AffineUsage
  } deriving (Eq, Show)
data AffineOperationReport = AffineOperationReport
  { affineOperationCharge :: !AffineCharge
  , affineOperationCumulative :: !AffineUsage
  , affineTensorReport :: !TensorOperationReport
  } deriving (Eq, Show)

-- rank, dimension, elements, cumulative cells, cumulative work, additional live
-- All six are nonnegative Ints; accounting minima are specified in section 6.
affineLimits :: Int -> Int -> Int -> Int -> Int -> Int
             -> Either TensorError AffineLimits
tensorSessionLimitsWithAffine :: SessionLimits -> AffineLimits -> SessionLimits
affineBudget :: AffineLimits -> Either TensorError AffineBudget
affineBudgetUsage :: AffineBudget -> AffineUsage
withAffineMap
  :: AffineBudget -> SShape base -> SShape view -> Int -> [Int]
  -> (forall map. AffineMap map base view -> AffineBudget
                  -> AffineMapReport -> value)
  -> Either TensorError value
permuteAffineMap
  :: AffineBudget -> AffineMap parent base source -> SShape target -> [Int]
  -> (forall child. AffineMap child base target -> AffineBudget
                    -> AffineMapReport -> value)
  -> Either TensorError value
reverseAffineMap
  :: AffineBudget -> AffineMap parent base shape -> Int
  -> (forall child. AffineMap child base shape -> AffineBudget
                    -> AffineMapReport -> value)
  -> Either TensorError value
sliceAffineMap
  :: AffineBudget -> AffineMap parent base source -> SShape target
  -> [Int] -> [Int]
  -> (forall child. AffineMap child base target -> AffineBudget
                    -> AffineMapReport -> value)
  -> Either TensorError value
bindAffineView
  :: TensorSession region -> AffineMap map base view
  -> OwnedTensor region owner 'F64 base
  -> IO (Either TensorError
       (OwnedAffineView region owner map base view, AffineOperationReport))
affineViewTensor
  :: OwnedAffineView region owner map base view -> FiniteTensor region 'F64 view
affineViewBase
  :: OwnedAffineView region owner map base view -> OwnedTensor region owner 'F64 base
affineViewMap
  :: OwnedAffineView region owner map base view -> AffineMap map base view
pullbackAffineView
  :: TensorSession region -> OwnedAffineView region owner map base view
  -> FiniteTensor region 'F64 view
  -> IO (Either TensorError
       (OwnedTensor region owner 'F64 base, AffineOperationReport))
```

`AffineUsage(..)`, `AffineCharge(..)`, `AffineMapReport(..)`, `AffineOperationReport(..)`, `AffineLimit(..)`, `AffineInput(..)`, `AffineProblem(..)` are public. All listed functions and only the four opaque type names (AffineLimits, AffineBudget, AffineMap, OwnedAffineView) are also exported. Reports constructed by callers are not witnesses or admission evidence; there is no arbitrary-charge commit API.

Each rank-2 continuation receives a fresh abstract nominal tag. Before calling it, fully force the admitted shape/descriptor spines and numeric facts, usage and report. A strict list field alone is insufficient. Caller continuation computation is outside engine cost: producing `Right value` must not require forcing arbitrary `value`. An exception/divergence in the continuation is not a checked validation failure. Existential map packaging and budget branching are allowed.

### Eventual source/package changes, not performed here

- Definitions stay in existing `Markovian.Tensor.Internal`, which owns allocator/session/owner/tensor constructors. Thin public `Markovian.Tensor.Affine` reexports; no unnecessary private-module cycle.
- Umbrella `Markovian.Tensor` adds `TensorAffineError` through its existing `TensorError(..)` export and exports `AffineProblem(..)`, `AffineLimit(..)`, `AffineInput(..)` so diagnostics remain nameable. Do not reexport all affine operations there.
- Cabal and `release/exposed-modules/markovian-tensor.txt` both become, in order: `Markovian.Tensor`, `Markovian.Tensor.Affine`, `Markovian.Tensor.Ownership`, `Markovian.Tensor.Primitive`, `Markovian.Tensor.Reverse`, `Markovian.Tensor.Shape`.
- The snapshot is mutable **current-development** membership; immutable published membership remains the published source revision and pinned released evidence. Preserve all 16 versions `2026.9.3.0`, `release/published-releases.json`, `docs/capabilities/released-modules.json` and its digest, actual release assets/tags/history. A scratch sdist at this version is not publication or a new admitted release candidate.
- No package/dependency edge/flag change, no D-083 placement decision, no new tape alternative or D-082 adapter implementation.

## 5. Arithmetic and precisely admitted transformations

Let `b = finiteBitSize (0 :: Int)` and `M = maxBound :: Int = 2^(b-1)-1`. Fresh evidence here is b=64, GHC9.14.1/base4.22.0.0. Correctness is parameterized by b, not silently hard-coded to 64.

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

### 6.1 Units, fixed costs, startup

A **cell** is a bounded scalar/reference/control slot in the logical algorithm; a list cons plus element slot counts two, a widened temporary two. A work unit is one logical primitive (field/spine inspection, bounded scalar comparison/arithmetic, slot assignment, branch/control transition, pointer peek/poke). It is not one CPU instruction or GHC allocation. Aggregate constructors/traversals are expanded into these primitives; no library traversal is one unit.

Successful charge uses `Q=H+R`, `L=8W+H+R`, and report `(L,W,Q,R)`. H is additional workspace; R newly retained slots, including new result/report/state references and copied registry spine; Q includes **both**, not H alone. Pre-existing input objects, retained older maps/bindings, old registry and managed payload are excluded from additional live cells; copies and newly retained references are charged. Holding a reference does not recursively charge the entire referent.

**Fixed per-attempt control envelope F:** at most W=256, constructed L=2048, additional live Q=256 on failure (including the returned bounded diagnostic record). This covers entry, constant-size meter registers, a failing debit, bounded formula evaluation/diagnostic record/return, and startup/terminal control together. It is not 256 per node or phase. Every variable iteration, including its meter-update overhead, uses coupons below. Success reservations include this fixed work in their constant term; it is not added again to successful reports. Failed attempts may spend F even with zero remaining path credit; this is the only independent allowance. F never depends on rank, list length, B, V, n, maxRank, retries, callback strings or retained child computations.

For `affineLimits`, inspect six Ints left-to-right, reject negatives first (`AffineInvalidLimit field value`), then minimum constructed cells **2432**, work **256**, live **384**, in that order (`AffineLimitTooSmall field minimum supplied`). Rank/dimension/elements may be zero. Safe policies cannot have zero/tiny accounting fields. All malformed-policy exits fit F without input traversal. A bottom argument is ordinary partiality.

`affineBudget` starts `(cells,work,peak)=(2432,256,384)` from `W0=256,H0=352,R0=32,L0=2432,Q0=384`. The opaque validated policy makes its `Either` failure channel unreachable for safe values. Enabled runtime account has the same startup reservation; disabled account has zero affine usage. The initialization reservation covers normalization/retention even if a policy is reused; it is not a measured allocation count.

`tensorSessionLimitsWithAffine` installs a validated optional policy, preserving the existing seven limits/signature. Fixed setter/getter/accessor work is bounded by F; getters share existing records and do no payload read or list rendering. Repeated calls are not a global quota. `affineBudgetUsage` does not create a new ledger event.

### 6.2 Planning path versus runtime

Successful pure operations return new budget usage `usedCells+L`, `usedWork+W`, `max(oldPeak,Q)`. The old immutable budget remains reusable: branching, discarded attempts, and retries are permitted. Failed attempts return only Left; no budget/report/witness is published and no successful cumulative usage is changed. Their bounded local computation is real but not a global retry account. D-082 must explicitly thread/add the path invocations it plans; nominal roles do not provide linearity or charge all retained branches automatically.

Runtime binding/pullback start from the session affine usage and update it under the existing MVar transaction. They do not import pure history or double-charge compilation. Existing operations do not consume the affine ledger and keep old scalar/payload report meanings. No global CPU/heap/linear-resource claim follows from either ledger or their combination.

### 6.3 Streaming header meter

At attempt entry retain fixed references to policy, input heads, and old usage. Let e=0. Before inspecting **any next** variable-size spine node, including nil:

1. Tentatively require `oldCells+1024*(e+1) <= cellLimit`.
2. Then require `oldWork+128*(e+1) <= workLimit`.
3. Then require `max(oldPeak,384) <= liveLimit`.
4. On success increment e and only then pattern-match the next spine. The 128-work/1024-cell coupon covers that iteration's successful debit/update, node access, local arithmetic and branch control; maximum extra variable scanner workspace is 128 slots, reused, not e*128 live slots.

Use capped incremental counters, not the displayed multiplication in code. Failure at steps 1–3 yields the corresponding `AffineLimitExceeded`; required is saturated `limit+1`, not actual unbounded requirement. The failed debit performs **no** inspection. No list/shape materialization, result descriptor, coordinate frame or registry traversal occurs in this phase. Input head references already supplied are fixed-size; accessing their next constructor is metered.

Shape scan pseudocode (one coupon per loop, including nil):

```
seen=0; product=1; zero=False; productCap=M
repeat:
  debit-prefix-coupon-or-return-limit-error()
  inspect next SShape constructor
  SNil: count = 0 if zero else product
        check old effective element cap if runtime, then affine element cap,
        then F64 machine cap; emit the exact wrappers below or return rank/count
  SCons proxy rest:
        check rank sentinel before reading proxy's natVal
        check dimension cap before converted copy/product
        seen += 1
        zero |= dimension == 0
        if zero: product = 0
        else: product = satMul(M, product, dimension) -- retains M+1 until nil/late zero
        continue with rest
```

Defer saturated element-product failure to nil so a later zero gives count0; still check **every dimension**. Do not defer an earlier dimension error to discover a later rank error. This streaming precedence is an intentional **new-API-only** change. Scalar SNil checks count1 against element cap0. There is no `checkedShape`/dimension-list copy hiding before a meter.

Length scan retains only original head/cursor/count: debit, inspect nil/cons; nil fails short or succeeds exact; cons at expected count fails long with observed=expected+1 without forcing its element/tail; otherwise increment. Do not force signed values yet. All argument-list lengths precede signed semantics. An infinite fully defined spine rejects by credit or expected+1, whichever comes first; no promise about bottom/divergent caller thunks.

Scan order and completed-success coupon counts:

| Operation | Sequential shape/list scans | e on complete valid header |
|---|---|---:|
| New map | base, view, strides | `rB+2*rV+3` |
| Permute | base, source, target, permutation | `rB+2*rS+rV+4` |
| Reverse | base, source, target (=source, deliberately rescanned) | `rB+rS+rV+3` |
| Slice | base, source, target, starts, steps | `rB+3*rS+rV+5` |
| Bind | base, map view | `rB+rV+2` |
| Pullback | bound base, map view, seed view | `rB+2*rV+3` |

Transforms recheck source/base shape facts against the supplied policy rather than trusting a former budget. Target/source rank equality is checked immediately after the three shape scans and before list scans. Rank disagreement uses `AffineShapeDisagreement AffineTargetShape 0 expectedRank actualRank`; axis0 is explicitly the rank sentinel in this diagnostic.

Runtime scans enforce old session and affine shape policy together: at each cons, old rank check then affine rank, old dimension then affine dimension; at nil old element then affine element, then machine count/bytes. Old caps are effectively `min(oldCap,M)` for the new APIs. Diagnostics identify the effective cap; old operations and their error payloads are unchanged. This deliberately replaces the draft's uncharged calls to `checkedShape` and its whole-pass old/new order. For inputs valid under both policies the accepted shapes/counts are the same, except the new scalar count0 rejection is correctly enforced. A new rank/dimension/work error may precede a later old-policy error; this is documented, not an alleged preservation of old ordering for new APIs.

#### R2: exact count diagnostics and ordered bounded product state

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

Capping at `min(E,A)` is forbidden: it loses ordered diagnostic information. For runtime E5,A1, shape[10], the correct result is `TensorShapeError (ElementLimitExceeded 5 6)`. A min-cap accumulator would store2, incorrectly pass E5 and blame A1. CapM preserves10; if the product is larger than M, M+1 exceeds **both** possible caps, so the first applicable configured error is still known exactly. No unbounded multiplication is needed; even `[M,M,0]` stores M, then M+1, then0. All operations fit the existing128-work prefix coupon, using the capped-product/zero-state24 and scalar compare/control slots; the extra runtime cap comparison is in the named comparisons/terminal control allowance. Header numbers/scan counts and success formulas are unchanged.

For b64: M=9223372036854775807; T=`M div 8+1`=1152921504606846976. Default rank/dimension policies below admit the fully defined shape; remaining credit admits all specified nodes. Exact fixtures (`Left` wrappers shown, shape is the first scan):

| Input / ordered caps | Nil result | Prefix coupons e / failure bound `(W_header,L_header,Q_header)` |
|---|---|---|
| Pure [T], A=M | `Left (TensorShapeError (MachineIndexOverflow 9223372036854775808))` | e2 / `(512,4096,384)` |
| Runtime [T], E=M,A=M; also oldCap=M+100 | Same exact machine result | e2 / same |
| Runtime [T], E=T-1,A=M | `Left (TensorShapeError (ElementLimitExceeded 1152921504606846975 1152921504606846976))` | e2 / same |
| Runtime [T], E=M,A=T-1; pure [T], A=T-1 | `Left (TensorAffineError (AffineLimitExceeded AffineElements 1152921504606846975 1152921504606846976))` | e2 / same |
| Runtime [T], E=T-2,A=T-3 (both fail) | `Left (TensorShapeError (ElementLimitExceeded 1152921504606846974 1152921504606846975))` | e2 / same; larger old cap still wins |
| Runtime [T], E=T-3,A=T-2 (both fail) | `Left (TensorShapeError (ElementLimitExceeded 1152921504606846973 1152921504606846974))` | e2 / same |
| Pure scalar[], A0; runtime scalar[], E=M,A0 | `Left (TensorAffineError (AffineLimitExceeded AffineElements 0 1))` | e1 / `(384,3072,384)` |
| Runtime scalar[], E0,A=M or E0,A0 | `Left (TensorShapeError (ElementLimitExceeded 0 1))` | e1 / same; old cap first |
| [M,M,0], pure A0 or runtime E0,A0 | Count0 at nil, **no count error** | e4; any later pre-plan exit bounded `(768,6144,384)`; all dimensions must still pass |
| Runtime [T], E=A=M, remaining work128 and ample cells | `Left (TensorAffineError (AffineLimitExceeded AffineWork 384 385))` from second debit using initial usedWork256 | e1 / `(384,3072,384)`; **nil uninspected**, no machine/count diagnostic |
| Pure scalar[], A0, startup-only cells/work | `Left (TensorAffineError (AffineLimitExceeded AffineConstructedCells 2432 2433))` | e0 / `(256,2048,256)`; nil uninspected |

Remaining cells in the work-failure row are sufficient for two coupons and live≥384; otherwise cells/live competition follows the existing debit order. More generally, failure bounds add the coupons of earlier completed scans; they do not reset e between base/source/target/list arguments. Even a known late zero must pay the nil coupon before returning the final count.

`CountDiagnostics.hs` freshly compiles/runs a **model** with local `TensorShapeError`/`TensorAffineError` wrappers around the actual existing exported `ShapeError` and declaration-only proposed `AffineProblem`. It is not the future integrated public `TensorError`. Its scanner mirrors the bounded count/credit stage after dimension admission, checks all exact fixtures above, and compares against an independent exact-product oracle over73 small shapes ×16 old caps ×16 affine caps =18688 cases at model M15. The oracle is deliberately finite; no real tensor allocation/affine execution occurs. The retained older Python `Rejected('machine')` model is insufficient for R2 and is not relabelled the new exact-diagnostic evidence.


After all header checks, compute the full §8 plan using cached bounded ranks/counts only. Check cumulative **L, W, Q**, in that order. Header coupons are a subset of the complete plan, not added twice. On success the remaining local reservation is the full plan less already debited header work/cells; all subsequent loops are prepaid. There is no refund and no success report based merely on e. An early malformed header returns without needing the full semantic reservation.

### 6.4 Capped arithmetic

All successful formula values and counters must fit M. `satAdd(c,x,y)` returns c+1 if either exceeds c or `x>c-y`, else x+y. `satMul` first handles zero, then over-cap inputs or `x>c div y`, else product. Evaluate W,H,R,Q,L with cap **M**, not configured work cap, then apply each cumulative cap. This prevents an underestimated L from reversing cells-before-work priority. P=V(V-1)/2 uses V=0 guard and divides the even factor first. Formula overflow consequently rejects the cell reservation (L exceeds M); no oversized exact product or success report is built. Error fields are bounded sentinels, not huge rejected totals.

## 7. Numerical failure bounds and complete error schedule

For every pre-plan exit let e be the **number of successfully debited prefix coupons**, including the node on which a semantic header error was discovered. Then, in report-independent failure-envelope order:

`W_header = 256 + 128e`

`L_header = 2048 + 1024e`

`Q_header = 256 if e=0, otherwise 384`.

These bounds include terminal error construction/forcing and failed debit control. They count computation even though no successful usage commits. They are not separately rank-scaled allowances. With remaining successful-path cells c and work w, `e <= min(floor(c/1024), floor(w/128))`; additionally each scan has its own rank/length sentinel bound. If counters cannot represent another coupon, return bounded credit failure without inspecting a node.

| Exit class before complete plan | e and exact applicable bound |
|---|---|
| Negative/minimum policy rejection; fixed getter/setter/startup channel | e=0: `(W,L,Q)=(256,2048,256)`; successful initialization uses its separate startup reservation. |
| Closed/disabled runtime, old counter nonrepresentable detected at entry | e=0, same fixed bound. Lock waiting/exceptions are outside logical work. |
| First credit rejection with exhausted startup-only budget | e=0, same fixed bound; **zero** shape/list nodes forced. |
| Rank excess at axis k of current scan | completed earlier scans plus k+1 coupons; excess dimension itself unforced. Plug that e into the three formulas. |
| Dimension violation at axis k | completed earlier scans plus k+1; earlier dimension beats an uninspected later rank excess. |
| Element/machine count failure | completed earlier scans plus current rank+1 (nil included); scalar failure uses one coupon. |
| Transform rank mismatch | `e=rB+rS+rV+3`; no signed-list node forced. |
| Short list with t entries | completed earlier scans plus t+1 (nil included). |
| Long list with expected t entries | completed earlier scans plus t+1 (excess cons included, its value/tail unforced). |
| Credit failure partway through any shape/list | only already admitted e; rejected next node not forced. |
| Complete-plan formula/cumulative failure | e from §6 table; formula and error control included in F; no signed list value/descriptor/seed/registry read. |
| Old payload preflight failure after header, before full affine reservation | pullback/bind e from §6; bounded constant arithmetic in F, no seed/allocation. |

Every later semantic/engine failure has already passed full reservation, so its **finite engine prefix** is bounded by that operation's `(W,L,Q)` (no extra F). Allocator/finalizer/exception-String handling and arbitrary external execution are excluded as below; their total failure cost has **no finite W/L/Q theorem**. Failed allocations/initialization still commit no old/new counters/ID/result. No success report is fabricated to represent failed work.

Concrete header traces:

- Limits exactly startup `(2432,256,384)`: first cells debit needs1024 additional and fails; e0 gives W256,L2048,Q256. With roomy cells but no work remaining, `AffineWork` wins and still e0. Policy rank may be M.
- Roomy cells, 256 remaining work, live384, 100,000 dimensions of1: two cons inspected; third debit rejects work, before touching node3. e2 gives W512,L4096,Q384.
- Rank allowance0, next shape cons999: with one coupon, rank wins before dimension999. e1 gives W384,L3072,Q384. With no coupon, credit wins before rank.
- Rank1, dimension cap5, shape `[999,1]`: first dimension error wins before later rank excess. Same one-coupon bound.
- Scalar `[]`, element cap0: one nil coupon; `AffineElements`, same bound. `[5,5,0]`, dimension cap5, element cap4, rank3: four coupons, count0 succeeds; no premature product failure.
- Base `[5]`, view `[3]`, stride list with two entries: base2+view2+list2=6 coupons, length-long observed2, W1024,L8192,Q384. Last list element may be bottom without affecting this spine error.
- Base `[5]`, view `[0,1]`, strides `[0,7]`: complete header e8, W1280,L10240,Q384. Full plan is L46080,W5632,Q1024; semantic empty error requires that plan. Pure work limit5887 (one below startup+plan) returns work error first; at5888 raw-empty is reached if other limits admit.

### Ordered semantic suffix after admitted plan

All `AffineProblem` diagnostics use `TensorAffineError`. The exact machine-count and old effective session-shape cap errors instead use `TensorShapeError` as explicitly specified in §6.3. Existing runtime/session/payload error constructors retain their roles. No checked result is promised for bottom, caller exceptions, process OOM or asynchronous interruption.

1. Signed input checks in argument order and axis order: offset then strides; permutation entries; reverse scalar axis; starts then steps. Reject minBound even when empty/singleton. A later minBound therefore beats an earlier noncanonical-empty coefficient or zero step.
2. New map: raw empty canonicality first; then singleton normalization; nonempty checked extrema arithmetic → extrema bounds → per-address checks → lexicographic collisions → fully forced result.
3. Permute: ranges → duplicate pairs → target dimensions → descriptor derivation → empty/singleton canonicalization → full target validator → result.
4. Reverse: axis range → empty canonicalization or checked offset term/add/negation → singleton normalization → full target validator → result.
5. Slice: all zero steps → each axis's endpoint arithmetic/domain check → all offset derivation products/sums → stride products → canonicalization → full target validator → result.

`AffineAddressBounds lo hi B` reports checked extrema; redundant per-address failure uses that address for both bounds. `AffineOverlap i j a` reports first lexicographic pair. `AffineArithmeticOverflow field axis` identifies offset/stride derivation or extent axis; `AffineParentDomain axis low high sourceDimension` uses the checked endpoints or empty anchor for low/high. `AffinePhysicalBounds offset count capacity` is for nonnegative interval data; invalid negative/overflowing private signed metadata uses `AffineArithmeticOverflow AffineOffset 0` before any conversion. Safe public constructors cannot forge metadata, but private faults must have defined behavior on this new path.

## 8. Successful reservations and auditable coupon algorithm

Let rB=base rank, rS=source rank, rV=current/target view rank, B/V logical counts, P=V(V-1)/2, and n=current committed buffer count before pullback. Success pays the following **exact reservations**, including header and result forcing even for identities/empty cases:

| Operation | W | H | R |
|---|---|---|---|
| New map | `4096+512*(rB+rV)+V*(128+64*rV)+P*(320+128*rV)` | `512+64*(rB+rV)` | `256+16*rB+24*rV` |
| Any one transform | `4096+1024*(rB+rS+rV)+512*rS*rS+V*(128+64*rV)+P*(320+128*rV)` | `512+64*(rB+rS+rV)` | `256+16*rB+24*rV` |
| Bind | `4096+512*(rB+rV)` | `512+48*(rB+rV)` | `256+24*rV` |
| Fresh pullback | `4096+768*(rB+rV)+64*B+V*(256+128*rV)+64*n` | `768+64*(rB+rV)+8*n` | `256+16*rB+8*(n+1)` |

Always `Q=H+R`, `L=8W+Q`. No optimizer/identity/inherited-proof refund. These replace **all** old draft startup and successful constants.

### 8.1 Elementary slot/microstep discipline

The model is a register/list/frame machine implemented prospectively with strict bounded loops. A primitive inspects at most two existing slots and writes at most two result slots; widened arithmetic uses at most four temporary/result slots. A recursive/control transition creates at most one two-slot frame and two argument/result slots. Thus **at most eight newly constructed logical slots per work step**, even retaining the overwritten boxed result, is a conservative upper bound. Bulk tuple/record/list construction is one assignment step **per field/slot**, not one step per whole object. Complete forcing walks each new list/record slot under explicit coupon work; arbitrary referents such as an owner String are not traversed.

This is not a claim that arbitrary Haskell syntax automatically satisfies eight cells. An implementation using uncharged `(++)`, `mapM`, `zip`, `(!!)`, `sort`, `Set`, `nub`, a V-address array, coordinate list per uncharged loop, caller String copy, or lazy post-publication work does not implement this model. Source review must map its strict loops/constructors to these coupons, or reject it. There is no unnamed future envelope.

### 8.2 Itemized work and storage coupons

The following are upper budgets for concrete subroutines, not additional charges atop the formulas:

| Subroutine | Work expansion / maximum | Newly retained / peak accounting |
|---|---|---|
| Fixed entry/terminal | F256 total. Fixed post-header plan/control/report/publication administration ≤1536: plan/capped arithmetic128, fixed semantic/physical checks128, wrapper and state field assignment256, force fixed records256, transaction/bracket/result administration256, bounded diagnostic/return control256, loop setup256. Sum1536. | Fixed map/bind result ≤128 slots, fixed pullback result ≤128; fixed workspace128 (pullback256). No copied String. |
| One header node/sentinel | 128: debit/check/update32, spine/tag/field access8, rank/dimension comparisons16, capped product/zero state24, counter/cursor updates16, loop/terminal branch16, slack16. | Reused ≤128 variable scanner slots, no copied list, no rank-sized frame chain. Tail-recursive strict cursor/count/product. |
| Base metadata after header (new map) | ≤80/rB: reread admitted singleton and bounded value16, dimension/reference list assignment16, cursor/count/force16, auxiliary arithmetic/links32. | Shape evidence shared, not reconstructed with `someNatVal`; copied numeric list nodes ≤2 per axis plus explicitly assigned references. |
| View metadata after header (new map) | ≤208/rV: shape/zipper construction64, signed/raw empty tests32, normalize/extent products and partial extrema64, contiguity comparison32, force new numeric/spine slots16. | Forward dimensions2r, normalized strides2r, reverse pair zipper4r, bounded temporary frames included below. Header already forced all singleton dimensions. |
| Transform metadata | ≤256 per base/source/target axis after header, plus ≤128*rS²: pair-duplicate suffix loops32*rS², dimension lookup32*rS², stride lookup32*rS², remaining lookup/control32*rS². | ≤40K+128 workspace for K=rB+rS+rV, including source/target copies and traversal frames; no parent history. |
| Bind metadata | ≤192*(rB+rV) after header, covering interval/extrema shift, shape/stride copy/normalization, wrapper construction and complete forcing. | Workspace ≤128+32K, retained ≤128+12*rV. |
| Pullback metadata | ≤320*(rB+rV) after header: physical bound checks; prepare map and seed coordinate zippers; construct fresh canonical base strides/dimensions; force descriptor, result, counters. | Workspace ≤256+40K+4n, retained ≤128+8*rB+4*(n+1). |
| One coordinate/address kernel | ≤96+48r: fixed96; per axis right-to-left zipper/digit `quotRem`/push≤24, forward multiply/add/bounds/pop≤24. Each micro-operation expanded per the slot rule. | Digit list2r, right/left traversal frames≤4r, bounded numeric/control temporaries; drop before next kernel. No payload in this kernel. |
| Address validation loop | One kernel per V plus loop/check control fits `128+64r` each. | No retained addresses. |
| Collision pair | Two sequential kernels plus32 pair-loop/compare work: `224+96r <= 320+128r`. | Only first address scalar survives second kernel; no simultaneous pair coordinate arrays. |
| Zero destination | ≤16/B: bound/index conversion, pointer arithmetic/write, increment and branch. Reservation is64/B. | No destination list; managed bytes counted separately. |
| Seed read + scatter | Two kernels (seed physical, logical map destination) plus64 loop/check/peek/poke work: `256+96r <= 256+128r`. | One seed scalar at a time, no seed list. Finite evidence means no new payload refinement scan. |
| Registry append and force | **Requires §9.0 fully forced old spine invariant at every prior commit.** Old-cons inspection8 + copied cons/return8 + new-spine inspection8 =24 per old node, plus fixed24 for fresh singleton/nil, drawn from fixed1536. Reservation64/n. No old-history normalization. | ≤4n temporary frames/slots, ≤4(n+1) retained list/reference slots; H8n/R8(n+1) dominate. Preserve allocation/cleanup order. Exact reduction/terminal proof and history models in §9.0. |
| Private shape/result forcing | SShape evidence is shared; complete header has visited its admitted spine/values. Numeric dimension/stride copies and every new result/report/state field are fully forced within the above linear/fixed coupons. | Old tensor/owner/map referents shared. Do not deep-force owner strings or payloads. New wrapper/tag/state/report/control slots are R, not omitted as compiler magic. |

The stated fixed administrative category ceilings total1536 (128+128+256+256+256+256+256=1536). Coupon slots count a bounded logical representation, not actual runtime dictionary/closure layouts.

### 8.3 Envelope proof, not an appeal to future constants

Let kernel K(r)=96+48r. For new map, the subroutine work sum is bounded by:

`256+1536+128*(rB+2rV+3)+80rB+208rV + V*(K(rV)+32)+P*(2K(rV)+32)`.

The extra32 per address explicitly pays the outer validation loop; `K(r)+32=128+48r <=128+64r`. The transform address pass pays the same extra32. These are not hidden inside pair-loop coupons.

Its fixed/linear terms are `2176+208rB+464rV`, below `4096+512(rB+rV)`. Address/pair inequalities are above. Thus full header, metadata, forcing, kernel, pair and fixed work fit W coefficientwise.

For transforms use the worst header count `rB+3rS+rV+5` (slice), followed by `256*(rB+rS+rV)+128*rS²`. The fixed sum is `256+1536+640=2432`; rank coefficients `(384,640,384)`, below the row's1024 each, and128rS² below512rS². Permute/reverse scan less but pay the same row.

Bind uses `256+1536+128*(rB+rV+2)+192*(rB+rV) = 2048+320*(rB+rV)`, below its row. Pullback uses `256+1536+128*(rB+2rV+3)+320*(rB+rV)+16B+V*(2K(rV)+64)+24n`: fixed2176, rank coefficients448/576, below4096+768K; scalar and registry inequalities are above.

The registry term is now justified by §9.0’s enforced invariant and evaluated history events, not by the older algebra model’s substitution of24n. Write fixed1512 + measured registry(24n+24) to include terminals without double charging.

Actual workspace/retained envelope pairs used in the independent model are:

- Map: `(128+32K, 128+8rB+12rV)`.
- Transform: `(128+40K, 128+8rB+12rV)`.
- Bind: `(128+32K, 128+12rV)`.
- Pullback: `(256+40K+4n, 128+8rB+4(n+1))`.

Each is coefficientwise at most its row's H/R. Since each work step constructs ≤8 slots, `constructed <=8*subroutineWork+workspace+retained <=8W+H+R=L`; simultaneous additional live `<=H+R=Q`. The extra H/R in L intentionally overreserves setup/retention, not undercounts it. Managed payload remains a different account. The 12,000-case scratch model checks these inequalities over ranks0..4, V0..5, n0..3 and all four operation rows; the coefficientwise proof above covers nonnegative ranks/counts beyond that finite enumeration, subject to admission saturation.

### 8.4 Nested old reports

Bind: `TensorOperationReport "affine/bind" 0 (TensorMemoryReport 0 0 0 0)`.

Pullback: `TensorOperationReport "vjp/affine-base" (B+V) (TensorMemoryReport (8*B) (8*B) 0 1)`.

B zero writes and V cotangent copies give the old scalar field; W separately covers complete affine logical engine work. Scratch managed payload bytes0 does not mean no Haskell workspace. All existing operations and their rendered reports stay unchanged.

## 9. Runtime transaction, representation and exact ownership handoff

### 9.0 R1: committed-registry invariant, universal publication schedule, and source trace

**Explicit additional private strictness/publication amendment selected by the parent.** In the prospective revised library, every committed session registry has a fully evaluated list **spine through its terminating `[]`**. This applies to all sessions created by that library, with affine policy enabled or disabled, and to every successful allocation commit through legacy APIs as well as affine pullback. No claim is made about a session created by an older compiled unit. A strict list field or strict state record is insufficient. No deep `ForeignPtr` forcing, finalizer execution, payload dereference, or arbitrary referent evaluation is required to establish this invariant.

For an **open** session, registry length equals `stateBuffers`, entries are in successful allocation order, and each committed pointer is represented once. At close, counters remain cumulative while registry becomes `[]`; do not assert the length equality on closed states. The detached old list remains fully spine-evaluated and becomes close-cleanup's responsibility. Rejecting a transaction preserves the previously established state/invariant. Affine entry does not normalize an old registry or charge another operation's deferred history.

#### Actual source and complete publication-path inventory

Fresh numbered snapshot: `Internal.numbered.txt` in this bundle; actual identifier search found only initial creation, close, and `allocatePayloads` registry publications. Some inherited line citations are approximate; the following use this snapshot.

| Actual path | Current behavior | Required prospective invariant/ownership step |
|---|---|---|
| `withTensorSessionAllocator`, Internal:108 | `newMVar (SessionState 0 0 0 0 [] False)` | Empty registry is already spine-normal; initialize new affine fields as specified. No registry history migration. |
| `allocatePayloads`, :481–515 | `modifyMVar`; accepted result uses `oldRegistry ++ pointers` at :513. Neither that expression nor strict field :63 forces all tails; even state WHNF need not be forced by the current MVar publication. | All accepted allocating paths below enter one rollback-protected masked prepare-and-publish phase. Force new registry **inside** it, before actual state put. |
| `hostTensorBatchFromLists`, :602–623 | Public dynamic batch, via :615; used by SafeTensors decode. Every complete batch preflights/validates before allocation. | Stage k pointers in original order, then establish full spine on `old ++ fresh`. All k disclosed pointers remain staged-owned until one commit. No report/old numeric admission changes. |
| `hostTensorFromList`, :625–643; `finiteTensorFromList`, :645–666 | One buffer through :636/:656, including zero-length payload. | Same universal transaction/invariant; count/ID increment1 even for length0. |
| `makeFinite`, :768–784 | One allocation via :778; primitive outputs, copy, unary/reduction/tape helpers reach it. | Same universal transaction. Do not special-case constructors but leave copies/primitive paths lazy. |
| `makeTwo`, :1035–1057; `makeTwoShapes`, :1059–1078 | Two outputs via :1048/:1069, including empty outputs; Hadamard/matmul VJPs. | Both staged pointers owned until one publication. Preserve allocation/finalizer/ID order and old preflight/report semantics. |
| Empty public batch `hostTensorBatchFromLists session []` | Reachable: bounded batch scan accepts []; `traverse`, `zipWithM`, payload list all []; `preflightPayloads` and `allocatePayloads` still check session/budgets and publish a zero-count result. Old registry expression is `old ++ []`. | Preserve both preflight stages and checked failures. On accepted zero-buffer commit share the already-normal old registry (or force an equivalent append under the bracket); no allocator call or new ID/buffer. The selected algorithm shares it. Public nested report remains `"from-lists"`, work0, all memory fields0. Do not silently skip closed/error checks. |
| Private `allocatePayloads ... work []` | Signature admits arbitrary declared scalar work with no payload; public empty batch supplies0. | Preserve original scalar admission/increment/report for the supplied work; count/bytes/ID unchanged. Registry shared and already-normal. No false assertion that every zero-buffer commit has scalar0. |
| `preflightPayloads`, :448–471 | Read-only snapshot under `withMVar`; restores same state. | No new registry traversal or reservation; previously normal spine shared. |
| AddTape VJP :983–984; transpose/reshape/ownership/refinement/observers | No session-state allocation publication; AddTape shares seed and ignores session. | No invented commit/affine charge/full-registry walk. Consumer/lifetime limitations stay as §10. |
| New affine bind | State publication changes affine counters only. | Share already-normal old registry. No n-sized work or forcing on entry. |
| New affine pullback | One new zero/scatter allocation. | Full new spine before state put; old spine prerequisite comes from **all** prior commits, not a claim that today's runtime already does it. |
| `closeTensorSession`, :144–150 | Under MVar, closed/no-op returns same state; first close replaces registry with[], marks closed, detaches old list; finalization follows outside lock. | Publish the trivially normal empty spine. Ownership of detached list transfers to cleanup at that put; later close takes no entries. Preserve async retry and diagnostics. |
| Rejections, allocator/init failures, pre-publication exceptions | Return/restore old state; initialization cleanup already staged. | Preserve old normal spine. Clean only new disclosed staged pointers. A cleanup exception must not strand the empty MVar or transfer ownership twice. |

There is no other assignment to `stateLiveAllocations` or direct `SessionState` construction in production tensor source. Private test-injected states must satisfy the invariant except in an explicitly named mutant/corruption fixture. All consumers allocate through this seam; adapters/serialization do not install their own registries.

**Preserved old numerical/report contract.** Do not move or alter old `checkedShape`/dynamic shape/finite-value/payload ordering, use new capped diagnostics in legacy APIs, reinterpret Natural limits, change arithmetic or StorageIds, charge affine cells/work to legacy calls, or modify old report strings/fields. The old reports never claimed complete registry/list/heap/CPU work; forcing runtime-owned registry metadata adds private work within their existing scope, not a new scalar/report meaning. Cumulative legacy construction of n singleton buffers may itself copy `n(n-1)/2` registry cons cells across commits; it is **not** claimed globally linear or retrospectively charged to affine usage. The new affine transaction is linear in its already-normal input spine. If eventual code needs a public/report/counter change rather than this private strictness/publication change, parent arbitration is required.

#### Actual masking gap and selected protected publication algorithm

Compiler-local source `GHC.Internal.Control.Concurrent.MVar` (retained as `MVar-source.txt`) defines current `modifyMVar` as:

```haskell
mask $ \restore -> do
  a <- takeMVar m
  (a',b) <- restore (io a >>= evaluate) `onException` putMVar m a
  putMVar m a'
  return b
```

The `evaluate` only forces the **pair**, not `a'` or the complete registry. An ordinary callback runs restored/unmasked. `allocateStaged` at Internal:518 enters its own `mask`, but returning from it restores the callback's prior masking state. Current result preparation at :506–515 is outside that helper's cleanup ownership interval. Its mask is not an enclosing rollback handler. Adding `evaluate (forceSpine newRegistry)` in that unprotected post-`allocateStaged` gap is **not** the amendment: async interruption or a synchronous forcing exception could precede registration with no caller-owned cleanup set. This is source reasoning, not an executed leak or affine-runtime test.

The selected prospective implementation schedule is a common private transaction discipline for `allocatePayloads` and new affine publication, retaining their public signatures:

1. Enter an outer `mask`, acquire the MVar (interruptible until acquired), and establish an exception-safe restoration duty for its empty slot. Existing legacy pre-staging validation/old rejection computations may run under `restore`, before any pointer exists, with original precedence. New affine entry/header follows its existing §9.2 schedule. Any such failure restores old state.
2. After admission, enter the protected preparation phase **without restoring masking**. Install staging/cleanup ownership before the allocator can disclose a pointer; allocator owns undisclosed partial resources on failure. Each disclosed pointer is added to the staged set before initializer/metadata work. Existing per-allocation/init checked-failure/async behavior is preserved. An interruptible allocator can still deliver async exceptions under masking and must be caught by this staging protocol.
3. Helper-internal allocation/init failure cleans its own staged set using the existing order/protocol and does not transfer it. On success, while still masked, helper transfers a known staged pointer list to the enclosing preparation owner. Force its reversal/list spine while that owner can still enumerate all disclosed pointers for rollback; never discard the original staged ownership list while forcing a candidate representation. No unmasked return gap. For k=0 there are no pointers but state-restoration duty remains.
4. Under the same preparation owner, append a fully spine-known fresh list to the already-normal old list and force the **entire candidate spine**. Old pointers are shared references, not newly rollback-owned. Prepare/force the allocator's runtime-owned `(StorageId,ForeignPtr)` result spine, report/counter/state records and the return pair to the specified level before publication. Do not deep-force ForeignPtrs or arbitrary caller strings. For new affine, additionally force all §9.3 new affine tensor/layout/binding/result metadata here.
5. Legacy tensor wrappers made by `hostTensorFromList`, `makeFinite`, etc. **after** `allocatePayloads` returns remain post-commit computations as in the current API. This amendment does not silently change them to deep eager evaluation. The allocator's prepared result/report/state is protected; once committed, any later wrapper/action exception leaves registered buffers owned by session cleanup, not staged rollback. The stronger fully-forced affine result requirement remains local to new affine APIs.
6. Make preparation yield either a checked failure with old state or an entirely prepared candidate `(newState,result)`. All callbacks, full-spine forcing and other synchronous throw-capable work are inside the pre-commit cleanup handler. The actual `putMVar` into the transaction-owned **empty** slot is the single success linearization/ownership-transfer point. Execute it masked with already evaluated arguments; under the empty-slot invariant it is nonblocking. There is no restore, callback, additional forcing, or allocation between successful preparation and this put. Do not treat return from a `modifyMVar` callback as the commit point. The transaction abstraction must own/control the real put, not hand a still-staged value to an unmasked outer wrapper.
7. That put is outside the lexical staged-cleanup handler; after it, no such handler can finalize or restore the staged set/old state. Post-commit exceptions go to caller/runner, whose session registry now owns the new entries. A queued async exception delivered on unmask after the put may prevent the caller receiving a result, but does not roll back a completed commit. Registry-force/metadata/pre-put exceptions attempt staged cleanup and restore old state, even if cleanup reporting itself throws. Lock restoration must be in a guaranteed outer finally/handler; restore **never** runs after a successful put.

Existing synchronous allocator/initializer failures keep `HostAllocationFailure` with their current allocation index, primary and cleanup diagnostic composition. Unexpected synchronous post-initialization engine/forcing exceptions retain their ordinary exception channel after attempted staged cleanup (`throwAfterCleanup` conventions); do not invent a budget failure or new checked error constructor. This extends cleanup coverage, not a new public report/error meaning. Async exceptions retain existing rethrow/cleanup-interruption protocol. Callback retries can occur; no fixed invocation count or physical-release guarantee is introduced.

#### Inductive strictness proof and concrete 24n/64n registry coupons

Base case: the initial `[]` is normal. Inductive cases: rejected/no-allocation updates retain it; accepted k=0 commit shares it; accepted k>0 append produces a finite candidate whose entire spine is forced before publication; close publishes[]. Hence every **future-unit** open state available to the next transaction contains no runtime-owned deferred append spine history. There is no rank/n-dependent normalization hidden at affine admission.

The prospective list algorithm uses ordinary order-preserving append and a strict tail-recursive spine-only walk. With old length n already normal and fresh length k already normal, an instrumented demand evaluator observes:

- n old-cons append matches/copies, exactly **one** append-nil reduction;
- n+k+1 output spine inspections (including terminating nil);
- no recursive evaluation of older append histories, no head/payload evaluation.

Allocate at most8 work to each old-node inspection,8 to its new cons/control return,8 to each output-spine inspection, and8 to the append terminal. Total `16n+8+8(n+k+1)=24n+8k+16`. For affine k=1 this is **24n+24**. The extra24 is explicitly debited from the existing fixed loop/transaction administration256 inside the fixed1536 budget (leaving232 there); it is **not** a zero-cost terminal or another n-term. The full pullback subroutine proof may thus write fixed1512 + measured registry `(24n+24)`, which equals its previous fixed1536 +24n. Successful W/H/R/L/Q constants and all existing exact/one-below figures are unchanged.

Temporary append/control frames use at most4n slots; newly retained registry cons/reference slots at most4(n+k); affine row H8n and R8(n+1) dominate these. Output walk is tail-recursive; no n-sized accumulated list of force results. Candidate and old spine coexist, but old pre-existing cells are excluded as before; every **new** copy is counted. Per-step construction remains ≤8 logical slots. Inspection/copy/terminal/publication coupons are logical units, not GHC byte/CPU counts. Bounded state-field forcing and one MVar publication remain in the named fixed record/transaction coupons. Actual Haskell implementation must match these operations; the proof does not grandfather an arbitrary `(++)` history as an external thunk.

#### Strictness-sensitive executed evidence and counterexample

`RegistryHistory.hs` uses a lazy append with instrumented cons/nil reductions, a state-like strict list field forced to WHNF on every commit, and a separate spine-only forcing walk. This WHNF assumption is **more eager** than current `modifyMVar` guarantees; the deferred-history counterexample therefore does not depend on exploiting an even lazier state record. Counter effects are scratch-only `NOINLINE` instrumentation; commands disable CSE/full-laziness and run both -O0 and -O2. No TensorAllocator, tensor constructor, real registry or payload is executed by this program. It is a synthetic Haskell evaluation model of the source expression.

| n singleton allocations | Cons copied while creating WHNF-only states | Further old-history cons copied by forcing old spine | New final-affine copies after forced commits | Final-affine registry work including constant24 |
|---:|---:|---:|---:|---:|
|0|0|0|0|24|
|1|0|0|1|48|
|2|1|0|2|72|
|3|2|1|3|96|
|1024|1023|522753|1024|24600|

For n≥2 the pending old-history copies are `(n-1)(n-2)/2`. At1024, forcing the **new** affine append directly without first normalizing the old history copies `522753+1024=523777` cons cells; that alone exceeds entire pullback W71168. The old normalization cannot be excluded as caller thunk evaluation. Synthetic no-op history also matters: `[3]` followed by1024 empty appends leaves2048 pending cons copies and1024 nil reductions with n still3. Under the prospective invariant, zero-buffer commits share the known spine and leave no history.

Both native runs also cover batches `[]`, `[0]`, `[0,0]`, `[2]`, `[2,0,1,3,0]`, `[512,0,512]`, `[1,0,0,0,1]`, `[0,3,0,0]`, and a list with undefined elements that survives full-spine forcing. `models.py` independently evaluates memoized lazy append histories, obtains actual reduction counts, and feeds them into720 full pullback coupon combinations. It does **not** just substitute24n. The copied older 12,000-case algebraic model is retained/rerun only as formula regression evidence, not as the R1 proof.

#### Frozen prospective mixed-history and fault fixtures (not executed runtime tests)

Mixed fixture: enable affine policy **at session creation**, then perform1024 existing `finiteTensorFromList session (knownShape @'[0]) []` calls without any private registry observation. Keep two results as empty base and same-shaped seed; they are included among the1024, not allocated afterward. Own the first explicitly, build the pure empty `[0]→[0]` map with o0,[0], and bind it. Pure map uses its own planning budget, not runtime usage. Buffer IDs0..1023 are legacy commits; bytes/scalar totals remain0, buffer count/next ID1024. No test may normalize the registry while checking setup.

First affine pullback has B=V=0, ranks1/1, n1024:

`W=4096+1536+65536=71168`, `H=768+128+8192=9088`, `R=256+16+8200=8472`, `Q=17560`, `L=569344+17560=586904`.

Runtime after bind is `(44280,5376,888)`; after pullback **(631184,76544,17560)**. Individual one-below fields are **631183 / 76543 / 17559**, giving respectively `TensorAffineError (AffineLimitExceeded AffineConstructedCells 631183 631184)`, `... AffineWork 76543 76544`, `... AffineLiveCells 17559 17560`, with other fields exact or roomy. All three below gives cells first. These policies still permit the1024 old calls and bind. Every failed full preflight does zero allocator calls, seed reads and registry-forcing steps relative to the post-bind snapshot; no IDs/counters commit. Exact success creates one length0 buffer, ID1024, old buffers1025, bytes0, scalar0, nested report `"vjp/affine-base"` work0/memory `(0,0,0,1)` and original bound owner/base shape. Registry coupon24600 includes24 fixed work; no payload dereference.

Mutation requirement: omit whole-spine forcing on **legacy allocation commits** while retaining WHNF and the same published report formulas; keep full forcing at affine publication. A mere report equality test will not detect this mutant. The future private test build must instrument append-demand/force primitives during the **natural execution**, without inspecting the registry to set up the fixture, and independently assert zero deferred-old-copy events at first affine execution plus n=1024 old-cons copies, one append nil, n+2 output inspections, and the full work envelope. The mutant produces523777 copies (at least522753 historical), exceeding W71168 even at one unit per copy; therefore it must fail this dynamic-work/strictness assertion although its dishonest declared report could be identical. No physical heap/time threshold is used. Disable optimizer transformations that invalidate instrumentation or use the explicit private operation-counter path, and keep the release implementation independent of test callbacks.

Add `[512,0,512]` batched history and accepted empty-batch calls before/after the mixed fixture, plus zero-buffer private work-only commit. Assert reports/numeric ordering unchanged and no allocator or ID for b0. Do not observe old registry before the first affine operation in the mutant-sensitive lane.

Future fault points (private deterministic counters/gates; no unsafe freed-memory reads): allocator success before init, init of each staged buffer, helper-success handoff, middle of candidate-registry forcing, result/state metadata forcing, immediately before actual put, and after actual put. Test k0/1/2 where reachable, including second-allocation failure. Pre-put failure: only fresh disclosed pointers are cleanup-owned, old normal state and all counters/next ID restored; same uncommitted ID available on retry. After-put exception/async delivery: commit remains, new registry owns fresh pointers, only session close finalizes them, not a staged handler. Throw during cleanup diagnostic handling and confirm MVar restoration; preserve existing checked/exception diagnostic channels. Async gates must test delivery at an interruptible masked preparation point and queued delivery after unmask, not promise async delivery in a noninterruptible masked pure loop. Caller test harness joins or cancel-and-joins all workers on every exit under lifetime A. Existing finalizer retry/failure tests remain; successful cleanup callbacks are test assumptions, not physical-release guarantees of the contract.

`models.py` executes a symbolic token-ownership fault schedule for these pre/post publication cases and cleanup-diagnostic exceptions, including k0/1/2. It proves properties of that **model**, not runtime exception safety. Actual implementation source/fault evidence remains a separate required gate.


### 9.1 Private representation schedule

```haskell
data CheckedLayout shape = CheckedLayout
  { layoutDimensions :: ![Natural]
  , layoutOffsetElements :: !Integer
  , layoutStridesElements :: ![Integer]
  , layoutIsContiguous :: !Bool
  }
type role CheckedLayout nominal

data HostTensor region (dtype :: DType) (shape :: [Nat])
  = HostTensor !(SDType dtype) !(SShape shape) !(CheckedLayout shape)
      !(StorageId region) !Natural !Natural !(ForeignPtr Double)
-- Natural fields: logical element count, backing storage capacity in elements.
type role HostTensor nominal nominal nominal

data AffineLimits = AffineLimits
  { affineLimitRank :: !Natural, affineLimitDimension :: !Natural
  , affineLimitElements :: !Natural, affineLimitConstructedCells :: !Natural
  , affineLimitWork :: !Natural, affineLimitLiveCells :: !Natural }
data AffineBudget = AffineBudget !AffineLimits !AffineUsage

data AffineMap (map :: Type) (base :: [Nat]) (view :: [Nat])
  = AffineMap !(SShape base) !(SShape view)
      !Natural !Natural !Natural !Natural
      !Integer ![Integer] !Integer !Integer !Bool
-- ranks B/view, logical counts B/V; offset, normalized strides, min/max, contiguity.
-- Empty extrema are zero as vacuous summaries, not valid-address assertions.
type role AffineMap nominal nominal nominal

data OwnedAffineView (region :: Type) (owner :: Type) (map :: Type)
                     (base :: [Nat]) (view :: [Nat])
  = OwnedAffineView !(OwnedTensor region owner 'F64 base)
      !(AffineMap map base view) !(FiniteTensor region 'F64 view)
type role OwnedAffineView nominal nominal nominal nominal nominal

logicalOffsets :: Natural -> CheckedLayout shape -> [Int]
```

Add private `limitAffine :: !(Maybe AffineLimits)` and `stateAffineUsage :: !AffineUsage`. Disabled startup is zero; enabled startup as §6. The public seven-argument `tensorSessionLimits` still constructs a disabled policy. Preserve nominal roles on layout/storage/region/owner/finite/owned wrappers. No public constructor changes.

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

This moves physical semantic errors after full affine admission so variable descriptor inspection is prepaid. The draft's noncontiguous/physical-before-full-credit priority is explicitly superseded for the **new** APIs. All old operations keep their existing order.

### 9.3 Staging ownership at every throw-capable step

The implementation direction is an **enclosing caller-owned rollback bracket** spanning staging through commit, under the universal §9.0 registry/publication amendment for legacy and new allocation paths. The detailed table here adds the new affine result-forcing requirements; it does not move legacy callers’ post-commit tensor wrappers into allocatePayloads. An initializer helper returning pointers by itself is not sufficient. A narrow private factoring may retain the old proposed signature:

```haskell
allocateStagedInitializers
  :: TensorAllocator
  -> [(Int, ForeignPtr Double -> IO ())]
  -> IO (Either (String, [String]) [ForeignPtr Double])
```

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

The state MVar publication invariant, not a separately mutable ownership flag with its own throwing gap, separates rollback from committed cleanup. Masking alone is not a synchronous-exception handler. A prospective implementation using `modifyMVar` must arrange the same ownership boundary at its actual internal state put, rather than declaring success when a callback merely returns a lazy `(state,result)` pair. This is a required source-level review seam.

Allocation happens before seed reads; all zero writes precede scatter. No base read, seed list, destination list, addition, cast-changing copy, mutation of old payloads, or second managed scratch allocation. Empty destination still calls allocator length0 once, consumes one buffer and fresh ID, and registers one ForeignPtr; no pointer dereference. The allocator may physically allocate administration or share a zero-length address; StorageId, not pointer equality, establishes freshness.

Preserve existing synchronous `HostAllocationFailure`/cleanup diagnostic aggregation and async rethrow retention. Existing `finalizeAll` retries the same callback after AsyncException before proceeding; **no finite retry count**, unconditional exactly-one callback invocation, successful physical release, or bounded String cost is promised. Once a cleanup attempt has logically discharged a rollback set under the inherited protocol, do not add an independent second finalization owner/retry policy. Failing callback side effects cannot be undone.

### 9.4 Explicit cost exclusions

Finite W/L/Q covers the specified engine algorithm through admitted pointer calls, fixed diagnostic **records**, and publication. It excludes actual allocator/finalizer execution, external callbacks, system/GC/OS allocation, MVar wait time, arbitrary caller thunks/continuations, owner String contents, and exception `displayException`, `show`, concatenation, `unwords`, traversal/forcing of arbitrary callback Strings or accumulated async diagnostics. Source `Internal.hs:152–204` has unbounded String-producing paths and unbounded repeated async cleanup. Preserving that protocol is **not** a bound on complete failure handling, time, heap, physical reclamation, or exception rendering. No purported `bounded-by-protocol` String theorem remains.

## 10. Parent-selected lifetime A and exact future public documentation

Supported region-dependent observation, refinement, primitive execution and allocation must **execute and complete** inside the runner callback's live interval. Never invoke escaped region-dependent IO actions or observe existentially retained tensors after exit. The caller must join or cancel-and-join dependent children **before callback exit on success, Left, synchronous exception and asynchronous interruption**. The runner does not do those joins. A mere start within the callback, a returned pending action, or a happy-path-only join is insufficient. Returning copied ordinary data whose reads completed inside the callback is permitted.

Nominal/direct-index protection is not temporal/effect confinement. Positive source-import types include an escaped `IO [Double]` and an existential `Packed :: HostTensor region 'F64 '[] -> Packed`. Current `tensorToList` has no closed check; primitives may release snapshot preflight before reading. A single closed-bit check would leave a check/use race. This contract adds no observer token/lease, synchronization, error-channel change, or automatic child cleanup.

The inherited lifetime investigation traced GHC9.14.1 compiler-local `GHC.Internal.ForeignPtr`: pinned `MallocPtr`/NoFinalizers allocation (`:132–144,261–305`), no-finalizer dispatch (`:497–507`), keepAlive (`:516–538`), and explicit finalization precondition (`:625–634`). Retained default tensors/closures keep the GC-pinned byte array reachable; session cleanup is not proof it was physically freed. Official `finalizeForeignPtr` still says the pointer must not be used again. This is an implementation observation, not supported post-finalization behavior.

**No current-default freed-buffer/UAF was established or executed.** Inherited -O0/-O2 private mock probes deliberately retained physically live payload and did not call real finalization during logical close: after-close reads `[17.0]`/`[31.0]` demonstrate missing access suppression, not freed-memory access. The gated `withForeignPtr` probe performed no payload read and is not an instrumented in-flight `tensorToList` trace. Public default escaped-action/existential examples were only typechecked, including freshly here. A genuinely freeing private allocator gives a conditional source-level hazard; public users cannot choose that allocator. Old probe results retain original attribution and were not rerun here.

### Exact future replacement prose; no tracked documentation edits now

1. **`Internal.hs:99–102`, runner Haddock**, replace the entire opening lifetime claim with:

   > Run an action in a fresh managed region. Nominal region indices reject direct tensor escape at a fixed external region index; they do not confine IO closures or existentially packaged tensors. All region-dependent observation, refinement, primitives and allocation must execute and complete before the callback exits. The caller must join or cancel-and-join dependent children before every exit, including Left, exceptions and asynchronous interruption; the runner does not join them. Do not later invoke escaped region-dependent actions or observe retained tensors. Ordinary copied data whose reads completed in the callback may be returned. On success, Left or exception, the runtime drains the committed registry and attempts finalization. Asynchronous cleanup interruptions are retained and the interrupted callback is retried before propagation. Finalization attempts and logical accounting do not guarantee prompt physical reclamation or release by a failing callback.

2. **`Markovian/Tensor.hs:5–12`, public module Haddock**, append:

   > Session lifetime is a supported-use discipline, not an enforced effect or linear lifetime system. Complete region-dependent work within the live callback and join or cancel-and-join dependent children on every exit. Never use escaped actions or existentially retained tensors for later observation. Nominal indices do not prevent these packages from typechecking. Unsupported post-close use is not uniformly rejected with TensorSessionClosed, and ordinary observers do not provide close/read synchronization. Default GC-pinned storage retention is not a supported post-finalization feature.

3. **`packages/markovian-tensor/README.md`, paragraph currently line10**, replace only sentence “The session finalizes all committed buffers when the rank-2 session closes.” with:

   > Closing the session drains its committed registry and attempts each finalizer; asynchronous interruption can retry a callback, and finalization is distinct from physical reclamation. All region-dependent reads, refinements, primitives and allocations must execute and complete before callback exit. The caller must join or cancel-and-join dependent children on success, Left, exceptions and asynchronous interruption; the runner does not join them. Do not later invoke captured tensor actions or observe existentially retained tensors. Returning copied ordinary data whose reads completed inside the callback is supported. Nominal region indices prevent direct fixed-index escape/coercion, not IO-closure or existential packaging; unsupported post-close actions are not uniformly rejected. The default pinned GC allocation may remain reachable after finalization, but post-finalization use is not supported.

4. **`docs/ARCHITECTURE.md:813`**, replace first sentence “A rank-2 region prevents ordinary buffers from escaping their session.” with:

   > The rank-2 region and nominal roles prevent direct fixed-index tensor escape and index coercion, not existential tensor or IO-closure packaging. Supported use requires all region-dependent work to execute and complete within the callback's live interval, with caller join or cancel-and-join of dependent children before every exit, including Left, exceptions and asynchronous interruption. The runner performs no child joins. Returning completed ordinary copies is permitted; later execution of captured region-dependent actions or observation of retained tensors is unsupported. No universal TensorSessionClosed rejection, observer/close race safety, or prompt physical reclamation is promised.

   In the same paragraph replace “Successful allocations register with the session and are explicitly finalized once at close.” with:

   > Successful allocations enter the committed registry, which is drained at close for finalization attempts. A finalizer interrupted asynchronously is retried under the inherited cleanup protocol; callback invocation is not unconditionally exactly once. Logical cleanup and explicit finalization are not proof of physical reclamation: the default GC-pinned payload remains live while reachable, and a failing callback need not release storage. Default retention does not authorize post-finalization use.

5. **`Internal.hs:78–80` exception Haddock**, replace “bounded cleanup diagnostics” with “cleanup diagnostics retained by the runtime protocol; their String contents and rendering are not numerically bounded”. Other historical messages/reports stay byte-for-byte unchanged.

6. **New Affine module Haddock**, include §3's exact map reuse/seed distinctions, the lifetime preconditions above, planning-versus-runtime distinction, logical-cost exclusions and default-disabled policy. Do not describe keeping a binding/ForeignPtr reachable as a lease excluding explicit finalization.

These are narrow future current-development documentation changes, not rewrites of historical/tagged evidence or acceptance status. Enforced liveness/leases/observer error channels require a separately scoped parent decision.

## 11. Revised exact fixtures, numerical traces and failure competition

All figures below are prospective logical-oracle results. Model PASS is not implementation PASS. Other fields are roomy when one boundary is isolated; all totals include startup unless explicitly stated.

### 11.1 Signed base-5 map and runtime account

Base `[5]`, view `[3]`, o4, strides[-2]: addresses `[4,2,0]`, extrema[0,4], V3/P3, ranks1/1. Seed `[10,20,30]` gives full base gradient `[30,+0,20,+0,10]`.

Map W=`4096+1024+3*192+3*448=7040`; H640,R296,Q936,L=`56320+936=57256`. Pure cumulative **(59688,7296,936)**. Exact cells/work/live admit; one-below **(59687,7295,935)** fails the individually reduced field before semantic descriptor work. All three reduced gives cells first. Header e6 consumes768W/6144L locally before full-plan comparison; total pre-plan failure bound W1024,L8192,Q384.

Bind W5120,H608,R280,Q888,L41848. Enabled runtime cumulative **(44280,5376,888)**; individually one-below **44279/5375/887**. No old-counter or payload change.

Existing construction of base and seed accounts bytes40+24=64, buffers2, scalar work5+3=8. Pullback n2:

- W=`4096+1536+320+3*384+128=7232`.
- H=`768+128+16=912`; R=`256+16+24=296`; Q1208.
- L=`57856+1208=59064`.
- Affine cumulative after bind+pullback **(103344,12608,1208)**. Individually one-below **103343/12607/1207**.
- Old nested report work8, fresh bytes40, maximum single40, scratch0, buffers1. Old cumulative counters become **bytes104,buffers3,scalar16**.
- Old preflight boundaries: single40/39, cumulative bytes104/103, buffers3/2, scalar16/15. Single39 must use a controlled **private preflight/session fixture**: an unchanged public session limited to39 cannot first construct the40-byte base. Do not count its constructor failure as pullback evidence.

Every failed preflight leaves allocator count and seed reads zero relative to the post-binding snapshot; all old/new counters, ID and registry unchanged.

### 11.2 Full fixed reservation table

Columns `(L,W,Q,R,H)` are operation reservations. Cumulative transform starts after the signed map; cumulative pull rows start after bind. Other map rows start at startup.

| Case | Operation `(L,W,Q,R,H)` | Cumulative `(cells,work,peak)` | Individually one-below totals |
|---|---|---|---|
| Signed map | `(57256,7040,936,296,640)` | `(59688,7296,936)` | `59687,7295,935` |
| Overlap `[4]→[2,2]`,0,[1,1] | `(81920,10112,1024,320,704)` | `(84352,10368,1024)` | `84351,10367,1023` |
| Empty `[5]→[0]`,0,[0] | `(41896,5120,936,296,640)` | `(44328,5376,936)` | `44327,5375,935` |
| Scalar `[]→[]`,0,[] | `(34560,4224,768,256,512)` | `(36992,4480,768)` | `36991,4479,767` |
| Singleton `[5]→[1]`,2,[-7] | `(43432,5312,936,296,640)` | `(45864,5568,936)` | `45863,5567,935` |
| Transform signed map, rB=rS=rV=1,V3 | `(77800,9600,1000,296,704)` | `(137488,16896,1000)` | `137487,16895,999` |
| Bind | `(41848,5120,888,280,608)` | `(44280,5376,888)` | `44279,5375,887` |
| Pull B5,V3,n2 | `(59064,7232,1208,296,912)` | `(103344,12608,1208)` | `103343,12607,1207` |
| Pull B0,V0,n2, ranks1/1 | `(47288,5760,1208,296,912)` | `(91568,11136,1208)` | `91567,11135,1207` |
| Raw-empty discriminator `[5]→[0,1]` | `(46080,5632,1024,320,704)` | reservation target `(48512,5888,1024)` | `48511,5887,1023` |

For overlap and noncanonical raw-empty, exact limits reach the specified **semantic rejection**, not success. One-below rejects reservation first. No failed attempt commits these would-be totals.

Expansion checks: overlap W=`4096+1536+4*256+6*576=10112`; empty W5120; scalar W4096+128=4224; singleton W5120+192=5312; transform W=`4096+3072+512+576+1344=9600`; zero pull W4096+1536+128=5760. All H/R/Q/L follow §8; fixed literals are asserted independently in scratch.

### 11.3 Geometry/error discriminators

- Overlap logical addresses `[0,1,1,2]`: first equal pair(1,2), address1, `AffineOverlap 1 2 1`. At cells84351 the cells error wins before collision work.
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

For x=[1,2,3,4,5], `f(x)=10*x[4]+20*x[2]+30*x[0]=140`, h=1/1024. The oracle uses this explicit indexing, not `new_model` or the proposed address kernel.

| Base i | f(x+h e_i) | f(x-h e_i) | Central difference |
|---|---|---|---:|
|0|71695/512|71665/512|30|
|1|140|140|0|
|2|35845/256|35835/256|20|
|3|140|140|0|
|4|71685/512|71675/512|10|

These dyadic values also fit exact F64 arithmetic for this fixed fixture; this run executed Rational arithmetic in Python, **not** a Haskell/F64 affine derivative test.

Independent fixed matrix A has rows `[0,0,0,0,1]`, `[0,0,1,0,0]`, `[1,0,0,0,0]`; `A^T*[10,20,30]=[30,0,20,0,10]`. For z=[2,-1,3,4,-2], Az=[-2,3,2]; both pairings100. Full vector equality, all five base differences, and matrix/pairing are required; pairing alone is insufficient.

Additional independent quadratic `q(x)=x[4]^2+2*x[2]^2+3*x[0]^2` gives all-five-coordinate differences `[6,0,12,0,10]`; gathered seed `[10,12,6]` must eventually yield that full vector. Exact scratch arithmetic checked this, not a shared enumerator. Future runtime nonlinear checks retain existing host-F64 tolerance policy where arithmetic is not exact.

Fixed fixture rational scope: normalized inputs <=64-bit numerator/denominator, widened products<=128 bits, explicit 15 matrix entries and five central differences per objective. No unbounded Rational AD or rational runtime API is proposed.

## 12. Eventual implementation audit and test map — not executed here

| Seam | Required evidence at implementation review |
|---|---|
| All HostTensor constructors/patterns and tensor shape/dtype/layout/count/ID observers | Count/capacity split everywhere; constructors/copies equal, views retain capacity; existing public signatures/nominal roles unchanged. |
| Logical observers/refinement/transpose/reshape | Signed logical count enumeration, empty zero reads, scalar one, offset retention, conservative contiguity/count precedence. `tensorToList` ordinary allocation/lifetime limitations remain explicit. |
| Primitives/contiguousCopy/closed VJPs | Signed/sliced/offset inputs versus independent materialized logical references; rectangular signed matrix and noncontiguous seed; all old report goldens unchanged; addition sharing unchanged. No tracked map relation inferred from exposed transpose/reshape. |
| SafeTensors encode/decode/raw batch | Encode logical V and 8V, not backing C/prefix; signed/offset/empty round trips become contiguous logical tensors; decode count=capacity; owner/map not serialized; atomic multi-buffer rollback/wire format preserved. |
| New `test/AffineContractTests.hs` | Full constants/prefix sentinels, zero/tiny policy, malformed/full-plan competition, exact/one-below, exact R2 machine/competing-cap wrappers, M edges and independent fixtures. Mixed1024 legacy-empty-constructor/first-affine-pullback fixture with **no private registry observation** and mutation-sensitive execution counters as §9.0. |
| `test-fault/Main.hs` | No reads/allocation on each failed admission; snapshot counters/ID/registry; first allocation/zero/scatter failure; **post-initialization metadata forcing and registry forcing failure before commit**; successful transfer followed by action exception finalized only by session. Cleanup String exception cannot strand lock. Fault hooks private. |
| Existing shared staging tests | Rerun first/second allocation, multi-output rollback, init, registry forcing/preput and afterput, cleanup failure/async retry, all session exits. Include reachable empty-batch/work-only commits and universal spine-invariant mutation. Affine still has one destination. Physical release not guaranteed when callback fails. |
| Public opacity/indices | Reject all three map-index and all five binding-index coercions, constructor access, wrong bind/seed shapes, direct fixed-region escape, and separate continuation-tag misuse. Six scratch coercions here do not cover all eight indices. |
| Positive allowed freedoms | Explicit compatible map rebinding, independent same-shaped seeds, semantic owners sharing storage, existential/closure typeability consistent with temporal precondition. Never execute default post-finalization reads as a lifetime test. |
| Freshness/zeros | New StorageId differs from base/view/seed and prior pullbacks, including empty destination; omitted positive zeros, selected negative zero; pointer equality not a test. |
| Installed unit and archive consumer | Modern explicit intended package unit, isolated package environment, positive import of Affine, individually identified negatives; source imports alone do not prove package exposure. Cabal/snapshot equality and tensor/SafeTensors affected archive closure checks; direct sdist not release admission. |
| Documentation/package scope | Exact §10 lifetime qualifications; F3 mutable snapshot only; no versions/dependency graph/released evidence/status/tape expansion. Historical transpose claims remain bounded. |

No source staging helper, runtime implementation, tests, formatting, package build, archive, installation or freeze is authorized by this scratch task. Future commands must use pinned GHC9.14.1/Cabal3.18.1.0 explicitly and isolated scratch outputs; this document does not claim they ran.
