# D-080 cumulative quotation compilation contract

D-080 is Accepted within its unreleased scope. See the
[decision record](../DECISIONS.md#d-080-add-bounded-first-order-quotation-with-callback-free-let)
for implementation and acceptance evidence. The
[original design record](https://github.com/josephjohncox/Markovian/blob/871f0eedf8a8f460b2b3a5906bec57a365d53e5d/docs/plans/D080-CUMULATIVE-COMPILATION.md)
retains the preliminary review and validation history.

The public declarations, constructor boundaries, roles, and signatures remain
unchanged. Standalone `compileExactPolynomial`, `preflightQuote`, `lowerQuote`,
direct execution preflights, and independent primal/JVP behavior retain their
contracts. Quotation compilation adds cumulative planning and reservations,
derived compiler capacities, and a larger `quoteCompilationPreflight` account.

## 1. Production sequence and report agreement

One `compileExactQuote` invocation must perform:

1. Existing `walkQuote` from `emptyLedger`, with the existing failure order.
2. Metadata-only compiler planning, continuing that ledger.
3. Bulk compiler traversal, allocation, then runtime reservation in that ledger.
4. Force successful checks and construct a private `AdmittedQuoteCompilation` witness.
5. Only in that successful `Either` branch, enter `buildQuote`, then call the unchanged exact compiler with the derived capacity envelope and supplied tape policy.
6. Forward compiler `Left` as `QuoteCompileTargetFailure`. On `Right`, return the executable, the complete quotation reservation report, and the **actual** `exactCompileReport`.

Report agreement is **test-only**. Production does not construct an expected opaque report, inspect its renderer, parse `Show`, compare hidden fields, retry with extra credit, invent a mismatch exception, or call partial `error`. A defensive compiler resource failure after admission is forwarded unchanged and indicates a predictor defect, not permission to enlarge its limits. Semantic ownership errors remain delegated.

No lowered `Program`, `ReverseProgram`, prepared tree, executable or tape may be built to estimate resources. Do not store a virtual target tree either: recursive numeric summaries and running sums suffice. The witness cannot be cached across syntax or exported. A lazy `let plan = ...` without forcing all admission checks before the successful builder branch does not meet this sequence.

Compilation reserves logical representation and structural work for this one compile operation plus the compiler's existing forward/reverse scalar allowances. It does not execute values or reserve unlimited future runs. `runExact` and tape applications remain separate operations.

## 2. Precisely delimited logical units

The new compiler reservation counts **cumulative logical structure construction/reservation**, not maximum reusable workspace, bytes, peak residency, allocator events, instructions, or evaluation timing.

Count these logical objects separately:

- Source/reverse/prepared syntax nodes and primitive-definition/circuit/space/report records.
- Each shape, ownership, finite-layout, parameter-witness, represented zero-value and runtime-product constructor occurrence reserved by a named reconstruction below. Reuse has no discount in a full-reconstruction reservation.
- Each **cons cell** in owner-key, seen-key, cursor-owner, primitive-use, reverse-path and layout-check lists, including copied prefixes and transient lists. Empty-list terminators are canonical and cost zero. These lists are not treated as workspace.
- Each rational coordinate slot in a represented zero vector, even if its list spine is still lazy. Scalar values, owner keys, primitive names/revisions, natural magnitudes and equality-mode tags are atomic. Their bit/character storage is not this unit.
- A string literal/formatting fragment and the result of each description/name concatenation are logical **text fragments**. String contents, character cons cells and the internals of `show` on one atomic name/number are excluded. This does not exempt recursion over the layout/ownership structure used to construct or compare metadata. No claim of a bound on string bytes or character-comparison time follows.

Function closures, machine-sized record fields, `Either`/tuple plumbing, numeric ledger/summary updates, stack frames and arbitrary-precision arithmetic internals are not separately metered objects. Metadata planning uses only a fixed number of scalar fields per recursive result and stack frame; its number of entries is bounded by charged planner visits and virtual-node completions. It neither enumerates coordinate lists nor builds owner/name tables. This explicit administrative exclusion is the same kind of logical-versus-heap distinction as the historical syntax ledger, **not** an exclusion of transient domain lists.

A **full-reconstruction coupon** of size `s` reserves `s` visits and `s` constructor slots, even at a source site that returns retained references or only visits. Coupons are tied to the named source requests in §5; unused slots are not refunded. Counts on successful reports are exact values of this declared reservation policy, not measurements of actual evaluation.

The existing syntax account is preserved literally: `Wq = Tq + Nq + Xq + Aq + Rq`. Its allocation/output units and pre-existing accessor traversals are historical units, not retroactively redefined as the new compiler inventory. In particular, old `quoteOutputShape`, `programOutputShape`, `pathEnvironment`, and `environmentShape` queries may inspect descendants before later child-entry charges. The stronger charged-entry rule below applies to the **new planner**, not to every recursive accessor in the old pass.

## 3. Summaries, equations and exact planner order

### 3.1 Numeric summaries

For a shape, retain extent `E`, layout nodes `L`, and layout depth `H`:

| Shape | E | L | H |
| --- | --- | --- | --- |
| Unit | 0 | 1 | 1 |
| Scalar | 1 | 1 | 1 |
| Vector n | n | 1 | 1 |
| Product l r | E(l)+E(r) | 1+L(l)+L(r) | 1+max(H(l),H(r)) |

These layout counts are exact for the baseline: `productFiniteLayout` preserves products without normalization. The vector's atomic layout is **not** its represented coordinate storage.

For parameters retain the associated layout summary, ownership nodes `O`, ownership depth `Ho`, owner leaves `K`, and cumulative enumeration cells `C`:

| Parameter object p | Layout | O, Ho | K | C |
| --- | --- | --- | --- | --- |
| NoParameters | Unit | 1, 1 | 0 | 0 |
| Owner s | s | 1, 1 | 1 | 1 |
| Product l r | product layouts | 1+O(l)+O(r), 1+max(Ho(l),Ho(r)) | K(l)+K(r) | C(l)+C(r)+K(l) |

`C` is the number of cons cells requested by `parameterOwnerKeys p`, including all left-prefix copies. It is also an upper bound (indeed the same list-construction recurrence when fully demanded) for the keys produced by `checkOwnership p`. Keys need not be known to compute it. `ShareParametersNode` retains the **left** parameter summary; both branches still contribute all node/work/reservation sums.

At every virtual target node `v`, retain parameter/input/output summaries `p,i,o`. Let `B(v)=Lp+Li+Lo`, `U(v)=O+Lp+2Li+Lo`, and `Z(v)=O+B+Ep+Ei+Eo`. `Z` bounds a complete parameter/input/output zero representation: parameter wrappers are bounded by `O`, shape/value constructors by `B`, and coordinate/list slots by the three extents. Overcounting scalar coordinates and unit/owner wrappers is intentional. Retain sums of each §5 expression, not endpoint trees.

Structural virtual-node equations are:

| Node | Parameters | Input | Output |
| --- | --- | --- | --- |
| Compose l r | p(l) × p(r) | i(l) | o(r) |
| Parallel l r | p(l) × p(r) | i(l) × i(r) | o(l) × o(r) |
| Fanout l r | p(l) × p(r) | i(l) | o(l) × o(r) |
| Share l r | p(l) | i(l) × i(r) | o(l) × o(r) |

Program leaves reuse their embedded program inventory; projections contribute one `ProjectValue` primitive. A quotation let has exactly the virtual structure `compose (fanout identity bound) body`, including all three synthetic nodes in **every** inventory and endpoint maximum. Its parameter tree is `(NoParameters × p(bound)) × p(body)`. No unit elimination, reassociation or dead-let discount is allowed.

Retain `N` nodes, `P` primitive occurrences, `D` maximum target depth, `Emax` maximum of all three endpoint extents over all virtual nodes, `Smax` maximum layout/ownership nodes, and `Hmax` maximum layout/ownership depth. No additional public layout-depth limit is introduced. Aggregate parameter extents, including zero-coordinate structural products, cannot be omitted.

For a primitive with parameter/input/output extents `p,i,o` and arithmetic weight `a`, `F=1+p+i+o+a`, `V=1+p+i+o+3a`. Identity: `F=V=1+e`. Each binary node: `F=1+Fl+Fr`, `V=1+Vl+Vr`. Thus let: `F=3+E(environment)+F(bound)+F(body)`, and likewise `V`.

Arithmetic weights: constant scalar/projection/first/second 0; scalar negate/add/multiply 1; vector constant/parameter output extent; vector add/Hadamard/sum n; dot 2n. There is no smooth primitive in this planner. `F,V` and the entire quotation reservation are policy-independent. This follows `Compile.preflightSource`, not a claim that recomputed execution has the same physical cost.

### 3.2 Charged entries and field order

Each entered quotation, embedded `Program`, **Primitive**, lexical `Path`, internal `Projection`, environment witness, shape witness, or explicit parameter witness constructor charges one traversal unit, then total, **before case inspection**. Counting primitive entry separately from `PrimitiveNode` is deliberate. No literal coordinates or owner-string characters are rescanned. Scope tokens, strings and numeric vector lengths are atomic, not recursive entries.

Shape products visit left then right; then check the completed extent, machine before configured coordinate. Atomic shapes check on completion. Environment root visits its stored shape; environment bind visits parent then bound shape, then checks their numeric product. An explicit parameter witness helper visits owner shape or product children in field order and checks its completed layout; current `Program` syntax stores no `SParameters`, so synthesizing `NoParameters`, owner and product summaries creates **no** such source visits.

At **every completed virtual target node**, check parameter, input, then output extent, each machine before coordinate, updating the same maximum. Repeated numeric checks charge no traversal. Numeric synthetic shapes are not source constructors and receive no entered-shape charge; their endpoint checks still occur. No new source/path/quote counts or source/path depths are added by planning.

| Quotation | Exact planner schedule after its entry |
| --- | --- |
| ProgramQuote environment program | Plan program; reuse its input. Do **not** inspect the redundant stored environment witness. |
| ProjectQuote path | Plan path once, returning both endpoints; complete one synthetic primitive (no stored primitive entry). |
| ComposeQuote quoted program | Plan quoted child, then program; complete composition. |
| FanoutQuote l r | Plan l then r; complete fanout. |
| LetQuote token bound body | Plan bound; complete/check environment-shaped identity; complete/check fanout of identity and bound; plan body; complete/check composition. No separate traversal of the token or redundant environment accessor. |

Embedded program: primitive node enters its primitive then its fields below; identity visits its stored shape once; each binary node visits left then right and completes using the table. Shared branches are both entered. Successful target size/depth must agree with the old predicted size/depth in tests; there is no production mismatch exception.

| Stored primitive | Fields inspected in order after primitive entry |
| --- | --- |
| ConstantScalar input value | input shape; value already admitted, no second scan |
| ConstantVector input output value | input shape, output vector shape; no second value scan |
| Parameter owner input output | atomic owner (no identity check), input shape, output shape once; reuse output for its owner layout |
| NegateScalar, AddScalar, MultiplyScalar | no stored shape fields; synthesize scalar/unit/products numerically |
| AddVector, Hadamard, Dot, SumVector | the single stored vector shape once; reuse it |
| First left right, Second left right | left shape then right shape once each |
| ProjectValue projection | charged projection helper returning both endpoints together |

| Path / Projection | Exact field order and endpoint derivation |
| --- | --- |
| PathHere shape | visit shape once; input=selected=that summary |
| PathLeft inner bound | plan inner, visit bound; input=product(inner input,bound); selected=inner selected |
| PathRight environment bound | visit environment, visit bound; input=product(environment,bound); selected=bound |
| ProjectionHere shape | visit shape once; input=selected=shape |
| ProjectionLeft inner right | plan inner, visit right; input=product(inner input,right); selected=inner selected |
| ProjectionRight left inner | visit left, plan inner; input=product(left,inner input); selected=inner selected |

Each completed path/projection checks input then selected extent. Environment, shape and explicit parameter helper checks precede those endpoint checks. Do not call `projectionInputShape` then `projectionOutputShape`, build replacement projections, or reconstruct shapes to rediscover these summaries.

### 3.3 Private planner metadata

The private summary types and helper signatures are defined in
[`Quote.hs`](../../packages/markovian-autodiff/src/Markovian/Autodiff/Quote.hs).
Their fields and the intermediate scalar summaries stay private.

`depthSum` is the sum of one-based relative node depths. Leaf=1; binary=`1+depthSum(l)+nodes(l)+depthSum(r)+nodes(r)`. Therefore `pathCellSum=depthSum-nodes`. `quoteEntries` counts quotation entries only (embedded program summaries carry zero); it supplies the builder bound below. The metadata/ownership/comparison sums are exactly the §5 recurrences evaluated bottom-up. No list of nodes is retained. `builderReservation` is additive across quotation children plus the local §5 `G` term; embedded programs contribute zero. Helper results may use additional fixed-size scalar tuples, never an owner table or shape/target tree.

## 4. Bounded arithmetic and exact failure precedence

For each additive bulk dimension separately:

1. Maintain `used <= limit`; compute `remaining = limit-used`.
2. Evaluate **that dimension's** reservation expression with nonnegative addition/multiplication capped at `remaining+1`.
3. If amount exceeds remaining, return that dimension's `(limit,limit+1)` error immediately.
4. Otherwise the amount is exact; charge total with this **exact** amount, using the existing saturated total error.

Never cap a dimension's expression at `min(remainingDimension,remainingTotal)`. Never cap a shared summary at one later consumer's smaller capacity. Summary values may remain exact `Natural`: they are computed by a fixed number of operations per entered constructor/virtual node, with additions/multiplications of bounded-degree structural counts, not loops proportional to extents. `C <= K*(O+1)`, all sums range over at most `N` nodes, and no exponent depends on input data. Such integers have bit size bounded by a constant multiple of the bit sizes of admitted visit counts/extents. This permits exact shared summaries without allocating coordinate arrays or huge exponential integers. Final dimension-specific evaluation is still capped, using multiplication's quotient guard before multiplication. The triangular polynomial `k*(k-1)/2` uses an even-factor division first, with k=0 handled as zero before natural subtraction.

Extents are different: compute the actual numeric product extent bottom-up, check machine then coordinate, and retain actual error payloads. No `Int` conversion occurs before admission. `F,V` retained as exact summaries are supplied only after successful runtime admission.

The old syntax pass has absolute precedence. New planner checks run in §3 order. Bulk order after planning is `Tc` traversal then total; `Ac` allocation then total; `Rc` runtime then total. No successful report or executable escapes a failure.

| Failure stage | Exact result / precedence |
| --- | --- |
| Any old syntax failure | unchanged `QuoteError`, wrapped in `QuoteCompilePreflightFailure`; no new planner entry |
| New constructor-entry exhaustion | `QuoteTraversalLimitExceeded limit (limit+1)` before `QuoteTotalWorkLimitExceeded limit (limit+1)` |
| Shape/environment/path/virtual endpoint extent | `QuoteMachineExtentExceeded actual` before `QuoteCoordinateExtentLimitExceeded limit actual` |
| Bulk Tc | traversal saturated error before its total saturated error |
| Bulk Ac | allocation saturated error before its total saturated error |
| Bulk Rc | runtime saturated error before its total saturated error |
| Sufficient reservation then compiler semantic error | unchanged `QuoteCompileTargetFailure`; no output or report |
| Sufficient reservation then defensive compiler capacity/work error | same target wrapper; no retry, refund, mismatch exception or extra credit |
| Successful compiler | complete reservation report and actual compiler report; no runtime tape |
| Later direct or compiled execution failure | existing execution context and order, separate operation |

All new pre-build errors in the middle rows are wrapped in `QuoteCompilePreflightFailure`. Resource failure therefore precedes duplicate-owner detection; with sufficient resources the existing reverse error and ordering apply. `prepareNode` still admits node/depth, resolves/checks primitive definition/ownership or prepares left then right children, then checks join compatibility/ownership. No owner-collision dry run is introduced. Finite, defined, well-typed syntax is the domain; this is not a termination guarantee for host-language bottoms.

## 5. Source-backed cumulative construction bound

Source abbreviations (all baseline files):

- **Q**: `packages/markovian-autodiff/src/Markovian/Autodiff/Quote.hs`, especially 414–618, 733–811.
- **C**: `packages/markovian-autodiff/src/Markovian/Autodiff/Compile.hs`, especially 255–373, 534–568, 663–862, 889–915.
- **R**: `packages/markovian-reverse/src/Markovian/Reverse/Program/Internal.hs`, especially 132–135, 486–787.
- **L**: `packages/markovian-reverse/src/Markovian/Reverse.hs`, especially 60–134, 143–199, 248–252.

### 5.1 Non-list metadata coupons at each virtual node

The following table **defines** `MT(v)` and `MA(v)` as the column sums. A row stated as a full coupon reserves both visits and construction slots, even when baseline code reuses references. Applying leaf-only requests at every virtual node deliberately gives a uniform, source-derived upper envelope; it is not a claim that every row executes at every node.

| Named requests and source coverage | Traversal coupon | Allocation coupon |
| --- | --- | --- |
| Seven endpoint-summary reconstruction rounds: resolver parameters/input/output; root executable parameters/input/output; source preflight's first parameter check, input check, output check, second parameter check, arithmetic shape query (C:286–300,534–543,663–730,766–789) | 7U | 7U |
| Initial primal and cotangent layout construction, independently for all endpoints (C:740–764,812–841; R product spaces) | 2B | 2B |
| Initial ownership skeleton and owner-leaf layouts (C:759–764) | O+Lp | O+Lp |
| Primal and cotangent equality-mode recursion (C:843–859) | 2(O+B) | 2(O+B) |
| One normal owner-description round and two error-description rounds over ownership/endpoints (C:835–841; R:623–627,717–719; L descriptions) | 24(O+B) | 24(O+B) |
| Layout materialization specifically for those three description rounds | 3B | 3B |
| Complete zero-value constructors and their unwrapped representation, parameters/input/output (C:792–810,824–833,892–896,912–915; R:759–773) | 2Z | 2Z |
| Six primal/cotangent space records, six layout/owner `Maybe` fields, circuit, owned definition, two recomputation-wrapper slots, two primitive-name fragments | 18 | 18 |
| `checkNodeSpaces` endpoint layout checks/rebuilds (R:695–718; L:108–134) | 2B | 2B |
| Extra primitive parameter layout check and ownership-leaf checks/rebuilds (R:540–541,668–697) | 2Lp | 2Lp |
| Layout-check lists and metadata/result pairs, explained below (R:695–718) | 42 | 30 |

Why these capacities suffice:

- `U` includes parameter ownership and layout, and **two input-layout inventories**. For an internal `Projection`, each spine step contributes a product to its input layout, so each of the separate input/output accessor walks is bounded by `Li`, even when the selected output is scalar. Returning a stored shape costs no reconstruction; reserving its full inventory is conservative. The seven named rounds cover every compiler endpoint query, including the three root `program*` traversals' leaf work. The **program-constructor** visits for those root queries are separately charged below, not hidden in ownership comparisons.
- Mode traversal has one call for each primal and each cotangent endpoint. Exact mode combination is atomic; approximate mode construction is not reachable in this Rational contract.
- Description factor 24 is **three rounds times eight fragments per contributing structural node**. A product has three delimiters and four concatenation results (seven); a scalar/vector layout description has two delimiters, one numeric-format fragment and three concatenations (six), plus its caller's prefix and concatenation (two). Ownership unit/owner prefixes fit the same bound with the ownership nodes counted separately. Two extra rounds cover construction of descriptions for both endpoints of a failed join, including layout descriptions inside `describeCotangent`; public report rendering is a separate observer, not compilation. Contents of names and strings remain atomic as §2 states. Structural equality is charged separately below.
- Full zero reservation includes each represented vector coordinate even though `CotangentSpaceWitness.spaceZero` is strict only to weak-head normal form. `VectorValue` is list-backed; `unwrapValue` can retain its spine, and a product runtime zero can leave components lazy. Therefore neither "all coordinates eagerly allocated at compile time" nor "one atomic vector layout pays for all zero coordinates" is valid. The two rounds reserve construction and unwrapping; later arithmetic outputs and tapes belong to execution, not this inventory.
- At most three primal and three cotangent entries feed `checkNodeSpaces`. Its two input lists have six cells; `traverse` produces three result lists of length at most three; two `map snd` lists have six cells: **21 constructed cons cells**. Cotangent metadata contributes three pairs and checked layouts six pairs: **nine** more objects, total allocation 30. Construction of the six input-list cells plus nine traverse visits, six `firstOver` visits, six map visits, six maximum-fold visits and nine pair operations is traversal 42. Early failures only shorten these bounds.

### 5.2 List, comparison, source and builder terms

For each virtual node let `k=K(p(v))`, `c=C(p(v))`, `o=O(p(v))`, and `tri(k)=k*(k-1)/2` (zero at k=0). Define:

- `OT(v)=2*(o+c+k+tri(k))`.
- `OA(v)=2*(o+c+k)`.

These reserve two ownership/enumeration rounds, two fully consumed key lists, and two duplicate scans/seen lists. Independent joins use one `parameterOwnerKeys l ++ parameterOwnerKeys r` and one `checkOwnership` keys result; `c` is exactly the whole product's enumeration recurrence for **each**. Primitive nodes use at most one of each and own at most one key. Identity/share nodes may use less but receive the same coupon. The `o` construction slots also cover ownership-check result product layouts; owner-leaf layout checking is paid by `2Lp` above. Duplicate detection visits at most k incoming cells and compares at most `tri(k)` prior keys, building at most k seen cells. Nothing depends on deduplicating strings in the planner.

For the j-th primitive in left-to-right preparation order, `1 <= j <= P`, reserve:

- `CT(j)=4j+1`: up to j owner membership cells, j owner append/copy cells, j cells for `length owners`, one incoming owner-key fold step, j primitive-use search cells.
- `CA(j)=2j+1`: up to j cursor-owner cons cells, j primitive-use cons cells, and one new/updated `PrimitiveUse` record.

`addUnique` copies its entire existing list on insertion; `addPrimitiveUse` rebuilds the prefix even on updating an existing entry. Consequently final inventory length is **not** their cumulative allocation bound. Closed primitives have zero or one owner; before primitive j both cursor lists have at most j-1 entries. Summation gives `sum CT=2P(P+1)+P`, `sum CA=P(P+1)+P`. Logical string comparisons use atomic keys/revisions; no cost is attributed to their character lengths.

Let `J=sum over binary v of (U(left(v))+U(right(v)))`. Reserve `CM=3J+4N` for both traversal and allocation: separate full coupons for primal-layout comparison, cotangent-layout comparison, and shared-ownership equality (including owner-leaf layouts); plus four atomic owner/equality-mode comparisons per node. For composition, the large intermediate endpoint belongs to a **child**, even if neither parent endpoint is large. This is why parent endpoints alone cannot bound compatibility. Full coupons also reserve comparison storage, although baseline equality often returns a constant without reconstruction.

**Primitive ownership-layout equality:** separately reserve `EQp = sum over primitive v of Lp(v)` in **both** traversal and allocation. At every `PrimitiveProgram`, R:540–543 first obtains `ownershipLayoutChecked` through `checkOwnership`, then obtains `parameterLayout` through `checkOneLayout`, then evaluates `ownershipLayoutChecked /= parameterLayout`. `FiniteLayout` derives structural equality (L:60–64); one comparison visits at most Lp constructor pairs, with atomic fields compared at each visited constructor. Both layouts have the same predicted parameter shape for this closed language. Early mismatch visits only a prefix. The separate full-reconstruction coupon intentionally over-reserves equality's construction, without claiming it constructs a layout. The `2Lp` metadata row covers the **checks/rebuilds**, not this equality; `CM` covers binary and atomic comparisons, not it. No unnamed spare capacity is borrowed. Every synthetic `ProjectQuote` primitive and every embedded primitive contributes its parameter layout (including Unit); identity contributes zero. `primitiveLayoutComparisonSum` is Lp at a primitive, zero at identity, and additive at binary nodes, including all virtual let nodes.

Let `Hpath=sum_v (depth(v)-1)`. Every child call builds `path ++ [step]`, requesting exactly its one-based depth minus one cons cells, including the new singleton and copied parent prefix. Charge `Hpath` to traversal and allocation. Root path is empty.

Let q be quotation entries. Define the builder coupon:

`G = q + sum(ProjectQuote v)(1+2Li(v)) + sum(LetQuote b body)(q(b)+2Li(b))`.

Here q(b) is quotation entries in the **bound** subtree, excluding embedded program constructors. The first term covers `buildQuote` dispatch. At a projection, one primitive payload and two full input-layout-sized passes cover `pathProjection` plus any `environmentShape` reconstruction. A path spine is bounded by input layout nodes; at a `PathRight` the preceding environment witness also contributes to that same input layout. At a let, chasing `quoteEnvironment` down the bound visits at most q(b) quotation nodes; `pathEnvironment` (if selected) and `environmentShape` each have at most Li(b) constructors. A redundant `ProgramQuote` environment has that same indexed input shape, even though the planner does not inspect it. G is reserved in both dimensions. Generated source `Program` nodes themselves remain paid exactly once by the old `Xq`/target allocation charges; embedded programs are reused.

### 5.3 Final compiler-stage formula and proof

Freeze:

- `Tc = 6N + G + sum_v MT(v) + sum_v OT(v) + sum_j CT(j) + Hpath + CM + EQp`.
- `Ac = 3N + P + 5 + G + sum_v MA(v) + sum_v OA(v) + sum_j CA(j) + Hpath + CM + EQp`.
- `Rc = F+V`.

`6N` consists of three target passes (reverse lowering, preparation, source-work inspection) plus three separate root-witness source traversals (`programParameters`, `programInput`, `programOutput`). Each root traversal enters at most N program constructors. Their endpoint/projection work is paid in the seven reconstruction rounds, not assumed constant.

`3N` reserves reverse syntax, prepared nodes, and prepared forms; `P` reserves `TargetPrimitive` wrappers. Five top-level objects reserve backend, exact executable, compiler report, reverse report and prepared-program wrapper. Primitive definitions/circuits/spaces/recomputation wrappers are in the per-node table. Numeric cursors and quotation report/admission summary records are administrative metadata under §2, but **their domain lists are fully charged above**. Compilation constructs no runtime tape or dense evaluated environment/output; the builder environment **witnesses** and cotangent zero representations are covered, not mistaken for evaluated values.

Proof by structural induction: shape and parameter recurrences bound their literal constructor inventories; the enumeration recurrence follows `(++)` copying exactly its left argument. Every virtual-node combination retains children sums and adds its own table, ownership and comparison coupons; synthetic let nodes use the same rule. Each actual compiler metadata request is assigned a named coupon above, including the prepared primitive's checked-ownership/parameter-layout equality in EQp independently of the two layout checks; retained references use no more than full reconstruction. Each independent-owner list request has the matching `C`, every duplicate scan is bounded by `tri(k)`, every cursor update by its primitive ordinal, every prepared path by its depth. Three source and three root traversals cover the non-metadata compiler passes. Q's builder cases are exhausted by G plus the already-paid generated source nodes. Semantic failure is a prefix of these bounded requests; error descriptions have their own rounds. Thus the cumulative logical objects/visits of the delimited model fit Tc/Ac, for either policy, without a workspace reinterpretation or assuming physical sharing. This proof does not bound excluded machine/character costs.

**Left-associated-owner discriminator.** For k distinct scalar-parameter primitives composed left-associatively, `N=2k-1`, `P=k`, and the parameter tree at join j has `Cj=j(j+1)/2`. One enumeration at every join sums to `k(k+1)(k+2)/6-1`. At k=128 this is **357759** cons cells, exceeding the rejected old entire allocation bound **268131**. The new OA contains **two** complete copies of that sum before seen lists, cursors, paths or metadata are added. Include the k=128 case as a bookkeeping test with actual production admission, not merely a test of a detached polynomial. k=1,2,3 and balanced trees distinguish recurrence mistakes and association erasure.

### 5.4 Single product-owner equality discriminator and exact budgets

For `k=2^d`, define balanced shape `S0=Scalar`, `S(d+1)=Product Sd Sd`. Quote exactly `quoteProgram (parameter @"product-owner" Unit Sd)`. There is **one primitive and no binary target node**, irrespective of k; parameters are one owned product value, not a product of scalar owners. This fixture is expressible through public shape/parameter constructors; the external `ProductOwner.hs` compiles it for k=1,2,4,128 under both baseline policies.

Let `h=2k-1`. Then `N=P=D=q=1`, `Lp=Lo=h`, `Li=1`, `O=K=C=1`, `B=2h+1`, `U=2h+3`, `Z=2h+2+2k`, `G=1`, `J=Hpath=0`, `CM=4`, and **`EQp=h`**. Structural depth capacity is d+1, not target depth 1. `Emax=k`, `Smax=h`, `F=1+3k`, `V=1+5k`.

Adding the named table rows gives `MT=87h+145+4k`, `MA=MT-12`; `OT=OA=6`, `CT=5`, `CA=3`. Hence `Tc=180k+79`, `Ac=180k+68`, `Rc=2+8k`. The old syntax account is `Tq=2`, `Nq=1`, `Xq=0`, `Aq=1+3k`, `Rq=1+5k`, `Wq=5+8k`. The extra 1 in Aq is the existing `chargeTargetNodes` allocation, not a new coupon. Planner entries are quotation, Program, Primitive, input Unit and h output-shape constructors: `Tp=4+h=2k+3`. Redundant environment and the reused parameter/output shape are not revisited.

Complete report work fields are therefore `T=182k+84`, `A=183k+69`, `R=13k+3`, `W=378k+157`. Other fields: quotation nodes 1, source depth 2, path depth 0, target nodes/depth 1, transformed 0, maximum extent k, source bits 0. Compiler limits are `compilerLimits 1 1 1 1 k k h (d+1) (1+5k) callerBits`.

| k | EQp | Tp | Tc | Ac | Rc | complete T | complete A | complete R | complete W |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | 1 | 5 | 259 | 248 | 10 | 266 | 252 | 16 | 535 |
| 2 | 3 | 7 | 439 | 428 | 18 | 448 | 435 | 29 | 913 |
| 4 | 7 | 11 | 799 | 788 | 34 | 812 | 801 | 55 | 1669 |
| 128 | 255 | 259 | 23119 | 23108 | 1026 | 23380 | 23493 | 1667 | 48541 |

These table values are independent arithmetic oracles. For each row and both policies, the production test must succeed with all four complete additive budgets exact. Separately set each complete budget one below, leaving other budgets generous: `QuoteTraversalLimitExceeded (T-1) T`, `QuoteAllocationLimitExceeded (A-1) A`, `QuoteRuntimeWorkLimitExceeded (R-1) R`, and `QuoteTotalWorkLimitExceeded (W-1) W`, all inside `QuoteCompilePreflightFailure`, before any builder/compiler call. At k=128 these are `(23379,23380)`, `(23492,23493)`, `(1666,1667)`, `(48540,48541)` respectively. Total one-below fails at Rc's total charge; individual one-below tests fail at their bulk dimension, not in old syntax or planner visits.

Also admit traversal `T-h=23125` or allocation `A-h=23238` separately, other limits generous: each must reject with its saturated `(limit,limit+1)` error. A mutation omitting EQp would wrongly succeed. Omitting EQp subtracts h from both complete T and A, and 2h from W, while leaving R unchanged; the one-below tests alone also detect that mutation. This discriminator isolates the previously unnamed structural equality without claiming the old aggregate bound was numerically disproved by it.

## 6. Derived capacities and complete successful report

Only after successful cumulative admission, construct exactly:

`compilerLimits N P D P Emax Emax Smax Hmax (max F V) (rationalBitLimit limits)`.

These are paid-for per-structure capacities, not fresh additive allocation/traversal budgets. Unique owners are bounded by primitive occurrences, without deduplication. Layout/ownership depth is **Hmax**, not target depth. Compiler bits are the **caller bit limit**, never the maximum observed source literal bits. A literal-free program can have source bits zero and must still execute nonzero supplied values within the caller allowance.

Let Tp be new planner entry charges and Q be the unchanged syntax report. The complete quotation compilation report is:

| Field | Prospective compile value |
| --- | --- |
| quoteNodeCount, quoteSourceDepth, quoteMaximumPathDepth | unchanged Q values |
| quotePredictedTargetNodes, quotePredictedTargetDepth | unchanged Nq=N and D |
| quoteTransformedNodes | unchanged Xq; reverse/prepared nodes are not new quotation transformations |
| quoteMaximumCoordinateExtent | max(Q maximum, Emax) |
| quoteMaximumRationalBits | unchanged admitted source-literal maximum |
| quoteTraversalWork | Tq+Tp+Tc |
| quoteAllocationCount | Aq+Ac |
| quoteRuntimeWork | Rq+F+V |
| quoteTotalWork | Tq+Tp+Tc+Nq+Xq+Aq+Ac+Rq+F+V |

The old Rq compiler-oriented charges remain; Rc is an additional compiler-stage allowance, not a claim that arithmetic ran twice. No refund or partial successful report occurs on target failure. `quoteCompilationTarget` is the actual unchanged compiler report. The complete quotation report must be equal across both policies for the same syntax and limits. Test-only target-renderer goldens must check N/P/D, F/V, ownership tree, extents and constructor inventory, plus the expected stored/recomputed counts and primitive-use policy changes. Do not assert that target reports differ in only the top-level policy line.

## 7. Independent differential fixture and literal parameter tree

### 7.1 Approved construction and literal parameter tree

Use input `I = Vector 2 × (Scalar × Vector 2)` with coordinates `(x,(s,z))` and output **`Product (Product Scalar Scalar) (Product Scalar Scalar)`**, flattened `(v0,v1,w,t)`. There is no scalar-to-vector pack/broadcast, new primitive, or identification of Vector with Product.

The following recipe specifies `QuoteCompilationFixture.hs`. All abbreviations expand at each use: host-language reuse is not an extra quotation let, parameter sharing or tree normalization. Let `0` below mean **`noParameters`**, not scalar zero; `(p,q)` means literal `parameterProduct p q`. The owner leaves are `a=ownedParameters (vectorValue [2,3])`, `b=ownedParameters (scalarValue 5)`, `c=ownedParameters (vectorValue [7,11])`; vector values use the public checked length-2 constructor. Direction/gradient trees replace only these owned values, retaining every zero leaf and product.

Bind in exact lexical order, with fresh scopes `su,sb,sc`:

1. At root environment E0, `X0 = composeQuote (project (pathHere I)) (first Vector2 (Scalar × Vector2))`. Bind `u = composeQuote (fanoutQuote (quoteProgramAt E0 (parameter @a I Vector2)) X0) (hadamard Vector2)`, tree `U=((a,(0,0)),0)`. E1 has shape `I × Vector2`.
2. Bind `quoteProgramAt E1 (parameter @b (I × Vector2) Scalar)`, tree `b`. **Owner b occurs once.** E2 has shape `(I × Vector2) × Scalar`.
3. Bind `quoteProgramAt E2 (parameter @c ((I × Vector2) × Scalar) Vector2)`, tree `c`. Its evaluated value is unused. E3 has shape `((I × Vector2) × Scalar) × Vector2`.
4. Compute the following body in E3; no further quotation binding is introduced.

The root path is `pathLeft sc (pathLeft sb (pathLeft su (pathHere I) Vector2) Scalar) Vector2`. The u path is `pathLeft sc (pathLeft sb (pathRight su E0 Vector2) Scalar) Vector2`; the b path is `pathLeft sc (pathRight sb E1 Scalar) Vector2`. `root`, `readU`, `readB` each mean one `project` of that exact path, tree 0. Define:

- `X = composeQuote root (first Vector2 (Scalar × Vector2))`, tree `Xtree=(0,0)`.
- `S = composeQuote root (compose (second Vector2 (Scalar × Vector2)) (first Scalar Vector2))` and `Z = composeQuote root (compose (second Vector2 (Scalar × Vector2)) (second Scalar Vector2))`, both tree `SZtree=(0,(0,0))`.
- `coord j q = composeQuote q (compose (fanout (identity Vector2) (constantVector Vector2 Vector2 mask_j)) (dot Vector2))`, with literal one-hot vectors `mask_0=[1,0]`, `mask_1=[0,1]`. Its tree is `C(p)=(p,((0,0),0))` when q has tree p. This uses only vector-to-scalar dot extraction.
- `add l r = composeQuote (fanoutQuote l r) addScalar`; `mul l r` substitutes `multiplyScalar`. Both have tree `A(p,q)=((p,q),0)`.
- `v_j = add (coord j readU) (mul readB (coord j Z))`, tree `V=A(C(0),A(0,C(SZtree)))`.
- `w = add (composeQuote readU (sumVector Vector2)) (mul S readB)`, tree `W=A((0,0),A(SZtree,0))`.
- `t = add (mul v_0 (coord 0 X)) (mul v_1 (coord 1 X))`, tree `T=A(A(V,C(Xtree)),A(V,C(Xtree)))`. The v expressions expand again, but contain only lexical reads, not additional owners.
- Body is `fanoutQuote (fanoutQuote v_0 v_1) (fanoutQuote w t)`, tree `B=((V,V),(W,T))`.

The complete quotation is `letQuote su u (letQuote sb b (letQuote sc c body))`. Its literal parameter tree is `((0,U),((0,b),((0,c),B)))`. For avoidance of any omitted unit suffix, the **entire expanded tree** is:

```text
((0,((a,(0,0)),0)),((0,b),((0,c),(((((0,((0,0),0)),((0,((0,(0,0)),((0,0),0))),0)),0),(((0,((0,0),0)),((0,((0,(0,0)),((0,0),0))),0)),0)),((((0,0),(((0,(0,0)),0),0)),0),(((((((0,((0,0),0)),((0,((0,(0,0)),((0,0),0))),0)),0),((0,0),((0,0),0))),0),(((((0,((0,0),0)),((0,((0,(0,0)),((0,0),0))),0)),0),((0,0),((0,0),0))),0)),0))))))
```

This has **79 NoParameters leaves, three owner leaves in a/b/c order, and 81 ParameterProduct nodes**. The external Python expansion renders the full public `ParameterValue` Show form and checks it byte-for-byte against the compiled client's independently spelled, typechecked parameter value. Successful compilation and execution additionally check that this tree actually fits the quotation, rather than merely flattening to five numbers. The baseline target renderer independently records that entire ownership association and `owners: ["a","b","c"]`.

### 7.2 Mathematical target and every coordinate

The numerical target remains `u=a⊙x`, `v_i=u_i+b*z_i`, `w=sum u+s*b`, `t=v0*x0+v1*x1`; the approved output is `((v0,v1),(w,t)) :: Product (Product Scalar Scalar) (Product Scalar Scalar)`, not a Vector. Flatten parameters then input in order `(a0,a1,b,c0,c1,x0,x1,s,z0,z1)`. All two unused c columns are retained.

Independent formal differentials:

- `dv_i=x_i da_i+a_i dx_i+z_i db+b dz_i`.
- `dw=sum_i(x_i da_i+a_i dx_i)+s db+b ds`.
- `dt=sum_i[x_i^2 da_i+(2a_i*x_i+b*z_i) dx_i+b*x_i dz_i]+sum_i(z_i*x_i) db`.

At `a=(2,3), b=5, c=(7,11), x=(13,17), s=19, z=(23,29)`, output is `(141,196,172,5165)`. The full four-by-ten Jacobian rows are:

| Seed row | a0 | a1 | b | c0 | c1 | x0 | x1 | s | z0 | z1 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| v0 | 13 | 0 | 23 | 0 | 0 | 2 | 0 | 0 | 5 | 0 |
| v1 | 0 | 17 | 29 | 0 | 0 | 0 | 3 | 0 | 0 | 5 |
| w | 13 | 17 | 19 | 0 | 0 | 2 | 3 | 5 | 0 | 0 |
| t | 169 | 289 | 792 | 0 | 0 | 167 | 247 | 0 | 65 | 85 |

For **both policies**, independently check all ten combined parameter/input basis columns with direct JVP, all four output-basis rows with VJP, every one of the forty pairings, both primals, zero and nontrivial mixed directions/seeds, and at least two seeds on one retained tape. Repeat at fixed zero/negative/non-unit-denominator points. Formal expectations must use these handwritten polynomial formulae, not production interpreters, lowering, primitive VJPs, tapes or shared derivative helpers. Typed values and a checked flattening/pairing helper may be shared. Actual `ParameterValue` tree assertions must accompany flattened comparisons, using the fully resolved §7.1 tree. The external baseline client already checks the complete parameter-gradient tree on every tested seed; these checks must be retained in the later implementation suite.

### 7.3 Executable fixture

[`QuoteCompilationFixture.hs`](../../packages/markovian-autodiff/test/QuoteCompilationFixture.hs)
defines the public-library fixture; the package tests compare its direct primal,
JVP, compiled primal, and retained-tape VJP with independent expected values.
The mathematical oracle and complete parameter tree above specify the comparison.

## 8. Required implementation tests

- Independently calculate small projection, identity, square, used-let and unused-let reports. For each additive dimension and total: exact complete budget succeeds, one below returns the exact saturated error, other limits generous. Syntax-only exact budgets are not compile budgets anymore.
- Reset-account discriminator: limit at least each stage's separate demand but less than their sum; `preflightQuote` and `lowerQuote` succeed, compilation fails before builders. Reject post-hoc report addition and final-total-only accounting.
- Failure competition: old syntax before planner; planner traversal before total; machine before coordinate; traversal/allocation/runtime reservations before their own total; bound and synthetic identity/fanout before body; insufficient resources before duplicate independent owners, and duplicate owners after sufficient reservation.
- Requote a previously lowered nested path so the embedded program contains `ProjectValue (ProjectionLeft/ProjectionRight ...)`; fix both endpoint and entered-Projection counts independently. Direct `ProjectQuote` alone is not coverage.
- Single balanced-product owner primitive at k=1,2,4,128 with the §5.4 independently calculated complete reports, exact/one-below and no-EQp discriminators; both policies and real pre-build admission events. This has no binary nodes and prevents CM from hiding the missing layout equality.
- Left-associated distinct owners including k=128, balanced association, `ShareParameters` with two branches but one endpoint owner tree, many NoParameters leaves, zero-length vectors, and shallow identity with deep product shape.
- Aggregate-parameter failure: compose k scalar-parameter-to-scalar primitives; syntax sees individual owner extents 1, planner checks aggregate k. Coordinate limit k-1 rejects only compilation. A machine-extent variant can use shallow owners with huge vector dimensions individually machine-admissible whose combined parameter extent is not; include the old syntax's output/runtime allowances generously and specify the resulting shapes before coding it.
- Literal-free identity/projection compiled with caller bits 8, observed literal bits 0, then nonzero runtime input 13 and seed 1. Both policies must succeed; passing observed bits instead would fail. No input literals are stored in quotation syntax.
- Equal complete reservation reports across policies; actual target-report policy lines, inventory and work checked using test-only renderer goldens.
- Nested equal-shaped lexical root/outer/newest selections returned together with different values and tangents; used and unused variants. Complete primitive coverage includes constants, product-shaped owners, vector lengths 0/1/2, first/second, parallel/fanout/compose/share and actual quotation wrappers. Empty coordinate cases are shape/primal evidence, not nonvacuous Jacobian evidence.
- Preserve unused-bound rational failure before body/sibling; separate primal overflow from JVP-only intermediate overflow. Test compiled forward behavior under both policies, with each backend's actual error context rather than invented cross-backend context equality.

## 9. Private production-path probe requirements

The `packages/markovian-autodiff/scripts/check-autodiff-boundary` harness compiles `test/QuoteCompilationPrivate.hs` with the private probe support module from the source distribution. The required instrumentation contract follows.

Freeze the build mechanism: the boundary script creates an isolated temporary output directory, compiles **the actual checkout** autodiff source and reverse source as home modules, with `-i<autodiff>/src -i<reverse>/src -i<autodiff>/test`, `-hide-all-packages -package base`, the script's isolated package DB flags, `-Wall -Werror -XGHC2021 -O0 -fforce-recomp -fno-cse -fno-full-laziness -cpp -DD080_PRIVATE_PROBE`, and an explicit `-outputdir`/executable path. Do not simultaneously expose the installed autodiff/reverse units to this probe. The normal opacity checks continue to use the exact installed unit. Production source has private CPP-gated counters with no changed public exports; a test-only IORef event sink from base may instrument pure entries under the probe flag. Normal builds contain neither sink nor hooks. Archive-only execution must compile the same local source paths.

Probe phases/events:

1. Record actual syntax-admission start/success, planner start/success, traversal/allocation/runtime reservation success, and witness construction in the **real** `preflightQuoteCompilation` path. Do not duplicate the preflight in the probe.
2. Count `buildQuote`, `buildPath`, `pathProjection`, and source constructor RHS entry (generated Program versus projection/environment/shape witnesses separately). Instrument the call edge **before** calling the builder, as well as constructor demand sites, to distinguish a lazy unevaluated invocation from forcing its result.
3. Count `compileExactPolynomial` call edge, `lower`, `prepareReverseProgram`, and prepared/space/zero-witness constructors in their real implementations. Cotangent witness construction is not tape construction.
4. Count runtime evaluated environment/output and `ReverseTape` constructor sites separately; deliberately call `runExact` only in the later positive phase.

On syntax/planner/reservation failure: no builder-call, target-compiler-call, target/prepared constructor or tape events. On sufficient admission followed by duplicate-owner failure: builder/compiler/preparation events positive, no successful executable and no tape. On successful compilation without run: witness/builder/compiler/prepared events positive, tape zero. Subsequent successful run: tape positive. Force reports and selected constructors explicitly; reset counters between cases and force setup syntax before reset. Do not count building the test's input syntax as compiler work.

Controls required: positive successful compile and run; negative each reservation failure; a poisoned later child for selected planner order; mutation moving builder/compiler call before reservation must fail the boundary probe even if its result stays lazy; bypassing a charge or resetting the ledger must fail budget tests; disabling an event site must fail its positive control; swapping Projection field order and truncating shared summaries to allocation capacity must fail exact/competing-limit tests. Compile and run normal builds too, proving CPP instrumentation does not change the public export set. No public callback injection is permitted.

These events establish production sequencing and selected **logical constructor-entry** coverage. They are not physical allocator instrumentation, byte counts, or proof of zero Haskell allocations during preflight. Compiler optimization/laziness changes real heap behavior; no such measurement is asserted.
