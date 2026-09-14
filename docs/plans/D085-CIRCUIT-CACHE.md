# D085: retained exact local circuit tables

Status: implemented and locally verified; decision acceptance is tracked in D-085. Consumer: `Markovian.Open.Acyclic.Circuit.Exact`.

The existing `ExactPrimitiveInterpreter` accepts arbitrary callbacks and has no
Rational/work meter. D085 therefore adds a closed table-primitive interpreter
with its own checked source algorithm; it does not retrofit a resource promise
onto those callbacks. Existing exact interpretation remains available as a
literal-denotation oracle. A second, explicitly uncached route executes the new
checked source algorithm and is the admission/error-order reference.

## Public boundary

The following names are exported by the existing consumer module. The engine
lives in its private `Cache.Internal` module.

```haskell
data ExactTablePrimitive (p :: Purity) a b -- opaque, nominal
exactDeterministicPrimitive :: DeterministicMatrix NonNegativeRational a b
  -> ExactTablePrimitive 'Deterministic a b
exactStochasticPrimitive :: StochasticMatrix NonNegativeRational a b
  -> ExactTablePrimitive 'Stochastic a b
exactTableInterpreter :: ExactPrimitiveInterpreter ExactTablePrimitive ()

data ExactTableInterpreterIdentity = ExactTableInterpreterV1

data CircuitSemanticLimits = CircuitSemanticLimits
  { circuitNodeLimit :: Natural, circuitCellLimit :: Natural
  , circuitWorkLimit :: Natural, circuitPrimitiveCellLimit :: Natural
  , circuitNumeratorBitLimit :: Natural, circuitDenominatorBitLimit :: Natural }
data CircuitCacheLimits = CircuitCacheLimits
  { cacheEntryLimit :: Natural, cacheCellLimit :: Natural
  , cacheTraceLimit :: Natural, cacheExecutorLimit :: Natural }
data CircuitCacheMode = UncachedCircuitTables | RetainCircuitTables

data RetainedAcyclicOpenCircuit purity sort input output vertex edge label value
retainAcyclicOpenCircuit :: ExactTableInterpreterIdentity
  -> AcyclicOpenCircuit ExactTablePrimitive purity sort input output vertex edge label value
  -> RetainedAcyclicOpenCircuit purity sort input output vertex edge label value

retainedAcyclicOpenCircuitDenotation :: CircuitCacheMode
  -> CircuitSemanticLimits -> CircuitCacheLimits
  -> RetainedAcyclicOpenCircuit purity sort input output vertex edge label value
  -> Either (RetainedCircuitError edge)
       ( StochasticMatrix NonNegativeRational (Assignment input value) (Assignment output value)
       , RetainedAcyclicOpenCircuit purity sort input output vertex edge label value
       , CircuitCacheReport )
```

`CircuitCacheReport` reports cumulative source nodes, reserved source cells,
source work, primitive cells, observed numerator/denominator maxima, executor
work, lookup comparisons, constructions, hits, resident entries/cells/trace
slots. Cells and trace slots are exact resident table-size measures; heap
allocation is measured separately. Limits count local-circuit tabulation across all selected edge occurrences in one
request. Global live-frontier matrix construction keeps its existing contract;
these counters make no claim to bound that separate consumer phase.

No public primitive callback exists on this route. Matrix arguments are already
constructed, proof-carrying inputs. Their creation is outside this request;
reading, endpoint validation, scalar admission and primitive row-cell accounting
are inside it. The interpreter has one closed version identity. A retained
network owns its immutable circuits and its cache; callers cannot transplant
entries or replace a circuit under an existing owner.

## Identity and storage

Assign each resolved label-and-ordered-signature table entry its stable table
slot during `acyclicOpenCircuit`. Selected occurrences retain that slot. Within
the opaque retained owner it is the circuit identity, not a claim that equal
labels imply equal arbitrary circuits. A key contains interpreter version,
slot, exact ordered source and target layouts, and all six semantic limits.
Metadata planning compares owner-bound slots, dimensions and limits without
enumerating layouts; execution verifies the full literal key after admission.
The immutable owner makes the metadata match sound. An inconsistent internal
slot/layout use is rejected before any unreserved replacement is constructed.
Changing any semantic limit misses. Executor limits are checked independently.
A fresh retained owner always starts empty. No closure comparison, hash-only
identity or cross-owner transfer is exposed.

Storage is a deterministic insertion-ordered list. Lookup counts comparisons.
Successful complete matrices and complete ordered traces are inserted once;
there is no eviction in this first bounded implementation. Request planning
reserves prospective resident entries, cells and trace slots, including misses,
before table/trace materialization. Existing occupancy must also fit new cache
limits. Capacity rejection has its own infrastructure error. Failed requests
return only their first error: no matrix, receipt, changed owner or partial
entry. Since the input owner is immutable, it remains reusable after failure.

## Source execution and replay

The private algebra plans the complete raw circuit with the existing bounded
fold. Structural preflight bounds request-wide raw nodes and all materialized
intermediate matrix cells before scalar execution. It retains source association
and supports every existing constructor, including derived copy/composition for
sharing and copy/tensor/composition for independent fanout. It performs no
rewrites.

The checked reference executes leaves left to right and operations after their
operands. Each leaf scalar read, generated structural scalar, multiplication and
addition charges one source work unit before observing its exact Rational numerator then denominator. Compose
uses source-row, result-column, middle-layout order and records both products
and each accumulator, including discarded values. Tensor records each product.
Convex choice records coefficients, their accumulated total, and every weighted
product and sum in source order. Primitive endpoint checks and cumulative
primitive-cell checks occur at the primitive's original source position.
Primitive input and intermediate Rational validation remains at each source
event's original position.

Internally the successful trace is a list stored in reverse construction order.
Replay walks its tail first, preserving source order without allocating another
trace list. Each event has one of these forms:

```haskell
data SourceEvent
  = SourceCharge !Natural
  | SourceRational !Rational
  | SourcePrimitiveCells !Natural
  | SourcePrimitiveEndpoints !Bool !Bool
replaySourceEvent :: CircuitSemanticLimits -> SourceEvent -> CircuitCacheReport
  -> Either CircuitSourceError CircuitCacheReport
```

Replay applies these events in the same order to the request's current source
account. A construction uses that account once; its later hits charge the full
trace again. Limits and source structural reservations never reset between
occurrences. Success-only storage cannot retain a primitive failure. A primitive
cell limit may nevertheless first fail on a later hit because its account is
cumulative. Final-matrix scanning, maxima and aggregate charges are insufficient
and are not used as substitutes for ordered replay.

The executor account is separate. Its unit is one executed source-event dispatch,
one Rational numerator/denominator bit-loop iteration, one scalar computation,
one raw syntax node planned, one original support-spine cell inspected (including
its terminator), one cache metadata/list entry visited, one cache-entry key
comparison, one retained event consed, one stored-event replay read, or one entry insertion. Matrix traversal
and lookup use shape-derived reservation allowances; a matrix-cell lookup reserves
the complete source-by-target scan length of the existing list representation.
Those reservations are reported separately from executed dispatch counts. Integer
arithmetic cost and arbitrary user `Eq` cost are not machine-time units; no bound
on wall-clock duration or total runtime allocation follows from these counters.
A scalar emits `SourceCharge 1` and then `SourceRational`; numerator is observed
before denominator. Compose emits product then accumulated sum for each middle
value (or one checked zero for an empty middle layout). Tensor emits one product
per output cell. Convex choice emits coefficient read then coefficient-total sum
for each term, followed by product then accumulated sum for each cell/term.
Leaf primitive endpoint and cumulative primitive-cell events precede its cells.

Planning, lookup comparisons, retained trace allocation, scalar execution, bounded integer-bit observation, replay events and
insertion each consume documented executor work. Preflight uses conservative
shape-derived reservations before allocating a trace or table; actual counted
operations are reported separately from reserved allowances. A hit still scans
its trace and performs Rational validation. No constant hit-cost or speedup is
assumed. Wall-clock and GHC allocated-byte measurements provide the actual cost
evidence, independently of these machine-independent counters.

Precedence is invalid limits; cumulative executor fuel for raw syntax and original
support-spine planning; cache/executor infrastructure preflight; source
node/cell preflight; ordered source charges, Rational checks and primitive
validations; complete entry insertion; existing global frontier interpretation.
With admitted infrastructure, checked reference and retained execution must
produce identical matrices or the same first local source error. Infrastructure
failures are deliberately a separate relation. Empty circuits and zero budgets
have explicit boundary tests; bit limits must be positive.

## Verification and measurements

`test/CircuitCache.hs` and `test/CircuitCachePrivate.hs` cover independent literal
matrices, uncached versus cold/warm cache outcomes, every circuit constructor, repeated local circuit slots,
independent stochastic occurrences, layout and limit-key changes, cumulative
work and primitive failures on hits, discarded Rational intermediates, error
precedence, exact/one-below node/cell/entry/trace/executor/work/bit bounds, zero
and empty layouts, failure atomicity, and forcing the outer result before its
payload. Tests use the original interpreter as an independent denotation oracle
and explicit source-event expectations for admission, not just the shared replay
function as its own oracle.

`test/CircuitCacheBench.hs`, registered as the normal `circuit-cache-bench` Cabal
benchmark, prints compiler/platform, workload dimensions, repeated-edge count,
elapsed CPU and wall time, allocated bytes (RTS statistics enabled), cold
constructions, warm hits, executor counts, resident tables/cells and full trace
slots for uncached/cold/warm runs. The build command records optimization level.
Each timed action reads its immutable starting owner through an `IORef`, forces
the returned matrix/report and retains the completed returned owner inside the
timing. Warmup/setup are excluded. Allocation stays nonzero and scales with the
workload at both optimization levels. These are empirical measurements, not
semantic admission laws.

## Frozen failure surface

```haskell
data CircuitSourceError
  = CircuitInvalidNumeratorBitLimit | CircuitInvalidDenominatorBitLimit
  | CircuitSourceNodeLimitExceeded !Natural !Natural
  | CircuitSourceCellLimitExceeded !Natural !Natural
  | CircuitSourceWorkLimitExceeded !Natural !Natural
  | CircuitSourcePrimitiveCellLimitExceeded !Natural !Natural
  | CircuitSourceNumeratorBitLimitExceeded !Natural !Natural
  | CircuitSourceDenominatorBitLimitExceeded !Natural !Natural
  | CircuitSourcePrimitiveSourceMismatch
  | CircuitSourcePrimitiveTargetMismatch
  | CircuitSourceMatrixError !MatrixError
  | CircuitSourceInvariantFailure

data CircuitInfrastructureError
  = CircuitCacheEntryLimitExceeded !Natural !Natural
  | CircuitCacheCellLimitExceeded !Natural !Natural
  | CircuitCacheTraceLimitExceeded !Natural !Natural
  | CircuitCacheExecutorLimitExceeded !Natural !Natural

data RetainedCircuitError edge
  = RetainedCircuitInvalidLimits !CircuitSourceError
  | RetainedCircuitInfrastructureError !CircuitInfrastructureError
  | RetainedCircuitSourceError !Int !edge !CircuitSourceError
  | RetainedCircuitConsumerError !(AcyclicOpenInterpretationError edge ())
```

The two naturals are the active bound and the first disallowed count. Bounded
integer bit observation reports `limit + 1` without scanning an arbitrarily
large supplied integer. Source charge is unit-sized, so work exhaustion reports
its exact next unit. Primitive-cell and structural reservations can be batched;
they report the first disallowed cumulative boundary, not an overflowed sum.

## Executed evidence

On 2026-09-14, GHC 9.14.1 on aarch64 Linux passed
`scripts/check-circuit-cache` at `-O0` and `-O2`, including public integration,
all-constructor literal matrices and exact event sequences, complete trace-size
predictions, five ownership/closed-interpreter compile-fail boundaries, original
support-scan fuel, a `2^256` endpoint product that rejects without enumeration,
infinite request-spine rejection, poison-payload preflight, and outer-`Either`
strictness. An executable mutation reverses retained-event replay and is rejected
at the real hit's primitive-before-work assertion at both optimization levels. The successful result completes every table-input observation;
forcing its retained matrix/trace later performs no deferred input computation.

The source denominator counterexample uses `[1/256,0,255/256]` versus
`[0,1/256,255/256]`, each composed with discard. Both final matrices are `[[1]]`.
At work one / denominator eight, the first fails on denominator nine and the
second on work two. This independently distinguishes an ordered trace from a
final matrix or aggregate/maxima receipt.

[Raw benchmark samples](../evidence/D085-CIRCUIT-CACHE.csv) contain every one of
the three samples for both workloads, all three paths and both optimization
levels. Timings include the entire public denotation call, including its existing
global frontier phase. The reported resource ledgers cover local tabulation.
The larger fixture has 16 edge occurrences, each selecting the same depth-16
composition of a fair two-state table. Its source work is 4,864 on every route;
cold execution constructs one table and hits 15 times, while warm execution hits
16 times. The retained payload has one table, four matrix cells and 640 event
slots.

| Build | Route | Median CPU (ms) | Median allocated bytes | Executor operations | Executor reservation |
| --- | --- | ---: | ---: | ---: | ---: |
| O0 | Uncached | 7.317104 | 45,557,464 | 36,144 | 2,868,464 |
| O0 | Cold | 3.537536 | 30,021,264 | 41,856 | 5,118,456 |
| O0 | Warm | 3.212704 | 28,959,752 | 41,554 | 5,267,091 |
| O2 | Uncached | 2.617584 | 10,877,680 | 36,144 | 2,868,464 |
| O2 | Cold | 1.429936 | 8,836,288 | 41,856 | 5,118,456 |
| O2 | Warm | 1.265600 | 8,672,680 | 41,554 | 5,267,091 |

This fixture runs faster and allocates less with retention. It consumes more
logical executor dispatches and a larger conservative reservation: replay
validates every event, and its bit-loop allowance is deliberately conservative.
Neither the time improvement nor the reservation comparison is a universal
optimizer claim. The small fixture's O2 median allocations are 1,239,120 bytes
uncached and 999,944 bytes warm, confirming that the larger sample is not
merely timing a reused pure result thunk. Raw samples preserve the O0 timing
variation; this is a local run, not a hardware-independent performance bound.

With the pinned GHC 9.14.1 toolchain on `PATH`, these commands produced the
measurements in separate output directories:

```sh
for optimization in 0 2; do
  ghc -O"$optimization" -isrc -itest -rtsopts -with-rtsopts=-T \
    -outputdir "/tmp/markovian-backlog-cache-O$optimization" \
    test/CircuitCacheBench.hs -o "/tmp/markovian-backlog-cache-O$optimization-run"
  "/tmp/markovian-backlog-cache-O$optimization-run"
done
```

The maintained package entry point is
`cabal bench Markovian:circuit-cache-bench --project-file=cabal.project.ci`.
