# EL-05 — Checked supplied-partition fixed-policy aggregation

**Status:** Proposed

Frozen for independent review; not implementation evidence. Place the new `Markovian.Aggregation.Exact` module in the exact root. No new package or dependency edge; D-077–D-085 retain their statuses. This is a finite fixed-policy checker, not MDP-wide bisimulation, partition discovery, POMDP inference, or optimal-policy preservation.

## Existing consumer and topology

The consumer closes a tiny exact MDP with `compileExactMDP` and `closeCompiledExactPolicy`, then supplies a partition of the resulting `CompiledExactMRP state`. Read `compiledMRPStateEntries`, `compiledMRPStateIndex`, `compiledMRPInitialState`, and the compiled terminal/continuing rows. Policy closure has already removed action IDs while preserving the joint reward/successor law. Therefore actions and policy alternatives are **not** observations preserved by this checker. Compilation/closure is prior separately budgeted work; the checker re-admits the resulting finite data and never calls policy or model closures.

The supplied `FiniteSet block` is ordered, duplicate-free, and must be nonempty. The partition list has exactly one `(state,block)` for each original indexed state and uses every declared block. A separate total `(state,observation)` list declares the only state observation; use `()` for no distinction. Validate lists before canonicalizing to original state order. Keep original layout and explicit mapping; quotient order is exactly the supplied block order, representative is its first member in original state order. Nominal type roles protect state, block, and observation parameters; a result always owns its quotient and mapping together, not a reusable proof applied to another model.

Within a block require equal state observations, equal terminal/continuing kind, equal terminal payoff for terminal states, and, for continuing states, exactly equal joint mass for **each (immediate ExactReward, successor block)**. Aggregate duplicate atoms, not expected rewards or separate reward and successor marginals. Zero mass is omitted; terminal states have no outgoing law. Compare each member against the representative, not all state pairs. This sufficient finite congruence preserves all declared reward/observation traces and finite-horizon returns from states mapped to the same block.

Timing matches `evaluateCompiledExactFinite`: a terminal state's payoff applies even at horizon zero; a continuing state at horizon zero returns zero, with no invented terminal payoff. At positive horizon, emit the current transition reward, move to its correlated successor, discount the future once. State observations are seen at the initial state and at every reached successor including the final horizon or terminal state. Stop immediately at a terminal before checking horizon, emitting its payoff once, or stop by horizon at a continuing state without payoff. There is no observation draw or hidden observation likelihood. Equality of these labeled traces is mathematical equality of finite laws; it does not require the same raw duplicate layout or trajectory enumeration order. An observation constant on each block is preserved even at horizon zero.

## Complete proposed public declarations

`Natural`, `Rational`, `FiniteSet`, `FiniteStateIndex`, `CompiledExactMRP`, and `ExactReward` are existing public types. The new quotient is a small closed table, **not** a forged `CompiledExactMRP`. No changes to that type's opacity or unmetered recompilation are needed. Tests can interpret the quotient table independently alongside the existing original-model evaluator.

```haskell
data AggregationLimits = AggregationLimits
  { maximumAggregationStates :: !Natural
  , maximumAggregationBlocks :: !Natural
  , maximumAggregationSupport :: !Natural
  , maximumAggregationWork :: !Natural
  , maximumAggregationRationalBits :: !Natural
  } deriving (Eq, Show)
data AggregationDimension
  = AggregationStates | AggregationBlocks | AggregationSupport
  | AggregationWork | AggregationRationalBits
  deriving (Eq, Ord, Show)
data AggregationError state block
  = AggregationLimitExceeded !AggregationDimension !Natural !Natural
  | AggregationMachineOverflow !AggregationDimension
  | AggregationEmptyBlocks
  | AggregationDuplicatePartitionState !state
  | AggregationUnknownPartitionState !state
  | AggregationUnknownBlock !block
  | AggregationMissingPartitionState !state
  | AggregationUnusedBlock !block
  | AggregationDuplicateObservationState !state
  | AggregationUnknownObservationState !state
  | AggregationMissingObservationState !state
  deriving (Eq, Show)
data AggregationAccounting = AggregationAccounting
  { aggregationStateCount :: !Natural
  , aggregationBlockCount :: !Natural
  , aggregationRawSupportCount :: !Natural
  , aggregationCanonicalSupportCount :: !Natural
  , aggregationWorkCharged :: !Natural
  , aggregationMaximumObservedBits :: !Natural
  } deriving (Eq, Show)
data AggregationDifference block observation
  = AggregationObservationDifference !observation !observation
  | AggregationTerminalDifference !Bool !Bool
  | AggregationPayoffDifference !ExactReward !ExactReward
  | AggregationJointMassDifference !ExactReward !block !Rational !Rational
  deriving (Eq, Show)

type role AggregationWitness nominal nominal nominal
data AggregationWitness state block observation -- opaque
type role CheckedAggregation nominal nominal nominal
data CheckedAggregation state block observation -- opaque
data AggregationResult state block observation
  = AggregationEquivalent !(CheckedAggregation state block observation)
  | AggregationDistinguished !(AggregationWitness state block observation)
type role AggregationResult nominal nominal nominal

checkFixedPolicyAggregation :: (Eq state, Eq block, Eq observation)
  => AggregationLimits -> CompiledExactMRP state -> FiniteSet block
  -> [(state, block)] -> [(state, observation)]
  -> Either (AggregationError state block)
       (AggregationResult state block observation, AggregationAccounting)
aggregationOriginalLayout :: CheckedAggregation state block observation
  -> FiniteStateIndex state
aggregationBlockLayout :: CheckedAggregation state block observation
  -> FiniteSet block
aggregationMapping :: CheckedAggregation state block observation -> [(state, block)]
aggregationInitialBlock :: CheckedAggregation state block observation -> block
aggregationRows :: CheckedAggregation state block observation
  -> [(block, observation, Either ExactReward [(ExactReward, block, Rational)])]
aggregationWitnessStates :: AggregationWitness state block observation -> (state, state)
aggregationWitnessBlock :: AggregationWitness state block observation -> block
aggregationWitnessDifference :: AggregationWitness state block observation
  -> AggregationDifference block observation
```

Only the result, difference, error, dimension, accounting and limit constructors are exported. Other constructors are private. Witness state order is `(representative,member)` and difference field order follows it; terminal flags are True for terminal. For joint differences the witness includes a single canonical reward/block atom and its two masses, with absent mass represented by zero. The opaque witness attests that all input validation and canonicalization succeeded and that the particular comparison failed. Public `AggregationDifference` alone is just data, not checked evidence. No callbacks or unsafe selectors; all observers return stored data without new rational operations.

## Bounded traversal, preflight, ledger, failure precedence

Limits are independent hard caps; zero work/rational/support are allowed and reject any required positive charge. Reject limits above `maxBound :: Int` in field order (states, blocks, support, work, rational bits) with `AggregationMachineOverflow`. Counters and products use Natural with saturation at the relevant limit+1, never overflowing Int. List admission inspects at most limit+1 spine cells, so infinite supplied lists fail. Original model witnesses are finite but still re-admitted.

1. Count original states n up to state cap, then blocks b up to block cap, then partition and observation raw spines separately up to state cap (in that order). Check empty block layout. Count raw transition atoms cumulatively across continuing states in original layout order up to the support cap; duplicates count and terminal rows contribute zero. Raw input distributions remain opaque lawful exact distributions with valid compiled successor indexes. No result table allocation precedes this dimension preflight.
2. Reserve one conservative structural-work charge `W0=4*n*n+2*n*b+n+b+k*(n+b+1)` where k is total raw support. It covers partition/observation duplicate and coverage scans, mapping lookup, block coverage, original initial/successor index decoding, and collecting bounded row data. Compare W0 with work limit before validation. This is semantic admission work, not an actual-operation or time estimate.
3. Validate partition entries left-to-right: duplicate state first, unknown state next, unknown block next, at each entry. Then report first missing state in original order, then first unused block in block order. Validate observation entries left-to-right: duplicate then unknown state; then first missing state in original order. Reordered complete input lists are accepted and canonicalized, not mistaken for a different original layout. No equality test is made on unbounded input spines. As elsewhere in the library, Eq instances are assumed lawful and terminating; the budget counts invocations, not arbitrary work inside user Eq.
4. Observe terminal payoffs and continuing `(reward,mass)` rationals in original state/raw atom order; for each observe reward then mass. Each observation charges one work unit before checking `max(bits(abs numerator),bits denominator)` (zero is one bit). Observe the synthetic mass zero once, even for a terminal-only model. No rational operation or comparison in later steps bypasses this same ledger.
5. Canonicalize *all* continuing rows before equivalence comparisons, in original order. Map successors through the validated mapping; insertion-sort/aggregate atoms by ascending exact reward then block-layout ordinal. Each visited candidate atom costs one work unit for its key comparison; each rational mass addition costs one more and is checked before retaining or discarding its result. New insertions and append-at-end each cost one unit. Charge even a single first atom's insertion. Positive input masses imply no cancellations. `maximumAggregationSupport` bounds cumulative raw input atoms and cumulative canonical atoms separately; the latter cannot exceed the former. Every retained canonical mass is re-observed (one unit plus bit check). No public distribution constructor with a reset/default budget is called.
6. Compare blocks in declared order, members in original order excluding the representative. At each pair check observation first, terminal kind second, applicable payoff third or continuing joint row last. Each observation/kind/payoff equality costs one unit. Compare joint canonical rows by a merge scan in canonical atom order: each head-key comparison costs one unit and each mass equality one unit. The first missing or unequal atom determines the difference; missing mass is zero. At a missing tail atom charge one key-visit and one mass comparison as well. A complete equal row scan has no extra end-of-list charge. Reward comparisons use exact Rational ordering of already admitted data, not a floating comparison.
7. Before returning the *first* difference, charge one witness-construction unit plus one re-observation unit per rational field (two payoffs, or one joint reward and its two masses), check their bit sizes again, and charge one literal revalidation comparison using the differing retained data. Witness construction or validation exhaustion returns Left, never an unbounded or partially checked witness. A terminal/observation witness has no rational fields. On equivalence charge one per quotient row and mapping row retained; use already admitted representative rows, observations and masses without rebuilding distributions. Return only after every charge passes.

The ledger covers both successful outcomes of `AggregationResult`. Left returns no partial quotient, witness or accounting. `aggregationCanonicalSupportCount` counts all original canonical rows, not only quotient rows, including when a witness is returned. Work-before-rational precedence applies at every operation. No automatic partition search, rational evaluation API, or trace enumerator is added in this stage.

## Required tests and theorem boundary

Teach a successful two-state merge with matching reward/next-block laws, and a concrete failed merge. Compare original `evaluateCompiledExactFinite` with independent finite induction on `aggregationRows` for each lifted state and several objectives, including horizon zero and terminal-after-last-transition. Independently enumerate the admitted reward/observation/stop traces of tiny fixtures; do not reuse production canonicalization as an oracle. Include equal expected reward but different reward laws, identical separate marginals but different reward/successor correlation, terminal mismatch, unequal terminal payoffs and observation mismatch. Include duplicate/unknown/missing partition and observation entries, unused blocks, reordered valid lists, infinite-list prefix rejection, nominal-role and constructor compile-fail gates, exact and one-below every limit, rational growth on duplicate mass addition, witness exhaustion and combined-invalid precedence. Add current public snapshots, capabilities, changelog, lesson and archive membership at implementation time only.
