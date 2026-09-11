# D083: frozen exact CE and CCE one-witness contract

## 1. Authority and scope

Placement is approved in the existing base-only `Markovian` library. Both public operations belong in `Markovian.Game.Correlated.Exact`.

This contract is frozen after parent design acceptance and independent DESIGN PASS. D083 remains Proposed and unimplemented. Contract freeze is not capability acceptance. Implementation is not authorized by this document and requires separate parent authorization.

Keep the active-set generator, constraint builder, elimination, and account private in that module. No additional internal module is needed for this design. Add no package, dependency edge, general LP interface, matrix callback, or pivot callback.

The solver returns the first checked witness. It does not optimize an objective, solve Nash, enumerate equilibria, or return all vertices. It uses exact `Rational` arithmetic. It does not clip masses, rescale a solution, apply a tolerance, or normalize a device after elimination.

## 2. Source basis and existing boundaries

The source basis is `/home/josephcox/dev/Markovian` at `7f2d8bdbe699a9b9fad99f22335a9ebc6b13446d`. The stale pre-integration semantic index is not evidence.

Read and preserve these definitions:

- `docs/DECISIONS.md`, D-083, and `TODO.md`, R7.
- `Markovian.cabal:215–269`: the game modules are exposed by the base-only library.
- `src/Markovian/Category/Finite/Object.hs:1–83`: `FiniteObject` is nonempty and layout-sensitive.
- `src/Markovian/Game/Profile/Finite.hs:55–232,327–383`: limits, nominal products/profiles, product enumeration, replacement, and combined Rational bits.
- `src/Markovian/Game/NormalForm/Exact.hs:49–208`: complete nominal player values and complete nominal payoff tables.
- `src/Markovian/Game/Correlated/Exact.hs`: literal device construction, CE/CCE row order, arithmetic, and report construction.
- `src/Markovian/Feedback/Value/Exact.hs:444–486` and `src/Markovian/Feedback/Internal.hs:107–136,255–306`: contrasting elimination and accounting, not code to copy.
- `src/Markovian/Category/Matrix.hs:63–98` and `src/Markovian/Category/Payoff/Exact.hs:54–78`: different construction contracts, not solver admission.
- `test/MixedBayesianGames.hs:139–274` and `test/compile-fail/MixedGameBoundary.hs`: current mathematical and ownership controls.

`FiniteObject []` fails with `EmptyFiniteObject`. Thus an admitted `OwnedProduct` has at least one owner. Each owner has at least one action. The represented profile carrier is also nonempty. The public constructors do not admit an empty-owner game, an empty action carrier, or an empty profile table.

The mathematical empty product has one empty tuple. That convention does not extend this public API. No admitted profile has zero owner entries. For this API, `n = 1` means every represented owner has exactly one action. The one profile is a complete, nonempty owner/action tuple. Only the selected *active subset* is empty.

Preserve the nominal roles of `ExactNormalGame`, `OwnedProduct`, `OwnedProfile`, `ExactPlayerValues`, and `ExactCorrelationDevice`. Reuse their layouts without reconstruction or coercion. Constructor completeness is trusted for total values produced through the public API. Bottoms, unsafe coercions, and unlawful or nonterminating `Eq` instances are outside the contract.

A successful solve certifies its actual input payoff table. A device alone certifies a literal distribution on a product. It does not certify equilibrium for every game with that product. Existing public checker reports remain ordinary reports, with their existing constructors and contracts.

## 3. Frozen public surface

The following names and signatures form the frozen public surface. `Natural` is from `Numeric.Natural`.

```haskell
data CorrelationSolveLimits
correlationSolveLimits
  :: GameLimits -> Natural -> Natural -> Natural -> CorrelationSolveLimits
correlationSolveGameLimits
  :: CorrelationSolveLimits -> GameLimits
maximumCorrelationSolveInequalities
  :: CorrelationSolveLimits -> Natural
maximumCorrelationSolveCandidates
  :: CorrelationSolveLimits -> Natural
maximumCorrelationSolveMaterialization
  :: CorrelationSolveLimits -> Natural

solveCorrelatedEquilibrium
  :: (Eq owner, Eq action)
  => CorrelationSolveLimits
  -> ExactNormalGame owner action
  -> Either (CorrelationSolveError owner action)
            (CorrelatedEquilibriumSolution owner action)

solveCoarseCorrelatedEquilibrium
  :: (Eq owner, Eq action)
  => CorrelationSolveLimits
  -> ExactNormalGame owner action
  -> Either (CorrelationSolveError owner action)
            (CoarseCorrelatedEquilibriumSolution owner action)

data CorrelatedEquilibriumSolution owner action
data CoarseCorrelatedEquilibriumSolution owner action
type role CorrelatedEquilibriumSolution nominal nominal
type role CoarseCorrelatedEquilibriumSolution nominal nominal

correlatedSolutionGame
  :: CorrelatedEquilibriumSolution owner action -> ExactNormalGame owner action
correlatedSolutionDevice
  :: CorrelatedEquilibriumSolution owner action -> ExactCorrelationDevice owner action
correlatedSolutionCheck
  :: CorrelatedEquilibriumSolution owner action -> CorrelatedEquilibriumReport owner action
correlatedSolutionAccounting
  :: CorrelatedEquilibriumSolution owner action -> CorrelationSolveAccounting

coarseCorrelatedSolutionGame
  :: CoarseCorrelatedEquilibriumSolution owner action -> ExactNormalGame owner action
coarseCorrelatedSolutionDevice
  :: CoarseCorrelatedEquilibriumSolution owner action -> ExactCorrelationDevice owner action
coarseCorrelatedSolutionCheck
  :: CoarseCorrelatedEquilibriumSolution owner action -> CoarseCorrelatedEquilibriumReport owner action
coarseCorrelatedSolutionAccounting
  :: CoarseCorrelatedEquilibriumSolution owner action -> CorrelationSolveAccounting

data CorrelationSolveAccounting
correlationSolveReservedWork :: CorrelationSolveAccounting -> Natural
correlationSolveReservedMaterialization :: CorrelationSolveAccounting -> Natural
correlationSolveObservedRationalBits :: CorrelationSolveAccounting -> Natural
correlationSolveCheckerCoveredRationalBits :: CorrelationSolveAccounting -> Natural
correlationSolveCandidates :: CorrelationSolveAccounting -> Natural
correlationSolveRankDeficientCandidates :: CorrelationSolveAccounting -> Natural
correlationSolveInconsistentCandidates :: CorrelationSolveAccounting -> Natural
correlationSolveInequalityRejectedCandidates :: CorrelationSolveAccounting -> Natural
correlationSolveSelectedInequalities :: CorrelationSolveAccounting -> [Natural]
```

The three trailing constructor arguments are inequality count, attempted candidate count, and cumulative materialization credits, in that order. Every cap is inclusive. Zero is a valid cap. The constructor performs no game traversal and supplies no defaults. Work and Rational caps come from the embedded `GameLimits`. Its horizon field has no solver use.

Limits, solutions, and accounting have hidden positional constructors and `Eq`/`Show` instances. Their accessors are functions, not record selectors. Clients cannot construct or update a solution or its accounting. Each solution retains four fields: the original game handle, device, actual checker report, and accounting. Retaining the game handle does not copy its payoff table.

Accounting has nine fields, in the accessor order above. The selected indices are zero-based inequality indices. There is no public search continuation or rejected-candidate trace. CE and CCE solution types are not interchangeable.

Export the following error and enumeration constructors. Give each `Eq` and `Show` instances. Apply nominal owner/action roles to `CorrelationSolveError`.

```haskell
data CorrelationSolvePhase
  = CorrelationAdmission
  | CorrelationConstraints
  | CorrelationCombination
  | CorrelationElimination
  | CorrelationInequalities
  | CorrelationVerification
  | CorrelationPublication

data CorrelationSolveResource
  = CorrelationInequalityCount
  | CorrelationCandidateCount
  | CorrelationMaterialization
  | CorrelationWork
  | CorrelationRationalBits

data CorrelationRepresentation
  = CorrelationOwnerLength
  | CorrelationChoiceLength
  | CorrelationProfileLength
  | CorrelationReportLength
  | CorrelationRationalLength

data CorrelationSolveInvariant
  = CorrelationInputLayoutInvariant
  | CorrelationConstraintLayoutInvariant
  | CorrelationCandidateShapeInvariant
  | CorrelationShadowVerificationInvariant
  | CorrelationCheckerDisagreement
  | CorrelationCompletedSearchWithoutWitness

data CorrelationSolveError owner action
  = CorrelationSolveProductError !(OwnedProductError owner)
  | CorrelationSolveLimitExceeded
      !CorrelationSolvePhase !CorrelationSolveResource !Natural !Natural
  | CorrelationSolveRepresentationExceeded !CorrelationRepresentation
  | CorrelationSolveDeviceError !(CorrelationDeviceError owner action)
  | CorrelationSolveCheckerError !CorrelatedCheckError
  | CorrelationSolveInvariantFailure !CorrelationSolveInvariant
```

For `CorrelationSolveLimitExceeded`, the two numbers are the cap and the saturated required value, in that order. On failure, the required value is exactly `cap + 1`. It is not a claimed exact demand beyond the cap. Errors contain no candidate, device, accounting snapshot, or resumable state. Existing nested product/device errors retain their existing meanings.

There is no public `NoEquilibrium` or successful `Nothing`. A completed public search without a witness returns `CorrelationCompletedSearchWithoutWitness`. This is an invariant failure, not a mathematical nonexistence result.

## 4. Geometry and row order

Let `r` be the owner count. Let `a_i` be owner `i`'s action count. Let `L = sum_i a_i`. Let `n = product_i a_i` be the represented profile count. Let `I = fromIntegral (maxBound :: Int)`, interpreted as a `Natural`. The chosen length ceiling remains `I-1`. Use `Natural` for all new counts and indices, including `q` and `m`.

Use `ownedProfiles` order for probability coordinates. Existing enumeration is owner-major. The first owner's action changes slowest. The last owner's action changes fastest. Never sort labels or discard zero-payoff profiles.

Use the equality

\[
\sum_{j=0}^{n-1} p_j = 1.
\]

Every inequality has the form `c dot p >= 0`. Store all coefficients, including zeros.

1. Indices `0 .. n-1` are nonnegativity rows `e_j dot p >= 0`, in profile order.
2. Append CE or CCE rows in the existing checker's order.

For CE, enumerate owner, recommended action, alternative action. Both action traversals use the owner's carrier order. Omit only self-deviation rows. For row `(i,a,b)`, set

\[
c_j = \begin{cases}
u_i(s_j)-u_i(s_j[i\leftarrow b]), & s_{j,i}=a,\\
0, & s_{j,i}\ne a.
\end{cases}
\]

Thus `q = sum_i a_i(a_i-1)` and `m = n+q`.

For CCE, enumerate owner and alternative action in carrier order. For row `(i,b)`, set

\[
c_j=u_i(s_j)-u_i(s_j[i\leftarrow b]).
\]

Thus `q = L` and `m = n+q`. Retain the self-replacement zero contributions. Retain duplicate, dependent, and identically zero rows in both modes. Do not deduplicate rows or bases.

Build rows from left to right. Use `replaceChoice` and `normalPayoff` deliberately. A failed lookup is a constraint-layout invariant failure. These helpers have bounded list traversals inside each reserved coefficient block. No array or `containers` dependency is needed.

## 5. Deterministic search

Set `k = n-1`. Stream increasing `k`-tuples of indices from `0 .. m-1` in lexicographic order. The initial tuple is `[0 .. k-1]`. For `k = 0`, emit `[]` exactly once.

The successor scans the current tuple from right to left. Find the greatest position `j` with `index_j < m-k+j`. Increment that entry. Replace its suffix with successive integers. If no position qualifies, the family is complete. Implement the scan with bounded linked-list traversal and reversal. Do not treat linked-list indexing as constant-time.

Do not compute a binomial coefficient. Do not allocate a list of combinations. Do not preflight the entire candidate family. Search admission covers a prefix, not a promise to complete every possible basis.

For each tuple, construct one `n` by `n+1` augmented matrix. Row zero is `(1,...,1 | 1)`. Remaining rows are the selected inequalities, in tuple order, with RHS zero. The equality is fixed, not selected from the inequality family.

Use Gauss-Jordan elimination as follows:

1. Start with pivot-row position zero and coefficient column zero.
2. Search the current column from the pivot-row position to the final row.
3. Select the first row whose coefficient is nonzero.
4. If no row qualifies, advance the column without advancing the pivot-row position.
5. Otherwise swap that row with the pivot-row position. Preserve all other row positions.
6. Save the pivot before the swap's row changes. Divide every augmented entry by that pivot, left to right.
7. Visit every other row in increasing row-position order. Save its current pivot-column coefficient as the factor.
8. For every augmented column, left to right, compute `factor * normalizedPivotEntry`, then `oldEntry - product`.
9. Do not skip a zero factor or an entry known to cancel. Observe each product and difference.
10. Install the normalized pivot row and the updated rows. Advance both pivot-row position and coefficient column.

Only coefficient columns can provide pivots. Never pivot on the RHS. Complete all coefficient-column searches before classifying the matrix.

Next scan rows in order. A zero coefficient row with a nonzero RHS makes the candidate inconsistent. Inconsistency takes precedence over rank deficiency. Otherwise, fewer than `n` pivots means consistent rank deficiency. Reject either class. Do not assign free variables.

For full rank, read the RHS in variable order. This is the only candidate probability vector. Check normalization with a left fold from zero. Then check all `m` inequalities in row order, including selected rows. For each row, multiply all `n` coefficients by their masses and add them with a left fold from zero. Retain and observe each product before addition. Do not omit zero terms.

A normalization failure is an invariant failure. A negative inequality slack rejects the candidate. Stop at the first negative slack. Do not check later rows for that candidate. Check inactive inequalities as well as active ones. No resource failure can reject a candidate and continue search.

After an ordinary rejection, discard the matrix, candidate, and row-check scratch. Keep only counters, the constraints, borrowed game layouts, and current active state. Compute the next tuple. The ledger does not reset or refund reservations.

After an eligible candidate passes final verification, return immediately. Do not request its successor. First active-set success is not global witness optimization.

## 6. One cumulative account

### 6.1 Units and chosen reservation policy

Use one private strict account `(W,C,H,V,t,d,i,v)`. `W` is cumulative reserved work. `C` is cumulative materialization credits. `H` is the historical observed combined Rational-bit maximum. `V` is the checker-covered Rational-bit maximum. The final four counters count candidates, rank-deficient rejections, inconsistent rejections, and inequality rejections.

This contract deliberately uses conservative block reservations. It does not report measured arithmetic work or actual allocated cells. The schedule below is fixed, including over-reservation on early block exits. There are no refunds.

A work unit covers one source-level scalar operation, branch, constructor inspection, list-link traversal, field access, or label equality invocation. Rational arithmetic counts as a source-level operation, not constant physical cost. Rational observation also counts numerator/denominator bit-scan steps. Label equality counts an invocation, not work inside the client's `Eq` instance. Account bookkeeping and saturated cap comparisons use a fixed number of scalar registers and do not recursively meter themselves.

A materialization credit covers one newly constructed logical field. A cons has two fields. A tuple has its arity in fields. A Rational result has one logical Rational field. A row header has one reference field. A label or borrowed object reference has one field, not a deep copy. Counters and bounded control registers are not a heap model. Input game storage is borrowed, not newly materialized.

Let `B` be `maximumGameRationalBits`. Define the exact reservation expressions

\[
F(d)=1024d^6,\qquad R(d)=F(d)(B+1).
\]

Each block in the table reserves exactly `(delta W, delta C) = (R(d),F(d))`, unless the table says otherwise. The dimensions are positive. Evaluate these expressions with capped multiplication, not unrestricted powers.

| Block | Number and parameter `d` |
|---|---|
| Admission spine inspection | One per cons or terminating nil inspected. Reserve `(16,0)` instead. |
| Product validation | Once, `d = 1+r+L+n`. |
| Payoff-vector admission | Once per canonical profile, `d = 1+r+L+n`. |
| Geometry and constants | Once, `d = 1+r+L+n`. |
| Constraint coefficient | Once per stored coefficient, row-major, `d = 1+r+L+n+m`. |
| Constraint row completion | Once per inequality row, same `d`. |
| Initial tuple or successor | Once per request, including the request that establishes completion, same `d`. |
| Candidate matrix construction | Once per emitted tuple, same `d`. |
| Elimination column | Once per coefficient column, even when no pivot exists, same `d`. |
| Matrix classification and RHS extraction | Once per eliminated matrix, same `d`. |
| Candidate normalization | Once per full-rank candidate, same `d`. |
| Inequality dot product | Once per visited inequality of a full-rank candidate, same `d`. |
| Verification shadow | Once per eligible candidate, same `d`. |
| Literal device constructor | Once per eligible candidate after its shadow, same `d`. |
| Actual CE or CCE checker | Once per constructed candidate device, same `d`. |
| Final report comparison and publication | Once after an actual checker result, same `d`. |

The geometry block counts `q` and `m` under the inequality-cap arithmetic but does not allocate their rows. If that cap fits, both counts are exact. Then admit `q <= I-1` before constants or row construction, as specified in §7. This uses the existing geometry reservation without another block or field allowance. The initial admission uses no materialized dimension list. It accumulates `r`, `L`, and the cardinality product in scalar registers.

These intentionally broad reservations cover linked-list searches, output structures, and transient structures. They are not unexplained per-call checker estimates. The following decomposition provides a reviewable bound for each non-spine block:

- All bounded lists have length at most `d`, except flattened matrices/tables, which have at most `d^2` fields or entries.
- One profile equality examines at most `r` owner/action pairs. A payoff lookup scans at most `n` profiles, then at most `r` owners.
- `replaceChoice` scans at most `r` owner rows, at most `L` local actions, and at most `r` profile entries.
- A coefficient block uses at most one replacement, two payoff lookups, and one payoff subtraction.
- A matrix block scans constraints and selected indices in increasing order. It builds at most `n(n+1)` Rational entries. No repeated enumeration of earlier combinations occurs.
- A pivot block uses at most `n` row searches and at most `n(n+1)` multiply/subtract pairs. Naive bounded list selection, swapping, and rebuilding add at most one further factor `d`.
- A checker block visits at most `qn` contribution sites. Each site has the replacement and lookup bounds above. CE matching and recommendation folds add at most `2qn` visits.
- Device duplicate, membership, and canonical lookup scans have at most `3n^2` profile comparisons. No supplied list is longer than `n`.
- Report equality traverses at most two ordered reports of `q` rows, not search history.

For these specified loops, 64 `d^4` bounds non-bit scalar visits and field construction per block. Rational inputs, outputs, and observations add at most 64 `d^4` sites. An arithmetic result from admitted operands has at most `4B+1` combined bits. A bounded observer stops at its first exceeded bound. Existing unmodified calls see only shadow-admitted values. Their bit scans therefore have at most `B` steps per observed Rational. Thus `512d^4(B+1)` bounds the specified source-level work. `R(d)` exceeds this bound for every `d >= 1`. `F(d)` exceeds field construction, including two reports during verification.

This bound permits ordinary linked-list implementations of these exact loops. It does not permit arbitrary extra traversals, memoization tables, callbacks, retained histories, or deliberate recomputation until the allowance is spent. An implementation must show its loops fit this decomposition. A changed schedule requires contract review. The large fixed factor favors a simple first implementation over tight admission.

### 6.2 Reservation equations and precedence

Let caps be `Wmax = maximumGameWork`, `Cmax = maximumCorrelationSolveMaterialization`, and `Tmax = maximumCorrelationSolveCandidates`.

Before each block:

\[
C' = \operatorname{sat}_{Cmax}(C+\Delta C),\quad
W' = \operatorname{sat}_{Wmax}(W+\Delta W).
\]

Check `C'` first, then `W'`. Commit both only if both fit. Reserve before block traversal, Rational arithmetic, or materialization. Inside an admitted block, observe Rational values in the specified order. A Rational failure terminates that block and the whole solve.

For tuple emission, first determine that a tuple exists inside the reserved combination block. Next check `sat_Tmax(t+1)`. Only then expose that tuple internally and increment `t`. A zero candidate cap fails on the initial tuple, not at constructor time. Completion after the last rejected tuple does not require an extra candidate credit. It does require the final successor block's work/materialization reservation.

Use `cappedGameAdd` and `cappedGameProduct` for cap-directed Natural arithmetic. Each cap has its own saturation. Do not use an overflow sentinel from one cap to calculate another cap's quantity. Stop when the earlier failing cap is known. `F` needs six capped multiplications and one constant multiplication. `R` adds one capped multiplication. Never construct a huge family size or a huge binomial integer to reject admission.

`C` is cumulative, not a live-memory maximum. Successful constraints, temporary replacements, rejected matrices, final checker structures, and publication all consume its credits. This stricter policy gives a simple bound on materialization without a general lifetime ledger. Increasing a cap can admit more work but cannot change tuple order or the first mathematical success.

### 6.3 Rational observations

Use the game's combined convention:

\[
\operatorname{size}(x)=\operatorname{bits}(|\operatorname{numerator}(x)|)
 +\operatorname{bits}(\operatorname{denominator}(x)),\quad
\operatorname{bits}(0)=1.
\]

The denominator is positive. `size(0) = size(1) = 2`. Do not use Feedback's maximum-of-two convention.

Observe each input payoff in canonical profile/owner order. Observe constants zero and one before constraint construction. Observe every generated coefficient. During elimination, observe every pivot, factor, quotient, product, and subtraction result immediately. Observe matrix entries copied into the candidate, every normalization prefix, every inequality product and prefix sum, every verification-shadow value, and every retained Rational result.

Update `H = max H (size value)` before any later discard or cancellation. Rejected candidates remain part of that history. Rational comparisons observe operands already admitted at their producing or input sites. Their hidden cross-products are not new source-level Rational values.

A bounded bit observer uses `Natural` counters and stops as soon as a bound is exceeded. It does not finish scanning an arbitrarily large input merely to report its exact size. First enforce `B`. Then enforce the representational ceiling `fromIntegral (maxBound :: Int) - 1`. If both fail at the same observed size, the configured Rational limit wins. The representation ceiling protects calls to the existing `Int`-counted `rationalSizeBits`. The solver does not change that public function.

Observe an arithmetic result after computing that source-level Rational result and before retaining or using it. This is not a pre-allocation bound on Data.Ratio reduction. Rational arithmetic on operands of combined size at most `B` has reduced result size at most `4B+1`. The statement covers addition/subtraction, multiplication, and division by a nonzero pivot. It does not expose integer products, GCD state, or other intermediates hidden inside Data.Ratio/GMP.

The contract bounds represented work, logical fields, and observed Rational sizes. It does not bound physical time, heap bytes, stack bytes, or hidden arithmetic allocation.

## 7. Admission and complete phase precedence

No solve-specific limit constructor validation precedes the solve. Caps are Naturals and may be zero.

1. **Admission:** count the owner carrier, then each action carrier in owner-row order, then the profile carrier. Reserve a spine block before each cons/nil inspection. Check the corresponding configured owner/local-choice/profile cap immediately after each count increment. Apply its existing product error constructor. Then check the relevant `Int - 1` representation ceiling. Accumulate local counts and product cardinality with saturated arithmetic. An opaque-layout disagreement is an input-layout invariant failure.
2. Check owner/profile cells under `maximumGameCells`. Call the existing `validateOwnedProduct` in its reserved product-validation block, with the unchanged supplied `GameLimits`. Return its original error through `CorrelationSolveProductError`.
3. Traverse canonical payoff vectors, one reserved block per profile. Check the vector's owner layout against the game's owner layout. Observe every payoff. Opaque completeness avoids a second duplicate-table construction. A layout or table-length disagreement is an input-layout invariant failure.
4. **Constraints:** reserve geometry and constants, with the existing materialization-before-work precedence. Compute the chosen mode's `q`, then `m = n+q`, using arithmetic saturated at the inequality cap. If the inequality cap fails, return `CorrelationSolveLimitExceeded CorrelationConstraints CorrelationInequalityCount cap (cap+1)` first. Once it fits, `q` and `m` are exact. Check `q <= I-1`. Otherwise return `CorrelationSolveRepresentationExceeded CorrelationReportLength`. This report-length check precedes constant Rational observations, row materialization, the shadow, and legacy calls. Construct each coefficient and row under its unchanged table reservation only after these checks.
5. **Combination:** reserve initial state or successor. Establish completion or check the next candidate credit. Completion without a witness returns the completed-search invariant failure.
6. **Elimination:** reserve matrix construction, each column, then classification. Rank deficiency and inconsistency are ordinary rejections, with their separate counters.
7. **Inequalities:** reserve normalization and each visited row. A negative slack is an ordinary rejection, with its counter. Then return to combination.
8. **Verification:** perform the shadow, literal construction, and actual checker, in that order.
9. **Publication:** compare the actual report with the shadow and construct the solution under the final reservation.

Within every reserved block, materialization precedes work, and work precedes observed Rational failure. Within admission, a reservation failure can precede a later structural error because the traversal has not occurred. Within a coefficient, replacement failure precedes incumbent lookup failure, then deviation lookup failure, then subtraction observation.

No later phase overrides an earlier failure. A final reservation failure returns no device, even when all inequalities already passed. No arithmetic or materialization continues after a failure. Do not return a lazy success that postpones admitted traversal, checking, or report construction until the client reads it.

## 8. Final verification with the real checker

The existing checker is mandatory. Its work estimate is not the solver ledger. Its returned report cannot recover discarded Rational intermediates.

Both real checkers set their row-count field with `fromIntegral (length checks)`. That `length` uses `Int`, even though the destination uses `Natural`. Individual ceilings on `r`, each `a_i`, and `n` do not bound aggregate `q`. The Constraints geometry check therefore admits `q <= I-1` before either checker can construct its report. A later report comparison cannot replace that admission. Do not refactor either legacy checker.

Use one narrowly scoped arithmetic shadow before unmodified calls. This is private solver verification, not a replacement public checker or a general tracing framework. It exists to admit and observe the exact source-level Rational operations that the unmodified calls will repeat.

The shadow performs these steps in this order:

1. Traverse candidate masses in profile order. Check nonnegativity and observe all masses first. Then observe each left-fold total from zero. Require literal total one. This follows the constructor's mass-admission and summation order. Repeat the fold from zero, with mass observation immediately before each addition, to follow `validateDevice`. Require literal total one again.
2. In CE mode, traverse the actual checker row order. Build the matching profile sequence with `profileChoice`, including zero-mass matches. Left-fold recommendation mass from zero. Then left-fold slack from zero. For each matching profile, use `replaceChoice`, incumbent `normalPayoff`, deviation `normalPayoff`, subtraction, multiplication, and addition, in that order.
3. In CCE mode, traverse its checker row order and every profile. Perform the same replacement, two payoff lookups, subtraction, multiplication, and addition.
4. Observe each recommendation total, slack, and all intermediate arithmetic results. Retain the ordered shadow row values and statuses. Require every slack to be nonnegative.

The shadow has separate mass-admission and total sequences for the constructor and device validation. Each sequence equals its corresponding unmodified call's sequence. Its per-row arithmetic sequence equals the selected current checker body. Repeating those exact pure Rational expressions produces the same reduced Rational values. This equality covers discarded source-level values, not Data.Ratio internals. Set `V` to the maximum size in those covered sequences, including masses and totals. The global `H` already includes them through direct shadow observation. `V <= H` on success.

The shadow must not use one fused dot product where the checker uses recommendation filtering and separate folds. It must not use only constraint slacks or the returned report to estimate intermediate sizes. Future checker arithmetic changes require a fresh coverage proof before the solver can use them.

Next reserve the literal-device block and actually call:

```haskell
exactCorrelationDevice gameLimits (normalGameProduct game)
  (zip canonicalProfiles candidateMasses)
```

Here `gameLimits` means the same embedded limits supplied at solve entry. Do not normalize, widen, reset, or replace them with a remaining-work limit. Construction failures terminate through `CorrelationSolveDeviceError`. They do not continue search.

Next reserve the checker block and actually call exactly one of:

```haskell
checkCorrelatedEquilibrium gameLimits game device
checkCoarseCorrelatedEquilibrium gameLimits game device
```

The block reservation covers the actual call's list work, arithmetic, bit scans, and report fields. Its charge is additional to the shadow's charge. No per-call estimate is credited back to the account. The checker's own `4qn` work gate, product validation, and Rational checks remain unchanged. The broader cumulative reservation dominates these per-call work requirements on an admitted successful path.

A checker `Left` terminates through `CorrelationSolveCheckerError`. After `Right`, reserve publication. Force and compare every report field with the shadow and the expected counts. Compare the arithmetic-work field with the checker's existing `4qn` formula. Require its satisfaction flag to be true. Preserve its actual report in the solution.

A false flag, row mismatch, status mismatch, or count mismatch is `CorrelationCheckerDisagreement`. A negative shadow slack after all generated inequalities passed is `CorrelationShadowVerificationInvariant`. Neither is a reason to try another basis.

The report comparison also confirms that all retained Rational fields belong to the admitted sequences. `correlationSolveObservedRationalBits` means an actually observed historical maximum, including shadow arithmetic. `correlationSolveCheckerCoveredRationalBits` means the maximum covered by that sequence-equality proof. It is not a claim that the unmodified checker emitted tracing events.

## 9. Streaming and storage invariants

Retain at most the following logical payloads during search:

- `m*n` Rational constraint coefficients and `m` row headers.
- One active tuple with `k` indices. Successor construction may retain its old and replacement lists temporarily, with at most `2k` index entries.
- One current augmented matrix with `n(n+1)` Rational entries. A pivot update may retain old and replacement matrices, with at most `2n(n+1)` entries.
- One normalized pivot row of `n+1` Rational entries and one factor/product/result scratch sequence evaluated strictly.
- One candidate of `n` masses and a constant number of Rational fold accumulators.
- One temporary replacement profile of `r` owner/action pairs at a contribution site. Do not collect all replacement profiles.

During final verification, release elimination workspace before device/checker construction. The shadow may retain one report-sized list. The actual checker may retain another. CE uses `q` rows with two Rational fields each. CCE uses `q` rows with one Rational field each. A CE matching list holds at most `n` profile/mass entries for the current row. The complete device has `n` profile/mass entries. The final solution retains one actual report and one selected tuple, not the shadow report.

Each site's construction fits its reserved field envelope. Cumulative `C` bounds all such construction, including transient old/new structures. The explicit payload counts explain streaming shape. Neither those counts nor `C` equals physical heap size. No eight-slot theorem or allocator action model applies here.

The combination state cannot retain prior tuples. The elimination loop cannot retain prior matrices through unevaluated closures. Rejection counters cannot retain candidates. Force fold accumulators and new matrix entries before the next transition. Keep no cache, all-vertex collection, or hidden optimizer state.

## 10. Completeness and terminal meaning

Every admitted game has finitely many nonempty action sets and rational payoffs. A finite game has a CE. Every CE is a CCE. Each corresponding probability polytope is nonempty and bounded.

Its constraints are rational. It therefore has a rational vertex. At a vertex, normalization plus `n-1` independent active inequalities determines the point. Redundant rows and degeneracy do not remove all such bases. The streamed family contains that basis.

Consequently, complete search with sufficiently large representable admission, materialization, work, candidate, and Rational limits must find a checked witness in both modes. The finite-search assertion is conditional on all required blocks being admitted. It is not a claim that a configured prefix is complete.

Resource exhaustion terminates with its resource error. It never skips a tuple, changes the order, returns nonexistence, or returns a partial witness. If the private traversal completes without success, the public operation reports an invariant failure. Test actual infeasibility through private linear systems, not a fabricated finite-game no-equilibrium example.

## 11. Independent test matrix and exact fixtures

These are required future tests, not executed evidence. Use source-named tests for each public operation and the private combination/elimination functions. Share no production constraint builder, elimination, or replacement function with the mathematical oracle.

The oracle reads labelled payoff tables and labelled joint masses. It evaluates deviations by constructing changed label tuples directly. CE evaluation groups by recommendation labels. CCE evaluation evaluates constant deviations before recommendation. Check literal mass completeness, nonnegativity, and total one separately.

Use the complete vertex fixtures below. Their completeness follows from the stated algebra, not a finite denominator grid. Production search need only return its first witness. Private test traversal may collect all tiny vertices for comparison. The production-builder comparisons in §11.6 are mandatory, whether or not tests collect vertices through private traversal. No public all-vertex interface is added.

### 11.1 Singleton and constructor admission

One owner, one action, payoff zero has `n=1`. Its only distribution is `(1)`. CE has no obedience rows. CCE has one zero row. Both use exactly the empty selected tuple, not an empty owned profile.

Test empty owner and action carriers at `finiteObject`. Test an empty profile table at normal-game/device construction. Do not bypass these errors to create a solver fixture. Test one and multiple owners with one action each. Test zero owner, local-choice, profile, cell, work, Rational, inequality, candidate, and materialization caps separately.

### 11.2 Unique strict optimum and inactive rejection

One owner has actions `[A,B]`, with payoffs `(1,0)`. Both CE and CCE have the single feasible vertex `(1,0)`.

The first selected tuple is `[0]`, which sets `p_A=0`. Its candidate `(0,1)` fails a deviation inequality. The next tuple `[1]` yields `(1,0)`. This fixes the first-success and inactive-rejection behavior without relying on Nash tests. CE's unused recommendation is explicitly null.

### 11.3 Unique non-Dirac witness

For two owners with actions `[A,B]`, use profile order `(AA,AB,BA,BB)`. Row-owner payoffs are `(1,-1,-1,1)`. Column-owner payoffs are their negatives.

Write masses `(x,y,z,w)`. CE inequalities give `x >= y`, `w >= z`, `z >= x`, and `y >= w`. Thus all four masses equal `1/4`. CCE has the same inequalities for this binary game, with its own row order. Its complete vertex list is also `{(1/4,1/4,1/4,1/4)}`.

Test both solver entry points independently. In each mode, the selected tuple is `[4,5,6]`, at candidate 53. A feasible basis cannot include nonnegativity because every feasible mass is positive. The first three deviation equalities already force all masses equal. Among the 56 three-element subsets of eight rows, only three tuples follow `[4,5,6]`. This derives candidate 53 without executing a model. Do not reuse the existing product-mixed-Nash test as this oracle.

### 11.4 Zero-payoff tie break

For the same two-owner layout, set all eight payoffs to zero. In each mode, the feasible set is the probability simplex. Its complete vertex list is `e_0,e_1,e_2,e_3`.

The initial selected tuple `[0,1,2]` succeeds with `e_3 = (0,0,0,1)`. Preserve zero and redundant deviation rows. CE must include null recommendations. The returned point does not maximize `p_0`, whose maximum occurs at `e_0`. First active-set success therefore makes no global witness-optimization claim.

### 11.5 Degenerate coordination polytope

Give both owners payoff one on `AA` and `BB`, and zero on `AB` and `BA`. CE and binary CCE require `x >= y`, `x >= z`, `w >= y`, and `w >= z`.

The complete vertex list is:

\[
(1,0,0,0),\ (0,0,0,1),\ (1,1,0,1)/3,\
(1,0,1,1)/3,\ (1,1,1,1)/4.
\]

For completeness, remove the excess of `x` over `w`, or conversely, as a diagonal ray. The remaining cone has `x=w=t` and `(y,z)` in `[0,t]^2`. The square's corner rays yield the last three vertices. Its `(0,0)` corner ray is the sum of the two diagonal rays. Thus there are no other extreme rays or normalized vertices.

Test redundant rows, several bases for one vertex, and first success `e_3`. Verify the complete tiny set independently, without requiring the public operation to continue after success.

### 11.6 CE and CCE differ

Use owners `[Row,Column]`, Row actions `[A,B,C]`, and Column actions `[L,R]`. Row payoffs in profile order `(AL,AR,BL,BR,CL,CR)` are `(0,0,1,-1,0,0)`. All Column payoffs are zero.

With one-based coordinate vectors `e_1,...,e_6` for this paragraph, the complete CE vertex list is

\[
e_2,e_3,e_6,(e_1+e_2)/2,(e_3+e_4)/2,(e_5+e_6)/2.
\]

The complete CCE vertex list is

\[
e_2,e_3,e_6,(e_1+e_2)/2,(e_1+e_6)/2,
(e_5+e_2)/2,(e_5+e_6)/2,(e_3+e_4)/2.
\]

To derive CE, split the three action pairs. Its nontrivial conditions are `p_AR >= p_AL`, `p_BL >= p_BR`, and `p_CR >= p_CL`. Each pair cone has a positive-coordinate ray and an equal-pair ray.

CCE instead requires `p_BL >= p_BR` and `p_AR+p_CR >= p_AL+p_CL`. The latter cone has two positive-coordinate rays and four equal positive/negative pair rays. These disjoint cones give the stated complete normalized lists.

The mass `(e_1+e_6)/2` is CCE but not CE. Both public first-success witnesses are `e_6` because the initial nonnegativity basis succeeds. Therefore checking only their returned witnesses would not distinguish the two feasible sets.

For each mode, require the actual private production constraint builder to generate this game's ordered rows. Evaluate those rows at every vertex in both lists and at `(e_1+e_6)/2`. Compare every ordered row evaluation against the separately labelled payoff evaluator. Include all nonnegativity, duplicate, and zero rows. Oracle agreement with itself, public first witnesses, or existing checker tests alone cannot satisfy this requirement.

Row indices remain zero-based, as in §4. In both modes, rows `0 .. 5` evaluate to `(p_AL,p_AR,p_BL,p_BR,p_CL,p_CR)`. The remaining ordered evaluations must be:

| Mode | Row | Label | Evaluation |
|---|---:|---|---|
| CE | 6 | Row, A to B | `-p_AL+p_AR` |
| CE | 7 | Row, A to C | `0` |
| CE | 8 | Row, B to A | `p_BL-p_BR` |
| CE | 9 | Row, B to C | `p_BL-p_BR` |
| CE | 10 | Row, C to A | `0` |
| CE | 11 | Row, C to B | `-p_CL+p_CR` |
| CE | 12 | Column, L to R | `0` |
| CE | 13 | Column, R to L | `0` |
| CCE | 6 | Row, alternative A | `p_BL-p_BR` |
| CCE | 7 | Row, alternative B | `-p_AL+p_AR-p_CL+p_CR` |
| CCE | 8 | Row, alternative C | `p_BL-p_BR` |
| CCE | 9 | Column, alternative L | `0` |
| CCE | 10 | Column, alternative R | `0` |

At the separating mass, require CE rejection: row 6 is `-1/2`. Require CCE admission: every deviation row is zero and all masses are nonnegative. Preserve Column's zero rows and all null CE recommendations. These tests need neither a public hook nor a mandatory all-vertices production API.

### 11.7 Private linear-system controls

Use the same private elimination and traversal, without exposing them publicly:

| Input | Required result |
|---|---|
| Normalization `(1,1 | 1)` and `(0,0 | 0)` | Consistent rank deficiency. |
| Normalization `(1,1 | 1)` and `(1,1 | 0)` | Inconsistency, not rank deficiency. |
| A missing early-column pivot with a later nonzero column | Search continues left to right and classifies after all columns. |
| Two eligible nonzero rows below the pivot position | First eligible row wins. Other row order remains unchanged. |
| Normalization on three variables plus `(0,1,16 | 0)` and `(0,16,256 | 0)` | Observe the product `16*16 = 256` before the zero residual. Consistent rank deficiency. |
| Normalization on three variables plus `(0,1/16,1 | 0)` and `(0,16,1 | 0)` | Initial peak six bits. The second pivot produces `256`, with ten bits, before residual `-255`, with nine bits. The unique solution is `(1,0,0)`. |
| Two-variable inequalities `x>=0`, `y>=0`, `-x-y>=0`, with `x+y=1` | Three one-row active tuples complete unsuccessfully. First two violate the last inequality. Last tuple is inconsistent. |

For the cancellation control, `size(256)=10` and `size(0)=2`. Historical observation must retain ten bits after the candidate is discarded. In the unique-system control, normalization by `1/16` makes the third pivot-row coefficient `16`. Eliminating the other row multiplies it by factor `16`. A Rational cap of nine therefore fails during elimination, despite six-bit inputs and a two-bit final solution. A cap of ten admits that system's Rational history. Keep all other limits sufficient. The private value `1/16` has combined size six rather than Feedback's five. Test divisions, negative numerators, zero, one, and products that later cancel. Do not count hidden integer products inside a Rational operation as observed Rationals.

### 11.8 Resource boundaries and competing failures

Build a separate ledger oracle from the block table, not by reading a successful production report and declaring it authoritative. For each fixture, derive the exact visited block sequence from its independent tiny basis calculation. Sum the stated reservations with uncapped arithmetic only in the tiny test oracle. Derive Rational peaks by separate exact arithmetic in the same declared operation order.

For each mode and each relevant phase, test these boundaries:

- A cap equal to the required inequality count, candidate prefix count, cumulative work, cumulative materialization, or observed Rational peak admits that boundary.
- One below fails at the first crossing, with `cap+1`, the correct phase/resource, and no solution.
- Materialization and work crossing in the same block selects materialization.
- An admitted block whose Rational output exceeds its cap selects Rational failure. If that block's work also fails, work wins before arithmetic.
- A next-candidate cap failure never reaches elimination. Final successor completion does not consume an extra candidate credit.
- A successor reservation failure after the last rejected private candidate is exhaustion, not completed infeasibility.
- A final constructor, actual-checker, or publication reservation failure returns no previously eligible candidate.
- Retain public Rational admission and elimination boundary controls. Tight input-payoff bits fail in admission. Tight canceled-intermediate bits fail during elimination. The concrete verification Rational crossing is the source-bound private CE shadow control in §11.10, not a public first-witness claim.
- Test aggregate report-length admission and its competing failures through the private production geometry logic in §11.9.
- A zero CE row count still validates the literal device and calls the real CE checker.
- Rejected-candidate reservations and historical bits remain in the final successful accounting.

Report limits depend on the fixed reservations and the explicit `q <= I-1` representation check, not physical allocations. Work reservations depend on configured `B`. When varying `B`, recompute work reservations or keep the work cap independently sufficient. Otherwise a test can correctly encounter work before Rational exhaustion.

Retain public verification work, materialization, and publication boundary tests. Do not require a CCE verification-only Rational crossing without a new arithmetic sequence. Constructor/device totals repeat normalization. CCE differences, products, and slack prefixes repeat the already admitted constraint and inequality sequences. CE recommendation-subset sums need the separate private control below.

For unmodified-call coverage, add private test instrumentation in the test build, not a public callback. Record the source-level Rational operands/results of the constructor/checker bodies. Compare each call's ordered arithmetic-result sequence with its corresponding shadow sequence, including zero-mass entries and null recommendations. Compare constructor and device-validation sequences separately. Compare every actual report field. This tests the coverage proof directly without claiming that a report exposes discarded arithmetic.

Add compile-fail controls for solution construction, record update, owner/action coercion, and CE/CCE interchange. Preserve all existing mixed-game boundary tests. Reordered layouts must change enumeration consistently. Games with the same layout but different payoffs must retain their own game handle and pass their own independent evaluator.

### 11.9 Arithmetic-only report-length admission

Test the same private scalar geometry-admission logic that production uses after its count fold. Do not implement a test-only substitute for the inequality/report gates. Use `Natural` scalar inputs. Reserve the existing geometry block before its gates, as in §7. Keep preceding admission outside these arithmetic-only controls.

The following synthetic gate inputs test the exact boundary. They do not claim that every supplied `(n,q)` pair describes a constructor-admitted game.

| `n` | `q` | Inequality cap | Expected geometry result |
|---:|---:|---:|---|
| 1 | `I-1` | `I` | Exact `m=I` fits. Report length fits at its ceiling. |
| 1 | `I` | `I+1` | Exact `m=I+1` fits. Return `CorrelationSolveRepresentationExceeded CorrelationReportLength`. |
| 1 | `I` | `I` | `q` fits but `m` exceeds the inequality cap. Return the inequality failure, not report-length failure. |
| 1 | `I` | `I-1` | `q` already exceeds the inequality cap. Return the inequality failure without using its saturation sentinel as an exact report count. |

Keep geometry work and materialization sufficient for those four controls. Add competing failures through the same reserved production route. Insufficient materialization wins before work or either geometry gate. With materialization admitted, insufficient work wins before either geometry gate. With both admitted, inequality failure wins before report-length failure. With inequalities admitted, report-length failure wins before a zero/one constant would exceed `B < 2`. At the admitted `q=I-1` boundary, that constant Rational failure can occur next.

Also bind these aggregate-count counterexamples to the production count formulas and geometry gate:

- **CE, 64-bit `Int`:** `I=2^63-1`. One owner with `a=2^32` actions has `n=a`, `q=a(a-1)=2^64-2^32`, and `m=2^64`. Owner, local-action, and profile counts individually fit `I-1`. With inequality cap `m`, the report-length gate must reject `q`.
- **CCE:** let `r=I-1`. Use two binary carriers and singleton carriers for the other owners. Then `n=4`, `L=2+2+(r-2)=r+2=I+1`, `q=I+1`, and `m=I+5`. All individual owner/action/profile lengths fit `I-1`. With inequality cap `m`, the report-length gate must reject `q`.

Compute these scalar expressions directly in future arithmetic-helper tests. For CCE, derive the singleton contribution algebraically, not by traversing `I-3` singleton entries. Inspect the source route to confirm that production's CE count fold computes `sum_i a_i(a_i-1)` and its CCE fold computes `L`. Confirm that both routes call this same gate before constants or materialization. No compressed-carrier production API is needed.

These are arithmetic-helper and source-route controls. Do not construct huge carriers, games, report lists, or row lists. Do not describe them as execution of those giant games. This draft adopts the algebra by inspection only. No executable arithmetic model ran.

### 11.10 Source-bound private shadow Rational control

Use a zero-payoff game with owners `[Row,Column]`, Row actions `[A,B]`, and Column actions `[L,R]`. Profile order is `(AL,AR,BL,BR)`. Inject the complete candidate masses `(1/3,1/6,1/5,3/10)` only into a private test route. This is not the public solver's selected candidate. Public zero-game search returns the last-profile Dirac witness `(0,0,0,1)`.

Prepare the private history explicitly. Admit the zero game and constants. Use the actual production constraint builder for each mode. Observe the injected masses in profile order. Run the declared candidate normalization and all ordered inequality folds, without elimination. Carry that preparation's cumulative account into the actual private production shadow. Do not reset the account, invent a public elimination prefix, or assert that such a prefix fits seven bits.

The independent labelled arithmetic trace is:

| Preparation sequence | Ordered values | Combined sizes |
|---|---|---|
| Input masses | `1/3, 1/6, 1/5, 3/10` | `3, 4, 4, 6` |
| Full normalization prefixes from zero | `1/3, 1/2, 7/10, 1` | `3, 3, 7, 2` |
| Nonnegativity-row products and prefixes | Zeros and the respective coordinate mass | At most 6 |
| Payoffs and deviation differences | All zero | 2 |
| Deviation-row products and slack prefixes | All zero | 2 |

The normalization arithmetic is `1/3+1/6=1/2`, `1/2+1/5=7/10`, and `7/10+3/10=1`. Thus mass observation peaks at six bits and preparation ends with historical `H=7`. Constants and nonnegativity coefficients have size two. No preparation step uses an eliminated matrix.

The shadow first repeats the full normalization sequence for constructor coverage and device validation. Those repeated prefixes still fit seven bits. CE then visits the following recommendation folds in its actual owner/recommendation/alternative order:

| CE row | Matching masses | Ordered recommendation prefixes | Combined sizes |
|---|---|---|---|
| Row, A to B | `1/3, 1/6` | `1/3, 1/2` | `3, 3` |
| Row, B to A | `1/5, 3/10` | `1/5, 1/2` | `4, 3` |
| Column, L to R | `1/3, 1/5` | `1/3, 8/15` | `3, 8` |
| Column, R to L | `1/6, 3/10` | `1/6, 7/15` | `4, 7` |

For Column L, `1/3+1/5=5/15+3/15=8/15`. The reduced numerator and denominator each have four bits, so the combined size is eight. This is the first new historical peak after preparation. For Column R, `1/6+3/10=5/30+9/30=7/15`, whose combined size is seven. All recommendation totals are positive. Every payoff difference, checker slack product, and checker slack prefix is zero in both modes.

Through the actual private production CE shadow, `B=7` must fail at the Column-L recommendation addition. The error is `CorrelationSolveLimitExceeded CorrelationVerification CorrelationRationalBits 7 8`. With `B=8`, the same CE shadow history fits and reaches `H=8`. The corresponding private production CCE shadow has no recommendation-subset folds. Its history fits `B=7` and retains `H=7`.

Keep work and materialization independently sufficient for preparation and the shadow under each chosen `B`. Derive their reservations from the actual private route and the unchanged block table. Do not borrow a public active-set prefix's charges. Record an independent ordered arithmetic trace and compare it with the source-bound preparation and production-shadow trace. A test of a separately reimplemented shadow alone is insufficient. Add no public injection hook.

This fixture proves a private CE recommendation-prefix boundary and the corresponding CCE mode distinction. It does not prove public first-witness reachability, a seven-bit public elimination history, or a CCE verification-only bit crossing. Retain the public boundary controls described in §11.8.

## 12. Freeze provenance and implementation gate

This freeze adopts `/tmp/d083-contract-i6mo3jv7/CONTRACT.md`, SHA256 `60d23acceaeb731c3de713540ec0d051c0aa67d197f496a96bb91daa0f36ac30`. Parent design acceptance is `/tmp/d083-parent-design-acceptance.md`. Its identity audit is `/tmp/d083-parent-contract-audit-2wn84rxm/result.json`.

The independent DESIGN PASS is `/home/josephcox/.pi/agent/sessions/--home-josephcox-dev-Markovian--/subagent-artifacts/outputs/90faab78-836d-4d11-8d03-1448084fa4b8/d083-contract-revision-review.md`, SHA256 `be991048983e5ee88deb9777f3b0a804883d91765578c7059e2606b81ad2a0e3`. Only freeze and provenance metadata differ from the selected source. The original draft, DESIGN BLOCK, and failed parent audit helpers remain historical inputs, not successful executions.

D083 remains Proposed. The solver is unimplemented and unaccepted. Required solver tests, source-loop and reservation proofs, strictness proofs, and fresh implementation review remain future work. Arithmetic design review is not executable proof. Private fixtures do not establish public reachability. Separate parent authorization must precede implementation. Separate reviewed capability acceptance must follow implementation evidence.
