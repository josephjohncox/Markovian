{-# LANGUAGE RoleAnnotations #-}

{- | Private D083 solver core for exact CE and CCE first-witness search.

The library keeps this module in @other-modules@. Standalone source probes
exercise its constraint builder, tuple traversal, Gauss-Jordan elimination,
geometry admission, cumulative ledger, and verification shadow.

Reservations here are conservative represented-work and logical-field blocks.
They bound represented work, logical fields, and observed 'Rational' sizes.
They are not a bound on physical time, heap bytes, stack bytes, or the
arithmetic allocation hidden inside @Data.Ratio@.
-}
module Markovian.Game.Correlated.Exact.Internal (
    -- * Configured solve limits
    CorrelationSolveLimits (..),
    correlationSolveLimits,

    -- * Failure vocabulary
    CorrelationSolvePhase (..),
    CorrelationSolveResource (..),
    CorrelationRepresentation (..),
    CorrelationSolveInvariant (..),
    SolveFault (..),

    -- * Cumulative account
    CorrelationSolveAccounting (..),
    emptyAccount,

    -- * Ledger primitives
    Solve,
    runSolve,
    abort,
    readAccount,
    writeAccount,
    reserveBlock,
    reserveSpine,
    creditCandidate,
    observeRational,
    recordCovered,
    representationCeiling,
    blockFields,
    blockWork,

    -- * Scalar geometry admission
    SolveMode (..),
    obedienceRowCount,
    coarseRowCount,
    admitGeometry,

    -- * Admission
    admitGame,

    -- * Constraint construction
    ConstraintRow (..),
    RowLabel (..),
    buildConstraints,
    deviationLabels,

    -- * Deterministic traversal
    initialTuple,
    successorTuple,

    -- * Elimination
    Classification (..),
    candidateMatrix,
    eliminate,
    classify,
    verifyCandidate,

    -- * Verification shadow
    ShadowRow (..),
    correlatedShadow,
    coarseShadow,

    -- * Bounded helpers
    boundedRationalSize,
    naturalCount,
    carrierValues,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio (denominator, numerator)
import Markovian.Category.Finite.Object
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Profile.Finite
import Numeric.Natural (Natural)

{-# ANN module ("HLint: ignore Use when" :: String) #-}

-- Keep the explicit folds and recursive traversal used by the work schedule.
{-# ANN module ("HLint: ignore Use sum" :: String) #-}
{-# ANN module ("HLint: ignore Use foldl" :: String) #-}

-- | Solve-specific caps around the shared 'GameLimits'.
data CorrelationSolveLimits = CorrelationSolveLimits
    { correlationSolveGameLimits' :: !GameLimits
    , maximumCorrelationSolveInequalities' :: !Natural
    , maximumCorrelationSolveCandidates' :: !Natural
    , maximumCorrelationSolveMaterialization' :: !Natural
    }
    deriving (Eq, Show)

{- | Construct solve limits.  Performs no game traversal and supplies no
defaults.  Every cap is inclusive and zero is valid.
-}
correlationSolveLimits :: GameLimits -> Natural -> Natural -> Natural -> CorrelationSolveLimits
correlationSolveLimits = CorrelationSolveLimits

-- | Ordered solve phases.
data CorrelationSolvePhase
    = CorrelationAdmission
    | CorrelationConstraints
    | CorrelationCombination
    | CorrelationElimination
    | CorrelationInequalities
    | CorrelationVerification
    | CorrelationPublication
    deriving (Eq, Show)

-- | Metered resources.
data CorrelationSolveResource
    = CorrelationInequalityCount
    | CorrelationCandidateCount
    | CorrelationMaterialization
    | CorrelationWork
    | CorrelationRationalBits
    deriving (Eq, Show)

-- | Representational length ceilings.
data CorrelationRepresentation
    = CorrelationOwnerLength
    | CorrelationChoiceLength
    | CorrelationProfileLength
    | CorrelationReportLength
    | CorrelationRationalLength
    deriving (Eq, Show)

-- | Internal consistency failures.
data CorrelationSolveInvariant
    = CorrelationInputLayoutInvariant
    | CorrelationConstraintLayoutInvariant
    | CorrelationCandidateShapeInvariant
    | CorrelationShadowVerificationInvariant
    | CorrelationCheckerDisagreement
    | CorrelationCompletedSearchWithoutWitness
    deriving (Eq, Show)

{- | Faults the private core can raise.  The public module maps these onto the
frozen public error type and adds its own device and checker cases, so the
private core needs no import of the public module.
-}
data SolveFault owner
    = SolveProductFault !(OwnedProductError owner)
    | SolveLimitFault
        !CorrelationSolvePhase
        !CorrelationSolveResource
        !Natural
        !Natural
    | SolveRepresentationFault !CorrelationRepresentation
    | SolveInvariantFault !CorrelationSolveInvariant
    deriving (Eq, Show)

type role SolveFault nominal

{- | The cumulative account @(W,C,H,V,t,d,i,v)@.  @C@ is cumulative, not a
live-memory maximum, and reservations are never refunded.
-}
data CorrelationSolveAccounting = CorrelationSolveAccounting
    { correlationSolveReservedWork' :: !Natural
    , correlationSolveReservedMaterialization' :: !Natural
    , correlationSolveObservedRationalBits' :: !Natural
    , correlationSolveCheckerCoveredRationalBits' :: !Natural
    , correlationSolveCandidates' :: !Natural
    , correlationSolveRankDeficientCandidates' :: !Natural
    , correlationSolveInconsistentCandidates' :: !Natural
    , correlationSolveInequalityRejectedCandidates' :: !Natural
    , correlationSolveSelectedInequalities' :: ![Natural]
    }
    deriving (Eq, Show)

-- | The zero account.
emptyAccount :: CorrelationSolveAccounting
emptyAccount = CorrelationSolveAccounting 0 0 0 0 0 0 0 0 []

{- | A strict state/error thread over the single cumulative account.  Every
transition forces the account so that no rejected candidate is retained through
an unevaluated closure.
-}
newtype Solve owner value
    = Solve (CorrelationSolveAccounting -> Either (SolveFault owner) (value, CorrelationSolveAccounting))

-- | Run a solve thread from the zero account.
runSolve :: Solve owner value -> Either (SolveFault owner) (value, CorrelationSolveAccounting)
runSolve (Solve step) = step emptyAccount

instance Functor (Solve owner) where
    fmap function (Solve step) = Solve $ \account -> case step account of
        Left problem -> Left problem
        Right (value, next) -> Right (function value, next)

instance Applicative (Solve owner) where
    pure value = Solve $ \account -> Right (value, account)
    Solve left <*> Solve right = Solve $ \account -> case left account of
        Left problem -> Left problem
        Right (function, middle) -> case right middle of
            Left problem -> Left problem
            Right (value, next) -> Right (function value, next)

instance Monad (Solve owner) where
    Solve step >>= continue = Solve $ \account -> case step account of
        Left problem -> Left problem
        Right (value, next) ->
            let Solve following = continue value
             in following next

-- | Fail the whole solve.
abort :: SolveFault owner -> Solve owner value
abort problem = Solve $ \_ -> Left problem

-- | Read the current account.
readAccount :: Solve owner CorrelationSolveAccounting
readAccount = Solve $ \account -> Right (account, account)

-- | Replace the account, forcing its counters.
writeAccount :: CorrelationSolveAccounting -> Solve owner ()
writeAccount account = Solve $ \_ -> account `seq` Right ((), account)

{- | The chosen representational length ceiling, @fromIntegral (maxBound :: Int)
- 1@.
-}
representationCeiling :: Natural
representationCeiling = fromIntegral (maxBound :: Int) - 1

{- | @F(d) = 1024 d^6@, evaluated with capped multiplication under the supplied
cap.  Never constructs an unrestricted power.
-}
blockFields :: Natural -> Natural -> Natural
blockFields cap dimension =
    let step accumulator = cappedGameProduct cap accumulator dimension
     in cappedGameProduct cap 1024 (step (step (step (step (step (step 1))))))

{- | @R(d) = F(d)(B+1)@, evaluated with capped multiplication under the work
cap.  Each cap saturates independently, so @F@ is recomputed under the work cap
rather than reusing the materialization sentinel.
-}
blockWork :: Natural -> Natural -> Natural -> Natural
blockWork cap bits dimension =
    cappedGameProduct cap (blockFields cap dimension) (cappedGameAdd cap bits 1)

{- | Reserve one block before any traversal, arithmetic, or materialization.
Materialization is checked first, then work; both commit only if both fit.
-}
reserveBlock :: CorrelationSolveLimits -> CorrelationSolvePhase -> Natural -> Solve owner ()
reserveBlock limits phase dimension =
    reserveExact
        limits
        phase
        (blockFields (materializationCap limits) dimension)
        (blockWork (workCap limits) (maximumGameRationalBits (correlationSolveGameLimits' limits)) dimension)

{- | Reserve the admission spine block, which is @(16,0)@ rather than the
polynomial schedule.
-}
reserveSpine :: CorrelationSolveLimits -> Solve owner ()
reserveSpine limits = reserveExact limits CorrelationAdmission 0 16

-- | Reserve explicit deltas, materialization before work.
reserveExact :: CorrelationSolveLimits -> CorrelationSolvePhase -> Natural -> Natural -> Solve owner ()
reserveExact limits phase fields work = do
    account <- readAccount
    let cmax = materializationCap limits
        wmax = workCap limits
        nextFields = cappedGameAdd cmax (correlationSolveReservedMaterialization' account) fields
        nextWork = cappedGameAdd wmax (correlationSolveReservedWork' account) work
    if nextFields > cmax
        then abort (SolveLimitFault phase CorrelationMaterialization cmax (cmax + 1))
        else
            if nextWork > wmax
                then abort (SolveLimitFault phase CorrelationWork wmax (wmax + 1))
                else
                    writeAccount
                        account
                            { correlationSolveReservedMaterialization' = nextFields
                            , correlationSolveReservedWork' = nextWork
                            }

{- | Charge one candidate credit.  Called only after a tuple is known to exist
inside its reserved combination block and before that tuple is exposed.
-}
creditCandidate :: CorrelationSolveLimits -> Solve owner ()
creditCandidate limits = do
    account <- readAccount
    let cap = maximumCorrelationSolveCandidates' limits
        next = cappedGameAdd cap (correlationSolveCandidates' account) 1
    if next > cap
        then abort (SolveLimitFault CorrelationCombination CorrelationCandidateCount cap (cap + 1))
        else writeAccount account{correlationSolveCandidates' = next}

materializationCap :: CorrelationSolveLimits -> Natural
materializationCap = maximumCorrelationSolveMaterialization'

workCap :: CorrelationSolveLimits -> Natural
workCap = maximumGameWork . correlationSolveGameLimits'

{- | Observe one source-level 'Rational'.  The configured bound is enforced
first, then the representational ceiling, so a value failing both reports the
configured 'Rational' limit.  The observer is bounded: it stops as soon as a
bound is exceeded and never finishes scanning an arbitrarily large input to
report an exact size.
-}
observeRational :: CorrelationSolveLimits -> CorrelationSolvePhase -> Rational -> Solve owner ()
observeRational limits phase value = do
    account <- readAccount
    let bits = maximumGameRationalBits (correlationSolveGameLimits' limits)
        observed = boundedRationalSize (min bits representationCeiling) value
    if observed > bits
        then abort (SolveLimitFault phase CorrelationRationalBits bits (bits + 1))
        else
            if observed > representationCeiling
                then abort (SolveRepresentationFault CorrelationRationalLength)
                else
                    writeAccount
                        account
                            { correlationSolveObservedRationalBits' =
                                max (correlationSolveObservedRationalBits' account) observed
                            }

{- | Raise @V@, the maximum size covered by the shadow's sequence-equality
proof.  This is not a claim that an unmodified call emitted tracing events.
-}
recordCovered :: Natural -> Solve owner ()
recordCovered observed = do
    account <- readAccount
    writeAccount
        account
            { correlationSolveCheckerCoveredRationalBits' =
                max (correlationSolveCheckerCoveredRationalBits' account) observed
            }

{- | Combined numerator/denominator bit size, matching the game's convention,
with a scan that stops once @bound@ is exceeded.
-}
boundedRationalSize :: Natural -> Rational -> Natural
boundedRationalSize bound value =
    let numeratorBits = boundedIntegerBits bound (abs (numerator value))
     in if numeratorBits > bound
            then numeratorBits
            else numeratorBits + boundedIntegerBits (bound - numeratorBits) (denominator value)

boundedIntegerBits :: Natural -> Integer -> Natural
boundedIntegerBits bound = go 1
  where
    go seen number
        | number < 2 = seen
        | seen > bound = seen
        | otherwise = go (seen + 1) (number `quot` 2)

-- | Which family of inequality rows a solve builds.
data SolveMode = CorrelatedMode | CoarseMode
    deriving (Eq, Show)

{- | @q = sum_i a_i(a_i - 1)@ for CE, saturated at the supplied cap.  This is
the production count fold used by both the solver and its private controls.
-}
obedienceRowCount :: Natural -> [Natural] -> Natural
obedienceRowCount cap =
    foldl
        ( \accumulator count ->
            cappedGameAdd cap accumulator (cappedGameProduct cap count (if count == 0 then 0 else count - 1))
        )
        0

-- | @q = L@ for CCE, saturated at the supplied cap.
coarseRowCount :: Natural -> [Natural] -> Natural
coarseRowCount cap = foldl (cappedGameAdd cap) 0

{- | The scalar geometry gate.  Computes the mode's @q@ and @m = n + q@ under
the inequality cap, reports an inequality failure before any report-length
failure, and only then admits @q <= I-1@.  Production and its arithmetic-only
controls share this one route.
-}
admitGeometry :: CorrelationSolveLimits -> SolveMode -> Natural -> [Natural] -> Solve owner (Natural, Natural)
admitGeometry limits mode profiles locals = do
    let cap = maximumCorrelationSolveInequalities' limits
        rows = case mode of
            CorrelatedMode -> obedienceRowCount cap locals
            CoarseMode -> coarseRowCount cap locals
    if rows > cap
        then abort (SolveLimitFault CorrelationConstraints CorrelationInequalityCount cap (cap + 1))
        else do
            let total = cappedGameAdd cap profiles rows
            if total > cap
                then abort (SolveLimitFault CorrelationConstraints CorrelationInequalityCount cap (cap + 1))
                else
                    if rows > representationCeiling
                        then abort (SolveRepresentationFault CorrelationReportLength)
                        else pure (rows, total)

{- | Admission.  Counts the owner carrier, then each action carrier in owner-row
order, then the profile carrier, reserving a spine block before each cons or
terminating nil inspection and checking the configured cap immediately after
each count increment, followed by the representation ceiling.
-}
admitGame ::
    CorrelationSolveLimits ->
    ExactNormalGame owner action ->
    Solve owner (Natural, Natural, [Natural])
admitGame limits game = do
    let limits' = correlationSolveGameLimits' limits
        product_ = normalGameProduct game
        owners = carrierValues (ownedOwners product_)
        rows = ownedProductRows product_
    ownerCount <-
        countSpine
            limits
            owners
            (maximumGameOwners limits')
            (\actual cap -> SolveProductFault (TooManyOwners actual cap))
            CorrelationOwnerLength
    locals <-
        traverseSolve
            ( \(owner, choices) ->
                countSpine
                    limits
                    (carrierValues choices)
                    (maximumGameLocalChoices limits')
                    (\actual cap -> SolveProductFault (TooManyLocalChoices owner actual cap))
                    CorrelationChoiceLength
            )
            rows
    profileCount <-
        countSpine
            limits
            (carrierValues (ownedProfiles product_))
            (maximumGameProfiles limits')
            (\actual cap -> SolveProductFault (ProductCardinalityLimitExceeded actual cap))
            CorrelationProfileLength
    let cardinality = foldl (cappedGameProduct (maximumGameProfiles limits')) 1 locals
    if cardinality /= profileCount || naturalCount rows /= ownerCount
        then abort (SolveInvariantFault CorrelationInputLayoutInvariant)
        else pure ()
    reserveBlock limits CorrelationAdmission (admissionDimension ownerCount locals profileCount)
    case validateOwnedProduct limits' product_ of
        Left problem -> abort (SolveProductFault problem)
        Right () -> pure ()
    mapSolve
        ( \(profile, values) -> do
            reserveBlock limits CorrelationAdmission (admissionDimension ownerCount locals profileCount)
            if playerValuesCarrier values /= ownedOwners product_
                then abort (SolveInvariantFault CorrelationInputLayoutInvariant)
                else pure ()
            mapSolve (\(_, payoff) -> observeRational limits CorrelationAdmission payoff) (playerValueEntries values)
            profile `seq` pure ()
        )
        (normalGamePayoffs game)
    if naturalCount (normalGamePayoffs game) /= profileCount
        then abort (SolveInvariantFault CorrelationInputLayoutInvariant)
        else pure ()
    pure (ownerCount, profileCount, locals)

{- | @d = 1 + r + L + n@, the admission/geometry dimension.  Accumulated in
scalar registers; no materialized dimension list is used.
-}
admissionDimension :: Natural -> [Natural] -> Natural -> Natural
admissionDimension owners locals profiles =
    1 + owners + foldl (+) 0 locals + profiles

{- | Count one carrier spine, reserving @(16,0)@ before each cons or terminating
nil inspection and checking the configured cap and then the representation
ceiling immediately after each increment.
-}
countSpine ::
    CorrelationSolveLimits ->
    [value] ->
    Natural ->
    (Natural -> Natural -> SolveFault owner) ->
    CorrelationRepresentation ->
    Solve owner Natural
countSpine limits values cap fault representation = go values 0
  where
    go [] seen = do
        reserveSpine limits
        pure seen
    go (_ : remaining) seen = do
        reserveSpine limits
        let next = seen + 1
        if next > cap
            then abort (fault next cap)
            else
                if next > representationCeiling
                    then abort (SolveRepresentationFault representation)
                    else go remaining next

-- | Which inequality a stored row represents.
data RowLabel owner action
    = NonnegativityRow !Natural
    | ObedienceRow !owner !action !action
    | CoarseRow !owner !action
    deriving (Eq, Show)

type role RowLabel nominal nominal

-- | One stored inequality @c dot p >= 0@, retaining every coefficient.
data ConstraintRow owner action = ConstraintRow
    { rowLabel :: !(RowLabel owner action)
    , rowCoefficients :: ![Rational]
    }
    deriving (Eq, Show)

type role ConstraintRow nominal nominal

{- | The mode's deviation-row labels, in the existing checker's order.  CE
enumerates owner, recommended action, alternative action, omitting only
self-deviation rows.  CCE enumerates owner and alternative action.
-}
deviationLabels :: (Eq action) => SolveMode -> OwnedProduct owner action -> [RowLabel owner action]
deviationLabels mode product_ = case mode of
    CorrelatedMode ->
        [ ObedienceRow owner recommended alternative
        | (owner, choices) <- ownedProductRows product_
        , recommended <- carrierValues choices
        , alternative <- carrierValues choices
        , alternative /= recommended
        ]
    CoarseMode ->
        [ CoarseRow owner alternative
        | (owner, choices) <- ownedProductRows product_
        , alternative <- carrierValues choices
        ]

{- | Build the fixed row order: @n@ nonnegativity rows in profile order, then
the mode's deviation rows in the existing checker's order.  Zero, duplicate, and
dependent rows are all retained; nothing is sorted or deduplicated.  Rows are
built left to right.
-}
buildConstraints ::
    (Eq owner, Eq action) =>
    CorrelationSolveLimits ->
    SolveMode ->
    ExactNormalGame owner action ->
    Natural ->
    Solve owner [ConstraintRow owner action]
buildConstraints limits mode game dimension = do
    let profiles = carrierValues (ownedProfiles (normalGameProduct game))
        positions = indicesOf profiles
    nonnegativity <-
        traverseSolve
            ( \index -> do
                coefficients <-
                    traverseSolve
                        ( \position -> do
                            reserveBlock limits CorrelationConstraints dimension
                            let value = if position == index then 1 else 0
                            observeRational limits CorrelationConstraints value
                            pure value
                        )
                        positions
                reserveBlock limits CorrelationConstraints dimension
                pure (ConstraintRow (NonnegativityRow index) coefficients)
            )
            positions
    deviation <-
        traverseSolve
            ( \label -> do
                coefficients <- traverseSolve (coefficient label) profiles
                reserveBlock limits CorrelationConstraints dimension
                pure (ConstraintRow label coefficients)
            )
            (deviationLabels mode (normalGameProduct game))
    pure (nonnegativity ++ deviation)
  where
    product_ = normalGameProduct game
    coefficient (ObedienceRow owner recommended alternative) profile = do
        reserveBlock limits CorrelationConstraints dimension
        case profileChoice profile owner of
            Nothing -> abort (SolveInvariantFault CorrelationConstraintLayoutInvariant)
            Just actual
                | actual == recommended -> difference owner alternative profile
                | otherwise -> do
                    observeRational limits CorrelationConstraints 0
                    pure 0
    coefficient (CoarseRow owner alternative) profile = do
        reserveBlock limits CorrelationConstraints dimension
        difference owner alternative profile
    coefficient (NonnegativityRow _) _ =
        abort (SolveInvariantFault CorrelationConstraintLayoutInvariant)
    difference owner alternative profile = do
        replacement <- case replaceChoice product_ owner alternative profile of
            Left _ -> abort (SolveInvariantFault CorrelationConstraintLayoutInvariant)
            Right value -> pure value
        incumbent <- case normalPayoff game owner profile of
            Nothing -> abort (SolveInvariantFault CorrelationConstraintLayoutInvariant)
            Just value -> pure value
        deviating <- case normalPayoff game owner replacement of
            Nothing -> abort (SolveInvariantFault CorrelationConstraintLayoutInvariant)
            Just value -> pure value
        let value = incumbent - deviating
        observeRational limits CorrelationConstraints value
        pure value

{- | The initial @k@-tuple @[0 .. k-1]@.  For @k = 0@ this is the empty tuple,
emitted exactly once.
-}
initialTuple :: Natural -> [Natural]
initialTuple count = go 0
  where
    go index
        | index >= count = []
        | otherwise = index : go (index + 1)

{- | The lexicographic successor over @k@-subsets of @0 .. m-1@.  Scans right to
left for the greatest position with @index_j < m-k+j@, increments it, and
rewrites the suffix with successive integers.  'Nothing' means the family is
complete.  Implemented with bounded linked-list traversal and reversal; no
binomial coefficient is computed and no combination list is allocated.
-}
successorTuple :: Natural -> [Natural] -> Maybe [Natural]
successorTuple bound indices = scan (reverseList indices []) 0
  where
    count = naturalCount indices
    scan [] _ = Nothing
    scan (index : earlier) offset
        -- position j from the right is count-1-offset; its ceiling is m-k+j.
        | index + 1 + offset < bound =
            Just (takeNaturals (count - 1 - offset) indices ++ successors (index + 1) (offset + 1))
        | otherwise = scan earlier (offset + 1)
    successors _ 0 = []
    successors start remaining = start : successors (start + 1) (remaining - 1)

{- | Build one @n@ by @n+1@ augmented matrix.  Row zero is @(1,...,1 | 1)@; the
remaining rows are the selected inequalities in tuple order with right-hand side
zero.  The equality is fixed, not selected from the inequality family.
-}
candidateMatrix ::
    CorrelationSolveLimits ->
    Natural ->
    [ConstraintRow owner action] ->
    [Natural] ->
    Solve owner [[Rational]]
candidateMatrix limits dimension constraints selected = do
    reserveBlock limits CorrelationElimination dimension
    let normalization = replicateNatural (columnsOf constraints) 1 ++ [1]
    selectedRows <-
        traverseSolve
            ( \index -> case nthRow index constraints of
                Nothing -> abort (SolveInvariantFault CorrelationCandidateShapeInvariant)
                Just row -> pure (rowCoefficients row ++ [0])
            )
            selected
    pure (normalization : selectedRows)
  where
    columnsOf [] = 0
    columnsOf (row : _) = naturalCount (rowCoefficients row)

{- | Gauss-Jordan elimination in the frozen order.  Pivots come only from
coefficient columns, never the right-hand side, and every product and
difference is observed, including those known to cancel.  All coefficient-column
searches complete before classification.
-}
eliminate ::
    CorrelationSolveLimits ->
    Natural ->
    Natural ->
    [[Rational]] ->
    Solve owner ([[Rational]], Natural)
eliminate limits dimension variables matrix = go matrix 0 0 0
  where
    go rows pivotRow column pivots
        | column >= variables = pure (rows, pivots)
        | otherwise = do
            reserveBlock limits CorrelationElimination dimension
            case selectPivot rows pivotRow column of
                Nothing -> go rows pivotRow (column + 1) pivots
                Just found -> do
                    let swapped = swapRows pivotRow found rows
                        pivotValue = indexRow (rowAt swapped pivotRow) column
                    observeRational limits CorrelationElimination pivotValue
                    normalized <-
                        traverseSolve
                            ( \entry -> do
                                let quotient = entry / pivotValue
                                observeRational limits CorrelationElimination quotient
                                pure quotient
                            )
                            (rowAt swapped pivotRow)
                    updated <-
                        traverseSolve
                            ( \(index, row) ->
                                if index == pivotRow
                                    then pure row
                                    else do
                                        let factor = indexRow row column
                                        observeRational limits CorrelationElimination factor
                                        traverseSolve
                                            ( \(oldEntry, pivotEntry) -> do
                                                let product_ = factor * pivotEntry
                                                observeRational limits CorrelationElimination product_
                                                let result = oldEntry - product_
                                                observeRational limits CorrelationElimination result
                                                pure result
                                            )
                                            (zip row normalized)
                            )
                            (zip [0 ..] swapped)
                    go (installRow pivotRow normalized updated) (pivotRow + 1) (column + 1) (pivots + 1)
    selectPivot rows position column = search position
      where
        search index
            | index >= naturalCount rows = Nothing
            | indexRow (rowAt rows index) column /= 0 = Just index
            | otherwise = search (index + 1)

-- | How an eliminated matrix classifies.
data Classification
    = Inconsistent
    | RankDeficient
    | FullRank ![Rational]
    deriving (Eq, Show)

{- | Classify an eliminated matrix.  Inconsistency takes precedence over rank
deficiency, and free variables are never assigned.  For full rank the
right-hand side is read in variable order.
-}
classify :: Natural -> Natural -> [[Rational]] -> Classification
classify variables pivots rows
    | any inconsistentRow rows = Inconsistent
    | pivots < variables = RankDeficient
    | otherwise = FullRank (map lastEntry rows)
  where
    inconsistentRow row = all (== 0) (takeNaturals variables row) && lastEntry row /= 0
    lastEntry row = case dropNaturals variables row of
        (value : _) -> value
        [] -> 0

{- | Check a full-rank candidate: normalization by a left fold from zero, then
all @m@ inequalities in row order.  A normalization failure is an invariant
failure; a negative slack rejects the candidate and stops immediately without
checking later rows.
-}
verifyCandidate ::
    CorrelationSolveLimits ->
    Natural ->
    [ConstraintRow owner action] ->
    [Rational] ->
    Solve owner Bool
verifyCandidate limits dimension constraints masses = do
    reserveBlock limits CorrelationInequalities dimension
    total <-
        foldSolve
            ( \accumulator mass -> do
                let next = accumulator + mass
                observeRational limits CorrelationInequalities next
                pure next
            )
            0
            masses
    if total /= 1
        then abort (SolveInvariantFault CorrelationCandidateShapeInvariant)
        else go constraints
  where
    go [] = pure True
    go (row : remaining) = do
        reserveBlock limits CorrelationInequalities dimension
        slack <-
            foldSolve
                ( \accumulator (coefficient, mass) -> do
                    let product_ = coefficient * mass
                    observeRational limits CorrelationInequalities product_
                    let next = accumulator + product_
                    observeRational limits CorrelationInequalities next
                    pure next
                )
                0
                (zip (rowCoefficients row) masses)
        if slack < 0
            then pure False
            else go remaining

-- | One shadow row: recommendation mass for CE, slack for both.
data ShadowRow owner action = ShadowRow
    { shadowLabel :: !(RowLabel owner action)
    , shadowRecommendation :: !Rational
    , shadowSlack :: !Rational
    }
    deriving (Eq, Show)

type role ShadowRow nominal nominal

{- | The CE arithmetic shadow.  It repeats, in order, the exact source-level
'Rational' operations that the unmodified constructor, device validation, and CE
checker will perform, so that each is admitted and observed before those calls
run.  It deliberately uses recommendation filtering and two separate folds
rather than one fused dot product.
-}
correlatedShadow ::
    (Eq owner, Eq action) =>
    CorrelationSolveLimits ->
    ExactNormalGame owner action ->
    [(OwnedProfile owner action, Rational)] ->
    [RowLabel owner action] ->
    Solve owner [ShadowRow owner action]
correlatedShadow limits game entries labels = do
    shadowMasses limits entries
    rows <- traverseSolve (correlatedShadowRow limits game entries) labels
    if any ((< 0) . shadowSlack) rows
        then abort (SolveInvariantFault CorrelationShadowVerificationInvariant)
        else pure rows

{- | The CCE arithmetic shadow.  It has no recommendation-subset folds, so its
covered sequence is the constraint and inequality arithmetic already admitted.
-}
coarseShadow ::
    (Eq owner, Eq action) =>
    CorrelationSolveLimits ->
    ExactNormalGame owner action ->
    [(OwnedProfile owner action, Rational)] ->
    [RowLabel owner action] ->
    Solve owner [ShadowRow owner action]
coarseShadow limits game entries labels = do
    shadowMasses limits entries
    rows <- traverseSolve (coarseShadowRow limits game entries) labels
    if any ((< 0) . shadowSlack) rows
        then abort (SolveInvariantFault CorrelationShadowVerificationInvariant)
        else pure rows

{- | Mass admission and the two separate total sequences: one following the
constructor's order, one following @validateDevice@.  Each requires a literal
total of one.
-}
shadowMasses ::
    CorrelationSolveLimits ->
    [(OwnedProfile owner action, Rational)] ->
    Solve owner ()
shadowMasses limits entries = do
    mapSolve
        ( \(_, mass) ->
            if mass < 0
                then abort (SolveInvariantFault CorrelationShadowVerificationInvariant)
                else do
                    observeRational limits CorrelationVerification mass
                    recordCovered (sizeOf mass)
        )
        entries
    constructorTotal <-
        foldSolve
            ( \accumulator (_, mass) -> do
                let total = accumulator + mass
                observeRational limits CorrelationVerification total
                recordCovered (sizeOf total)
                pure total
            )
            0
            entries
    deviceTotal <-
        foldSolve
            ( \accumulator (_, mass) -> do
                observeRational limits CorrelationVerification mass
                recordCovered (sizeOf mass)
                let total = accumulator + mass
                observeRational limits CorrelationVerification total
                recordCovered (sizeOf total)
                pure total
            )
            0
            entries
    if constructorTotal /= 1 || deviceTotal /= 1
        then abort (SolveInvariantFault CorrelationShadowVerificationInvariant)
        else pure ()

correlatedShadowRow ::
    (Eq owner, Eq action) =>
    CorrelationSolveLimits ->
    ExactNormalGame owner action ->
    [(OwnedProfile owner action, Rational)] ->
    RowLabel owner action ->
    Solve owner (ShadowRow owner action)
correlatedShadowRow limits game entries label = case label of
    ObedienceRow owner recommended alternative -> do
        let matching = [entry | entry <- entries, profileChoice (fst entry) owner == Just recommended]
        recommendation <-
            foldSolve
                ( \accumulator (_, mass) -> do
                    let total = accumulator + mass
                    observeRational limits CorrelationVerification total
                    recordCovered (sizeOf total)
                    pure total
                )
                0
                matching
        slack <- foldSolve (shadowContribution limits game owner alternative) 0 matching
        pure (ShadowRow label recommendation slack)
    _ -> abort (SolveInvariantFault CorrelationShadowVerificationInvariant)

coarseShadowRow ::
    (Eq owner, Eq action) =>
    CorrelationSolveLimits ->
    ExactNormalGame owner action ->
    [(OwnedProfile owner action, Rational)] ->
    RowLabel owner action ->
    Solve owner (ShadowRow owner action)
coarseShadowRow limits game entries label = case label of
    CoarseRow owner alternative -> do
        slack <- foldSolve (shadowContribution limits game owner alternative) 0 entries
        pure (ShadowRow label 0 slack)
    _ -> abort (SolveInvariantFault CorrelationShadowVerificationInvariant)

{- | One contribution site: replacement, incumbent lookup, deviation lookup,
subtraction, multiplication, addition -- in exactly that order.
-}
shadowContribution ::
    (Eq owner, Eq action) =>
    CorrelationSolveLimits ->
    ExactNormalGame owner action ->
    owner ->
    action ->
    Rational ->
    (OwnedProfile owner action, Rational) ->
    Solve owner Rational
shadowContribution limits game owner alternative accumulator (profile, mass) = do
    replacement <- case replaceChoice (normalGameProduct game) owner alternative profile of
        Left _ -> abort (SolveInvariantFault CorrelationShadowVerificationInvariant)
        Right value -> pure value
    incumbent <- case normalPayoff game owner profile of
        Nothing -> abort (SolveInvariantFault CorrelationShadowVerificationInvariant)
        Just value -> pure value
    deviating <- case normalPayoff game owner replacement of
        Nothing -> abort (SolveInvariantFault CorrelationShadowVerificationInvariant)
        Just value -> pure value
    let difference = incumbent - deviating
    observeRational limits CorrelationVerification difference
    recordCovered (sizeOf difference)
    let term = mass * difference
    observeRational limits CorrelationVerification term
    recordCovered (sizeOf term)
    let total = accumulator + term
    observeRational limits CorrelationVerification total
    recordCovered (sizeOf total)
    pure total

sizeOf :: Rational -> Natural
sizeOf = boundedRationalSize representationCeiling

-- Bounded list helpers.  Linked-list indexing is treated as a traversal.

-- | Read a carrier's values as an ordinary bounded list.
carrierValues :: FiniteObject value -> [value]
carrierValues = NonEmpty.toList . finiteObjectValues

indicesOf :: [value] -> [Natural]
indicesOf values = initialTuple (naturalCount values)

-- | Length as a 'Natural', by bounded traversal.
naturalCount :: [value] -> Natural
naturalCount = foldl (\accumulator _ -> accumulator + 1) 0

reverseList :: [value] -> [value] -> [value]
reverseList [] accumulator = accumulator
reverseList (value : remaining) accumulator = reverseList remaining (value : accumulator)

takeNaturals :: Natural -> [value] -> [value]
takeNaturals _ [] = []
takeNaturals count (value : remaining)
    | count == 0 = []
    | otherwise = value : takeNaturals (count - 1) remaining

dropNaturals :: Natural -> [value] -> [value]
dropNaturals _ [] = []
dropNaturals count values@(_ : remaining)
    | count == 0 = values
    | otherwise = dropNaturals (count - 1) remaining

replicateNatural :: Natural -> value -> [value]
replicateNatural count value
    | count == 0 = []
    | otherwise = value : replicateNatural (count - 1) value

nthRow :: Natural -> [value] -> Maybe value
nthRow index values = case dropNaturals index values of
    (value : _) -> Just value
    [] -> Nothing

indexRow :: [Rational] -> Natural -> Rational
indexRow row column = case dropNaturals column row of
    (value : _) -> value
    [] -> 0

rowAt :: [[Rational]] -> Natural -> [Rational]
rowAt rows index = case dropNaturals index rows of
    (row : _) -> row
    [] -> []

swapRows :: Natural -> Natural -> [[Rational]] -> [[Rational]]
swapRows left right rows
    | left == right = rows
    | otherwise =
        let leftRow = rowAt rows left
            rightRow = rowAt rows right
         in [ if index == left
                then rightRow
                else
                    if index == right
                        then leftRow
                        else row
            | (index, row) <- zip [0 ..] rows
            ]

installRow :: Natural -> [Rational] -> [[Rational]] -> [[Rational]]
installRow position replacement rows =
    [if index == position then replacement else row | (index, row) <- zip [0 ..] rows]

traverseSolve :: (value -> Solve owner result) -> [value] -> Solve owner [result]
traverseSolve function = go
  where
    go [] = pure []
    go (value : remaining) = do
        result <- function value
        results <- result `seq` go remaining
        pure (result : results)

mapSolve :: (value -> Solve owner ()) -> [value] -> Solve owner ()
mapSolve function = go
  where
    go [] = pure ()
    go (value : remaining) = function value >> go remaining

foldSolve :: (accumulator -> value -> Solve owner accumulator) -> accumulator -> [value] -> Solve owner accumulator
foldSolve function = go
  where
    go accumulator [] = pure accumulator
    go accumulator (value : remaining) = do
        next <- function accumulator value
        next `seq` go next remaining
