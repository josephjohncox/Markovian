{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- | Closed, checked local-table source and its bounded retained trace engine.
This module is private to the acyclic retained-circuit consumer.
-}
module Markovian.Open.Acyclic.Circuit.Cache.Internal where

import Control.Monad (when)
import Data.Foldable (foldlM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio (denominator, numerator)
import Markovian.Algebra.NonNegativeRational
import Markovian.Algebra.Semiring
import Markovian.Category.Finite.Set
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Deterministic
import Markovian.Category.Matrix.Stochastic
import Markovian.Category.Matrix.Stochastic.Internal (StochasticMatrix (UnsafeStochasticMatrix))
import Markovian.Circuit
import Markovian.Circuit.Interpret.Exact
import Numeric.Natural (Natural)

{- | Closed primitive inputs. Construction of the supplied proof-carrying matrix
precedes the metered request; its complete validation/read trace does not.
-}
type role ExactTablePrimitive nominal nominal nominal

data ExactTablePrimitive (purity :: Purity) source target where
    DeterministicTablePrimitive :: !(DeterministicMatrix NonNegativeRational source target) -> ExactTablePrimitive 'Deterministic source target
    StochasticTablePrimitive :: !(StochasticMatrix NonNegativeRational source target) -> ExactTablePrimitive 'Stochastic source target

-- | Retain a proof-carrying deterministic primitive input.
exactDeterministicPrimitive :: DeterministicMatrix NonNegativeRational source target -> ExactTablePrimitive 'Deterministic source target
exactDeterministicPrimitive = DeterministicTablePrimitive

-- | Retain a proof-carrying stochastic primitive input.
exactStochasticPrimitive :: StochasticMatrix NonNegativeRational source target -> ExactTablePrimitive 'Stochastic source target
exactStochasticPrimitive = StochasticTablePrimitive

-- | The original, unmetered interpreter is an independent denotation oracle.
exactTableInterpreter :: ExactPrimitiveInterpreter ExactTablePrimitive ()
exactTableInterpreter =
    ExactPrimitiveInterpreter
        { interpretDeterministicPrimitive = \_ _ (DeterministicTablePrimitive table) -> Right table
        , interpretStochasticPrimitive = \_ _ (StochasticTablePrimitive table) -> Right table
        }

-- | Closed interpreter version, never a Haskell function identity.
data ExactTableInterpreterIdentity = ExactTableInterpreterV1 deriving (Eq, Show)

-- | Request-wide source limits for local circuit tabulation. Bit caps must be positive.
data CircuitSemanticLimits = CircuitSemanticLimits
    { circuitNodeLimit :: !Natural
    -- ^ Maximum cumulative raw syntax nodes.
    , circuitCellLimit :: !Natural
    -- ^ Maximum cumulative materialized intermediate matrix cells.
    , circuitWorkLimit :: !Natural
    -- ^ Maximum cumulative scalar source operations.
    , circuitPrimitiveCellLimit :: !Natural
    -- ^ Maximum cumulative primitive matrix cells admitted.
    , circuitNumeratorBitLimit :: !Natural
    -- ^ Positive maximum observed Rational numerator bits.
    , circuitDenominatorBitLimit :: !Natural
    -- ^ Positive maximum observed Rational denominator bits.
    }
    deriving (Eq, Show)

-- | Infrastructure caps for resident storage and cumulative executor reservations.
data CircuitCacheLimits = CircuitCacheLimits
    { cacheEntryLimit :: !Natural
    -- ^ Maximum resident complete cache entries.
    , cacheCellLimit :: !Natural
    -- ^ Maximum resident final matrix cells.
    , cacheTraceLimit :: !Natural
    -- ^ Maximum resident ordered source-event slots.
    , cacheExecutorLimit :: !Natural
    -- ^ Maximum cumulative executor reservation, including planning.
    }
    deriving (Eq, Show)

-- | Choose checked source recomputation or retention with ordered trace replay.
data CircuitCacheMode = UncachedCircuitTables | RetainCircuitTables deriving (Eq, Show)

-- | First local source failure, with active bound and first disallowed count.
data CircuitSourceError
    = CircuitInvalidNumeratorBitLimit
    | CircuitInvalidDenominatorBitLimit
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
    deriving (Eq, Show)

-- | Infrastructure admission failure; no source result is claimed.
data CircuitInfrastructureError
    = CircuitCacheEntryLimitExceeded !Natural !Natural
    | CircuitCacheCellLimitExceeded !Natural !Natural
    | CircuitCacheTraceLimitExceeded !Natural !Natural
    | CircuitCacheExecutorLimitExceeded !Natural !Natural
    deriving (Eq, Show)

{- | All source counters are cumulative over local edge occurrences in a request.
Executor dispatch counts and conservative reservations are distinct.
-}
data CircuitCacheReport = CircuitCacheReport
    { cacheSourceNodes :: !Natural
    -- ^ Total admitted raw source syntax nodes.
    , cacheSourceCells :: !Natural
    -- ^ Total admitted intermediate source matrix cells.
    , cacheSourceWork :: !Natural
    -- ^ Executed or replayed scalar source work.
    , cacheSourcePrimitiveCells :: !Natural
    -- ^ Cumulative primitive cells checked at their source positions.
    , cacheMaximumNumeratorBits :: !Natural
    -- ^ Maximum numerator width observed, or zero with no scalars.
    , cacheMaximumDenominatorBits :: !Natural
    -- ^ Maximum denominator width observed, or zero with no scalars.
    , cacheExecutorOperations :: !Natural
    -- ^ Executed planning, metadata, event, scalar and bit-loop operations.
    , cacheExecutorReserved :: !Natural
    -- ^ Conservative executor allowance reserved before materialization.
    , cacheLookupComparisons :: !Natural
    -- ^ Key comparisons across planning and execution lookup passes.
    , cacheConstructions :: !Natural
    -- ^ Occurrences executing the checked source rather than replaying.
    , cacheHits :: !Natural
    -- ^ Occurrences actually replaying a complete retained trace.
    , cacheResidentEntries :: !Natural
    -- ^ Complete entries in the returned owner.
    , cacheResidentCells :: !Natural
    -- ^ Final matrix cells in the returned owner.
    , cacheResidentTraceSlots :: !Natural
    -- ^ Source-event slots in the returned owner.
    }
    deriving (Eq, Show)

emptyReport :: CircuitCacheReport
emptyReport = CircuitCacheReport 0 0 0 0 0 0 0 0 0 0 0 0 0 0

data SourceEvent
    = SourceCharge !Natural
    | SourceRational !Rational
    | SourcePrimitiveCells !Natural
    | SourcePrimitiveEndpoints !Bool !Bool
    deriving (Eq, Show)

data CacheRunError
    = CacheInvalidLimits !CircuitSourceError
    | CacheInfrastructure !CircuitInfrastructureError
    | CacheSource !Int !CircuitSourceError
    deriving (Eq, Show)

data TableRequest source target where
    TableRequest :: !Int -> !Int -> !(Circuit ExactTablePrimitive purity source target) -> TableRequest source target

data CacheKey source target
    = CacheKey
        !ExactTableInterpreterIdentity
        !Int
        !(FiniteSet source)
        !(FiniteSet target)
        !Natural
        !Natural
        !CircuitSemanticLimits

type role TableCache nominal nominal
newtype TableCache source target = TableCache [CacheEntry source target]

data CacheEntry source target
    = CacheEntry
        !(CacheKey source target)
        !(Matrix NonNegativeRational source target)
        ![SourceEvent]
        !Natural
        !Natural

emptyTableCache :: TableCache source target
emptyTableCache = TableCache []

sameKey :: CacheKey source target -> CacheKey source target -> Bool
sameKey (CacheKey li lc ls lt _ _ ll) (CacheKey ri rc rs rt _ _ rl) =
    li == ri && lc == rc && sameFiniteSetLayout ls rs && sameFiniteSetLayout lt rt && ll == rl

-- Planning uses the retained owner's immutable circuit slots. Exact endpoint
-- layout validation is deferred until both storage and source shape admission.
sameShapeKey :: CacheKey source target -> CacheKey source target -> Bool
sameShapeKey (CacheKey li lc _ _ ln lm ll) (CacheKey ri rc _ _ rn rm rl) =
    li == ri && lc == rc && ln == rn && lm == rm && ll == rl

-- A private action closure is assembled only by this closed algebra. No user
-- callback can supply an action, a cost claim, or an unchecked trace.
data Plan (purity :: Purity) source target = Plan
    { planSource :: !(FiniteSet source)
    , planTarget :: !(FiniteSet target)
    , planSourceSize :: !Natural
    , planTargetSize :: !Natural
    , planCells :: !Natural
    , planEvents :: !Natural
    , planScalars :: !Natural
    , planTraversal :: !Natural
    , executePlan :: Work (Matrix NonNegativeRational source target)
    }

data PlannedRequest source target where
    PlannedRequest :: !Int -> !(CacheKey source target) -> !Natural -> !(Plan purity source target) -> PlannedRequest source target

data WorkState = WorkState
    { workLimits :: !CircuitSemanticLimits
    , workReport :: !CircuitCacheReport
    , workRecording :: !Bool
    , workEvents :: ![SourceEvent]
    }

newtype Work value = Work {runWork :: WorkState -> Either CircuitSourceError (value, WorkState)}

instance Functor Work where
    fmap f action = Work $ \state -> do
        (value, next) <- runWork action state
        Right (f value, next)
instance Applicative Work where
    pure value = Work $ \state -> Right (value, state)
    function <*> argument = do
        f <- function
        f <$> argument
instance Monad Work where
    action >>= continuation = Work $ \state -> do
        (value, next) <- runWork action state
        runWork (continuation value) next

sourceFailure :: CircuitSourceError -> Work value
sourceFailure problem = Work $ \_ -> Left problem

executorTick :: Natural -> Work ()
executorTick amount = Work $ \state ->
    let report = workReport state
        updated = report{cacheExecutorOperations = cacheExecutorOperations report + amount}
     in Right ((), state{workReport = updated})

-- Bound bit observation at limit+1. The returned count is also its executed
-- integer-division loop count (zero's single representation bit counts once).
integerBits :: Natural -> Integer -> Natural
integerBits limit integer = go 1 (abs integer)
  where
    go count value
        | count > limit = count
        | value < 2 = count
        | otherwise = go (count + 1) (value `quot` 2)

replaySourceEvent :: CircuitSemanticLimits -> SourceEvent -> CircuitCacheReport -> Either CircuitSourceError CircuitCacheReport
replaySourceEvent limits event report = case event of
    SourceCharge amount ->
        let next = cacheSourceWork report + amount
         in if next > circuitWorkLimit limits
                then Left (CircuitSourceWorkLimitExceeded (circuitWorkLimit limits) (circuitWorkLimit limits + 1))
                else Right report{cacheSourceWork = next}
    SourcePrimitiveCells amount ->
        let next = cacheSourcePrimitiveCells report + amount
         in if next > circuitPrimitiveCellLimit limits
                then Left (CircuitSourcePrimitiveCellLimitExceeded (circuitPrimitiveCellLimit limits) (circuitPrimitiveCellLimit limits + 1))
                else Right report{cacheSourcePrimitiveCells = next}
    SourcePrimitiveEndpoints sourceMatches targetMatches
        | not sourceMatches -> Left CircuitSourcePrimitiveSourceMismatch
        | not targetMatches -> Left CircuitSourcePrimitiveTargetMismatch
        | otherwise -> Right report
    SourceRational rational -> do
        let n = integerBits (circuitNumeratorBitLimit limits) (numerator rational)
        when (n > circuitNumeratorBitLimit limits) $ Left (CircuitSourceNumeratorBitLimitExceeded (circuitNumeratorBitLimit limits) n)
        let d = integerBits (circuitDenominatorBitLimit limits) (denominator rational)
        if d > circuitDenominatorBitLimit limits
            then Left (CircuitSourceDenominatorBitLimitExceeded (circuitDenominatorBitLimit limits) d)
            else
                Right
                    report
                        { cacheMaximumNumeratorBits = max n (cacheMaximumNumeratorBits report)
                        , cacheMaximumDenominatorBits = max d (cacheMaximumDenominatorBits report)
                        , cacheExecutorOperations = cacheExecutorOperations report + n + d
                        }

emit :: SourceEvent -> Work ()
emit event = Work $ \state -> do
    let report = workReport state
        recording = workRecording state
        counted = report{cacheExecutorOperations = cacheExecutorOperations report + 1 + if recording then 1 else 0}
    checked <- replaySourceEvent (workLimits state) event counted
    let events = if recording then event : workEvents state else workEvents state
    events `seq` Right ((), state{workReport = checked, workEvents = events})

scalar :: NonNegativeRational -> Work NonNegativeRational
scalar value = do
    emit (SourceCharge 1)
    executorTick 1
    emit (SourceRational (getNonNegativeRational value))
    pure value

productSet :: FiniteSet left -> FiniteSet right -> FiniteSet (left, right)
productSet (UnsafeFiniteSet left) (UnsafeFiniteSet right) = UnsafeFiniteSet [(a, b) | a <- left, b <- right]

unitSet :: FiniteSet ()
unitSet = UnsafeFiniteSet [()]

forceMatrix :: Matrix NonNegativeRational source target -> ()
forceMatrix matrix = foldl' (\() row -> foldl' (\() value -> getNonNegativeRational value `seq` ()) () row) () (matrixRows matrix)

buildMatrix :: FiniteSet source -> FiniteSet target -> (source -> target -> Work NonNegativeRational) -> Work (Matrix NonNegativeRational source target)
buildMatrix source target entry = do
    rows <- traverse (\s -> traverse (entry s) (finiteSetValues target)) (finiteSetValues source)
    case matrixFromRows source target rows of
        Left problem -> sourceFailure (CircuitSourceMatrixError problem)
        Right matrix -> forceMatrix matrix `seq` pure matrix

entryAt :: Matrix NonNegativeRational source target -> source -> target -> Work NonNegativeRational
entryAt matrix source target = case matrixEntry matrix source target of
    Nothing -> sourceFailure CircuitSourceInvariantFailure
    Just value -> pure value

structural :: Natural -> Natural -> FiniteSet source -> FiniteSet target -> (source -> target) -> Plan purity source target
structural sourceSize targetSize source target@(UnsafeFiniteSet _) function =
    Plan source target sourceSize targetSize cells (2 * cells) cells (2 * cells + sourceSize + targetSize) $
        buildMatrix source target $
            \s t -> scalar (if function s == t then one else zero)
  where
    cells = sourceSize * targetSize

tablePlan :: Bool -> Natural -> Natural -> Natural -> FiniteSet source -> FiniteSet target -> Matrix NonNegativeRational source target -> Plan purity source target
tablePlan primitive sourceSize targetSize tableCells source target table =
    Plan source target sourceSize targetSize cells (2 * cells + if primitive then 2 else 0) cells traversal $ do
        when primitive $ do
            emit
                ( SourcePrimitiveEndpoints
                    (sameFiniteSetLayout source (matrixSource table))
                    (sameFiniteSetLayout target (matrixTarget table))
                )
            emit (SourcePrimitiveCells cells)
        buildMatrix source target $ \s t -> entryAt table s t >>= scalar
  where
    cells = sourceSize * targetSize
    traversal = cells * (1 + tableCells) + sourceSize + targetSize

composePlan :: Plan leftPurity source middle -> Plan rightPurity middle target -> Plan (JoinPurity leftPurity rightPurity) source target
composePlan left right = Plan source target sourceSize targetSize cells events scalars traversal $ do
    first <- executePlan left
    second <- executePlan right
    buildMatrix source target $ \s t ->
        case finiteSetValues middle of
            [] -> scalar zero
            represented -> foldlM (accumulate first second s t) zero represented
  where
    source = planSource left
    target = planTarget right
    middle = planTarget left
    sourceSize = planSourceSize left
    targetSize = planTargetSize right
    output = sourceSize * targetSize
    ownScalars = output * max 1 (2 * planTargetSize left)
    cells = planCells left + planCells right + output
    events = planEvents left + planEvents right + 2 * ownScalars
    scalars = planScalars left + planScalars right + ownScalars
    traversal = planTraversal left + planTraversal right + output * (1 + planTargetSize left * (planSourceSize left * planTargetSize left + planSourceSize right * planTargetSize right))
    accumulate first second s t total m = do
        a <- entryAt first s m
        b <- entryAt second m t
        productValue <- scalar (a `times` b)
        scalar (total `plus` productValue)

tensorPlan :: Plan leftPurity leftSource leftTarget -> Plan rightPurity rightSource rightTarget -> Plan (JoinPurity leftPurity rightPurity) (leftSource, rightSource) (leftTarget, rightTarget)
tensorPlan left right = Plan source target sourceSize targetSize cells events scalars traversal $ do
    first <- executePlan left
    second <- executePlan right
    buildMatrix source target $ \(ls, rs) (lt, rt) -> do
        a <- entryAt first ls lt
        b <- entryAt second rs rt
        scalar (a `times` b)
  where
    source = productSet (planSource left) (planSource right)
    target = productSet (planTarget left) (planTarget right)
    sourceSize = planSourceSize left * planSourceSize right
    targetSize = planTargetSize left * planTargetSize right
    output = sourceSize * targetSize
    cells = planCells left + planCells right + output
    events = planEvents left + planEvents right + 2 * output
    scalars = planScalars left + planScalars right + output
    traversal = planTraversal left + planTraversal right + output * (1 + planSourceSize left * planTargetSize left + planSourceSize right * planTargetSize right)

convexPlan :: NonEmpty (NonNegativeRational, Plan 'Stochastic source target) -> Plan 'Stochastic source target
convexPlan terms@((_, first) :| _) = Plan source target sourceSize targetSize cells events scalars traversal $ do
    interpreted <- traverse (\(coefficient, plan) -> (coefficient,) <$> executePlan plan) terms
    total <-
        foldlM
            ( \acc (coefficient, _) -> do
                value <- scalar coefficient
                scalar (acc `plus` value)
            )
            zero
            interpreted
    when (total /= one) (sourceFailure CircuitSourceInvariantFailure)
    buildMatrix source target $ \s t -> foldlM (accumulate s t) zero interpreted
  where
    source = planSource first
    target = planTarget first
    sourceSize = planSourceSize first
    targetSize = planTargetSize first
    output = sourceSize * targetSize
    count = fromIntegral (NonEmpty.length terms)
    ownScalars = 2 * count * (1 + output)
    cells = output + sum (fmap (planCells . snd) terms)
    events = 2 * ownScalars + sum (fmap (planEvents . snd) terms)
    scalars = ownScalars + sum (fmap (planScalars . snd) terms)
    traversal = sum (fmap (planTraversal . snd) terms) + output * (1 + sum (fmap (\(_, plan) -> planSourceSize plan * planTargetSize plan) terms))
    accumulate s t total (coefficient, matrix) = do
        value <- entryAt matrix s t
        weighted <- scalar (coefficient `times` value)
        scalar (total `plus` weighted)

-- The planning algebra carries one request-wide executor fuel. Every original
-- support-spine cell (including its terminator) is charged before inspection.
-- Product witnesses retain their separately computed dimensions and stay lazy
-- until infrastructure and source structural reservations both succeed.
newtype Planning value = Planning
    {runPlanning :: Natural -> Either CircuitInfrastructureError (value, Natural)}

instance Functor Planning where
    fmap f action = Planning $ \fuel -> do
        (value, remaining) <- runPlanning action fuel
        Right (f value, remaining)
instance Applicative Planning where
    pure value = Planning $ \fuel -> Right (value, fuel)
    function <*> argument = do
        f <- function
        f <$> argument
instance Monad Planning where
    action >>= continuation = Planning $ \fuel -> do
        (value, remaining) <- runPlanning action fuel
        runPlanning (continuation value) remaining

newtype PlanningArrow (purity :: Purity) source target = PlanningArrow (Planning (Plan purity source target))

planningSize :: Natural -> FiniteSet value -> Planning Natural
planningSize limit object = go 0 (finiteSetValues object)
  where
    go count values = Planning $ \fuel ->
        if fuel == 0
            then Left (CircuitCacheExecutorLimitExceeded limit (limit + 1))
            else case values of
                [] -> Right (count, fuel - 1)
                _ : remaining -> runPlanning (go (count + 1) remaining) (fuel - 1)

planAlgebra :: Natural -> CircuitAlgebra ExactTablePrimitive PlanningArrow CircuitInfrastructureError
planAlgebra limit =
    CircuitAlgebra
        { algebraPrimitive = \_ source target primitive -> wrapped $ case primitive of
            DeterministicTablePrimitive table -> primitivePlan source target (forgetDeterministic table)
            StochasticTablePrimitive table -> primitivePlan source target (forgetStochastic table)
        , algebraDeterministicTable = \table -> wrapped $ do
            let source = deterministicSource table; target = deterministicTarget table
            leaf source target $ \n m -> tablePlan False n m (n * m) source target (forgetDeterministic table)
        , algebraIdentity = \object -> wrapped $ leaf object object $ \n m -> structural n m object object id
        , algebraCompose = \(PlanningArrow left) (PlanningArrow right) -> wrapped (composePlan <$> left <*> right)
        , algebraTensor = \(PlanningArrow left) (PlanningArrow right) -> wrapped (tensorPlan <$> left <*> right)
        , algebraSymmetry = \left right -> wrapped $ do
            n <- sizeOf left
            m <- sizeOf right
            let size = n * m
            pure (structural size size (productSet left right) (productSet right left) (\(a, b) -> (b, a)))
        , algebraAssociate = \a b c -> wrapped $ do
            n <- sizeOf a
            m <- sizeOf b
            k <- sizeOf c
            let size = n * m * k
            pure (structural size size (productSet (productSet a b) c) (productSet a (productSet b c)) (\((x, y), z) -> (x, (y, z))))
        , algebraUnassociate = \a b c -> wrapped $ do
            n <- sizeOf a
            m <- sizeOf b
            k <- sizeOf c
            let size = n * m * k
            pure (structural size size (productSet a (productSet b c)) (productSet (productSet a b) c) (\(x, (y, z)) -> ((x, y), z)))
        , algebraLeftUnitor = \object -> wrapped $ do
            size <- sizeOf object
            pure (structural size size (productSet unitSet object) object snd)
        , algebraLeftUnitorInverse = \object -> wrapped $ do
            size <- sizeOf object
            pure (structural size size object (productSet unitSet object) ((),))
        , algebraRightUnitor = \object -> wrapped $ do
            size <- sizeOf object
            pure (structural size size (productSet object unitSet) object fst)
        , algebraRightUnitorInverse = \object -> wrapped $ do
            size <- sizeOf object
            pure (structural size size object (productSet object unitSet) (,()))
        , algebraCopy = \object -> wrapped $ do
            size <- sizeOf object
            pure (structural size (size * size) object (productSet object object) (\value -> (value, value)))
        , algebraDiscard = \object -> wrapped $ do
            size <- sizeOf object
            pure (structural size 1 object unitSet (const ()))
        , algebraConvexChoice = \terms ->
            wrapped $
                convexPlan
                    <$> traverse
                        (\(coefficient, PlanningArrow action) -> (coefficient,) <$> action)
                        terms
        , algebraWeaken = \(PlanningArrow action) -> wrapped $ do
            plan <- action
            pure
                ( Plan
                    (planSource plan)
                    (planTarget plan)
                    (planSourceSize plan)
                    (planTargetSize plan)
                    (planCells plan)
                    (planEvents plan)
                    (planScalars plan)
                    (planTraversal plan)
                    (executePlan plan)
                )
        }
  where
    wrapped :: Planning (Plan purity source target) -> Either CircuitInfrastructureError (PlanningArrow purity source target)
    wrapped = Right . PlanningArrow
    sizeOf :: FiniteSet value -> Planning Natural
    sizeOf = planningSize limit
    leaf :: FiniteSet source -> FiniteSet target -> (Natural -> Natural -> Plan purity source target) -> Planning (Plan purity source target)
    leaf source target build = do
        n <- sizeOf source
        m <- sizeOf target
        pure (build n m)
    primitivePlan :: FiniteSet source -> FiniteSet target -> Matrix NonNegativeRational source target -> Planning (Plan purity source target)
    primitivePlan source target table = do
        tableRows <- sizeOf (matrixSource table)
        tableColumns <- sizeOf (matrixTarget table)
        leaf source target $ \n m -> tablePlan True n m (tableRows * tableColumns) source target table

-- Reservations bound executed dispatches, all possible admitted bit-loop
-- iterations, and conservative list-matrix traversal. They are not timings.
planExecutorReservation :: CircuitSemanticLimits -> Bool -> Plan purity source target -> Natural
planExecutorReservation limits recording plan =
    16 * planTraversal plan
        + planScalars plan * (1 + circuitNumeratorBitLimit limits + circuitDenominatorBitLimit limits)
        + planEvents plan * (if recording then 3 else 1)

traceExecutorReservation :: CircuitSemanticLimits -> Natural -> Natural
traceExecutorReservation limits slots = slots * (2 + circuitNumeratorBitLimit limits + circuitDenominatorBitLimit limits)

checkInfrastructure :: (Natural -> Natural -> CircuitInfrastructureError) -> Natural -> Natural -> Either CacheRunError ()
checkInfrastructure constructor limit requested
    | requested > limit = Left (CacheInfrastructure (constructor limit (limit + 1)))
    | otherwise = Right ()

planRequests :: CircuitSemanticLimits -> CircuitCacheLimits -> ExactTableInterpreterIdentity -> [TableRequest source target] -> Either CacheRunError ([PlannedRequest source target], Natural)
planRequests semantic infrastructure interpreter = go 0
  where
    go visited [] = Right ([], visited)
    go visited (TableRequest occurrence identity circuit : rest) = do
        checkInfrastructure CircuitCacheExecutorLimitExceeded (cacheExecutorLimit infrastructure) (visited + 1)
        (nodes, PlanningArrow planning) <- case foldCircuitWithNodeLimit (cacheExecutorLimit infrastructure - visited) (planAlgebra (cacheExecutorLimit infrastructure)) circuit of
            Left (CircuitFoldNodeLimitExceeded _ _) -> Left (CacheInfrastructure (CircuitCacheExecutorLimitExceeded (cacheExecutorLimit infrastructure) (cacheExecutorLimit infrastructure + 1)))
            Left (CircuitFoldAlgebraError problem) -> Left (CacheInfrastructure problem)
            Right result -> Right result
        (plan, fuel) <-
            either (Left . CacheInfrastructure) Right $
                runPlanning planning (cacheExecutorLimit infrastructure - visited - nodes)
        let spent = cacheExecutorLimit infrastructure - fuel
            key = CacheKey interpreter identity (planSource plan) (planTarget plan) (planSourceSize plan) (planTargetSize plan) semantic
        (remaining, total) <- go spent rest
        Right (PlannedRequest occurrence key nodes plan : remaining, total)

entryMetadata :: CacheEntry source target -> (CacheKey source target, Natural, Natural)
entryMetadata (CacheEntry key _ _ cells slots) = (key, cells, slots)

-- Preflight simulates insertions using metadata only. No scalar computation or
-- ordered trace materialization is permitted until this completes.
reserveRequests :: CircuitCacheMode -> CircuitSemanticLimits -> CircuitCacheLimits -> Natural -> TableCache source target -> [PlannedRequest source target] -> Either CacheRunError CircuitCacheReport
reserveRequests mode semantic limits planning (TableCache entries) requests = do
    let initial = emptyReport{cacheExecutorOperations = planning, cacheExecutorReserved = planning}
    (metadata, report) <- inspectEntries initial entries
    go metadata report requests
  where
    checkStorage report = do
        checkInfrastructure CircuitCacheEntryLimitExceeded (cacheEntryLimit limits) (cacheResidentEntries report)
        checkInfrastructure CircuitCacheCellLimitExceeded (cacheCellLimit limits) (cacheResidentCells report)
        checkInfrastructure CircuitCacheTraceLimitExceeded (cacheTraceLimit limits) (cacheResidentTraceSlots report)
    checkExecutor report = checkInfrastructure CircuitCacheExecutorLimitExceeded (cacheExecutorLimit limits) (cacheExecutorReserved report)
    inspectEntries report [] = Right ([], report)
    inspectEntries report (entry : remaining) = do
        let next =
                report
                    { cacheResidentEntries = cacheResidentEntries report + 1
                    , cacheExecutorOperations = cacheExecutorOperations report + 1
                    , cacheExecutorReserved = cacheExecutorReserved report + 2
                    }
        checkInfrastructure CircuitCacheEntryLimitExceeded (cacheEntryLimit limits) (cacheResidentEntries next)
        checkExecutor next
        let item@(_, cells, slots) = entryMetadata entry
            occupied =
                next
                    { cacheResidentCells = cacheResidentCells next + cells
                    , cacheResidentTraceSlots = cacheResidentTraceSlots next + slots
                    }
        checkStorage occupied
        (rest, final) <- inspectEntries occupied remaining
        Right (item : rest, final)
    boundedLookup key keyCost report = search 0
      where
        search comparisons [] = Right (comparisons, Nothing)
        search comparisons ((candidate, cells, slots) : rest) = do
            checkInfrastructure
                CircuitCacheExecutorLimitExceeded
                (cacheExecutorLimit limits)
                (cacheExecutorReserved report + (comparisons + 1) * keyCost)
            if sameShapeKey key candidate
                then Right (comparisons + 1, Just (cells, slots))
                else search (comparisons + 1) rest
    go _ report [] = do
        -- Complete the returned cache spine before publishing its owner.
        let final =
                report
                    { cacheExecutorReserved = cacheExecutorReserved report + cacheResidentEntries report
                    , cacheExecutorOperations = cacheExecutorOperations report + cacheResidentEntries report
                    }
        checkExecutor final
        Right final
    go metadata report (PlannedRequest _ key _ plan : remaining) = do
        let keyCost = 2 * (1 + planSourceSize plan + planTargetSize plan)
        (comparisons, found) <-
            if mode == UncachedCircuitTables
                then Right (0, Nothing)
                else boundedLookup key keyCost report metadata
        let isHit = case found of Just _ -> True; Nothing -> False
            retaining = mode == RetainCircuitTables && not isHit
            insertionTraversal = if retaining then 2 * cacheResidentEntries report else 0
            reservation = case found of
                Just (_, slots) -> traceExecutorReservation semantic slots
                Nothing -> planExecutorReservation semantic retaining plan + if retaining then 1 else 0
            next =
                report
                    { cacheExecutorOperations = cacheExecutorOperations report + 2 * comparisons + insertionTraversal
                    , cacheExecutorReserved = cacheExecutorReserved report + keyCost * comparisons + insertionTraversal + reservation
                    , cacheLookupComparisons = cacheLookupComparisons report + 2 * comparisons
                    , cacheResidentEntries = cacheResidentEntries report + if retaining then 1 else 0
                    , cacheResidentCells = cacheResidentCells report + if retaining then planSourceSize plan * planTargetSize plan else 0
                    , cacheResidentTraceSlots = cacheResidentTraceSlots report + if retaining then planEvents plan else 0
                    }
        checkStorage next
        checkExecutor next
        let nextMetadata = if retaining then metadata ++ [(key, planSourceSize plan * planTargetSize plan, planEvents plan)] else metadata
        go nextMetadata next remaining

reserveSource :: CircuitSemanticLimits -> CircuitCacheReport -> [PlannedRequest source target] -> Either CacheRunError CircuitCacheReport
reserveSource limits = go
  where
    go report [] = Right report
    go report (PlannedRequest occurrence _ nodes plan : rest) = do
        let nextNodes = cacheSourceNodes report + nodes
            nextCells = cacheSourceCells report + planCells plan
        when (nextNodes > circuitNodeLimit limits) $ Left (CacheSource occurrence (CircuitSourceNodeLimitExceeded (circuitNodeLimit limits) (circuitNodeLimit limits + 1)))
        when (nextCells > circuitCellLimit limits) $ Left (CacheSource occurrence (CircuitSourceCellLimitExceeded (circuitCellLimit limits) (circuitCellLimit limits + 1)))
        go (report{cacheSourceNodes = nextNodes, cacheSourceCells = nextCells}) rest

lookupEntry :: CacheKey source target -> [CacheEntry source target] -> Either CircuitSourceError (Maybe (CacheEntry source target))
lookupEntry _ [] = Right Nothing
lookupEntry requested (entry@(CacheEntry key _ _ _ _) : rest)
    | sameKey requested key = Right (Just entry)
    | sameShapeKey requested key = Left CircuitSourceInvariantFailure
    | otherwise = lookupEntry requested rest

runCircuitCache :: CircuitCacheMode -> CircuitSemanticLimits -> CircuitCacheLimits -> ExactTableInterpreterIdentity -> [TableRequest source target] -> TableCache source target -> Either CacheRunError ([StochasticMatrix NonNegativeRational source target], TableCache source target, CircuitCacheReport)
runCircuitCache mode semantic infrastructure interpreter requests cache = do
    when (circuitNumeratorBitLimit semantic == 0) (Left (CacheInvalidLimits CircuitInvalidNumeratorBitLimit))
    when (circuitDenominatorBitLimit semantic == 0) (Left (CacheInvalidLimits CircuitInvalidDenominatorBitLimit))
    (planned, nodes) <- planRequests semantic infrastructure interpreter requests
    reserved <- reserveRequests mode semantic infrastructure nodes cache planned
    admitted <- reserveSource semantic reserved planned
    let TableCache entries = cache
    (matrices, finalEntries, report) <- executeRequests admitted entries planned
    if cacheExecutorOperations report > cacheExecutorReserved report
        then Left (CacheInfrastructure (CircuitCacheExecutorLimitExceeded (cacheExecutorReserved report) (cacheExecutorOperations report)))
        else forceEntries finalEntries `seq` Right (matrices, TableCache finalEntries, report)
  where
    executeRequests report entries [] = Right ([], entries, report)
    executeRequests report entries (PlannedRequest occurrence key _ plan : remaining) = do
        found <- mapSource occurrence $ if mode == UncachedCircuitTables then Right Nothing else lookupEntry key entries
        (matrix, nextEntries, nextReport) <- case found of
            Just (CacheEntry _ table events _ _) -> do
                (_, replayed) <-
                    mapSource occurrence $
                        runWork
                            (replayReverse events)
                            (WorkState semantic report False [])
                Right (table, entries, (workReport replayed){cacheHits = cacheHits (workReport replayed) + 1})
            Nothing -> do
                let retaining = mode == RetainCircuitTables
                (table, executed) <- mapSource occurrence $ runWork (executePlan plan) (WorkState semantic report retaining [])
                let events = workEvents executed
                    cells = planSourceSize plan * planTargetSize plan
                    updated =
                        (workReport executed)
                            { cacheConstructions = cacheConstructions (workReport executed) + 1
                            , cacheExecutorOperations = cacheExecutorOperations (workReport executed) + if retaining then 1 else 0
                            }
                    entry = CacheEntry key table events cells (planEvents plan)
                    inserted = if retaining then entries ++ [entry] else entries
                forceEvents events `seq` forceMatrix table `seq` Right (table, inserted, updated)
        (others, finalEntries, finalReport) <- executeRequests nextReport nextEntries remaining
        let arrow = UnsafeStochasticMatrix matrix
        arrow `seq` Right (arrow : others, finalEntries, finalReport)
    mapSource occurrence = either (Left . CacheSource occurrence) Right
    replayReverse [] = pure ()
    replayReverse (event : remaining) = do
        executorTick 1
        replayReverse remaining
        emit event

forceEvents :: [SourceEvent] -> ()
forceEvents = foldl' (\() event -> event `seq` ()) ()

forceEntries :: [CacheEntry source target] -> ()
forceEntries = foldl' (\() entry -> entry `seq` ()) ()
