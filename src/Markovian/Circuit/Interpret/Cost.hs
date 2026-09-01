{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Bounded static cost accounting for finite circuit syntax.

Primitive and quoted-table charges are supplied by the caller. Structural
counts are fixed by the circuit elaboration. The reported maximum live layout
cardinality is only the largest represented finite-object cardinality seen by
the fold. It is not heap liveness, interpreter frontier width, asymptotic
complexity, or measured runtime.
-}
module Markovian.Circuit.Interpret.Cost (
    PrimitiveCharge,
    primitiveCharge,
    primitiveChargeOwner,
    primitiveChargeWork,
    CircuitCostInterpreter (..),
    CircuitCostLimits (..),
    CircuitCostLimitDimension (..),
    CircuitCostError (..),
    OwnerCost (..),
    CircuitStructuralCounts (..),
    CircuitCostReport (..),
    interpretCircuitCost,
    renderCircuitCostReport,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Markovian.Algebra.NonNegativeRational (NonNegativeRational)
import Markovian.Category.Finite.Set (FiniteSet, finiteSetCardinality)
import Markovian.Category.Matrix.Deterministic (
    DeterministicMatrix,
    deterministicSource,
    deterministicTarget,
 )
import Markovian.Circuit
import Numeric.Natural (Natural)

-- | One caller-owned charge for a primitive or quoted deterministic table.
data PrimitiveCharge owner = PrimitiveCharge !owner !Natural
    deriving (Eq, Show)

-- | Construct an owner-labelled nonnegative work charge.
primitiveCharge :: owner -> Natural -> PrimitiveCharge owner
primitiveCharge = PrimitiveCharge

-- | Read the charge owner.
primitiveChargeOwner :: PrimitiveCharge owner -> owner
primitiveChargeOwner (PrimitiveCharge owner _) = owner

-- | Read the caller-declared work units.
primitiveChargeWork :: PrimitiveCharge owner -> Natural
primitiveChargeWork (PrimitiveCharge _ work) = work

-- | Caller-owned primitive and table accounting callbacks.
data CircuitCostInterpreter primitive owner primitiveError = CircuitCostInterpreter
    { costDeterministicPrimitive ::
        forall source target.
        FiniteSet source ->
        FiniteSet target ->
        primitive 'Deterministic source target ->
        Either primitiveError (PrimitiveCharge owner)
    , costStochasticPrimitive ::
        forall source target.
        FiniteSet source ->
        FiniteSet target ->
        primitive 'Stochastic source target ->
        Either primitiveError (PrimitiveCharge owner)
    , costDeterministicTable ::
        forall source target.
        DeterministicMatrix NonNegativeRational source target ->
        Either primitiveError (PrimitiveCharge owner)
    }

-- | Framework-owned limits for one complete report.
data CircuitCostLimits = CircuitCostLimits
    { maximumSyntaxNodes :: !Natural
    , maximumDeclaredWork :: !Natural
    , maximumLayoutCardinality :: !Natural
    , maximumMatrixCells :: !Natural
    , maximumOwnerEntries :: !Natural
    }
    deriving (Eq, Show)

-- | The report dimension whose first exceeded value was rejected.
data CircuitCostLimitDimension
    = CostSyntaxNodes
    | CostDeclaredWork
    | CostLayoutCardinality
    | CostMatrixCells
    | CostOwnerEntries
    deriving (Eq, Show)

-- | Bounded cost-analysis failure.
data CircuitCostError primitiveError
    = CircuitCostPrimitiveError !primitiveError
    | CircuitCostLimitExceeded
        { circuitCostLimitDimension :: !CircuitCostLimitDimension
        , circuitCostLimit :: !Natural
        , circuitCostFirstExceededValue :: !Natural
        }
    deriving (Eq, Show)

-- | Stable first-occurrence owner total.
data OwnerCost owner = OwnerCost
    { ownerCostOwner :: !owner
    , ownerCostWork :: !Natural
    }
    deriving (Eq, Show)

-- | Counts in the elaborated circuit fold.
data CircuitStructuralCounts = CircuitStructuralCounts
    { identityCount :: !Natural
    , compositionCount :: !Natural
    , tensorCount :: !Natural
    , symmetryCount :: !Natural
    , associatorCount :: !Natural
    , unassociatorCount :: !Natural
    , leftUnitorCount :: !Natural
    , leftUnitorInverseCount :: !Natural
    , rightUnitorCount :: !Natural
    , rightUnitorInverseCount :: !Natural
    , copyCount :: !Natural
    , discardCount :: !Natural
    , convexChoiceCount :: !Natural
    , weakeningCount :: !Natural
    }
    deriving (Eq, Show)

-- | Deterministic static report for one elaborated fold.
data CircuitCostReport owner = CircuitCostReport
    { syntaxNodeCount :: !Natural
    , deterministicPrimitiveCount :: !Natural
    , stochasticPrimitiveCount :: !Natural
    , deterministicTableCount :: !Natural
    , declaredPrimitiveWork :: !Natural
    , ownerCosts :: ![OwnerCost owner]
    , structuralCounts :: !CircuitStructuralCounts
    , totalRepresentedCopyRows :: !Natural
    , maximumLiveLayoutCardinality :: !Natural
    , maximumRepresentedMatrixCells :: !Natural
    }
    deriving (Eq, Show)

data CostArrow owner purity source target
    = CostArrow
        !(CircuitCostReport owner)
        !Natural
        !Natural

{- | Interpret all represented branches with deterministic left-to-right order.
Primitive callback failure is retained as the original error payload.
-}
interpretCircuitCost ::
    forall owner primitive primitiveError purity source target.
    (Eq owner) =>
    CircuitCostLimits ->
    CircuitCostInterpreter primitive owner primitiveError ->
    Circuit primitive purity source target ->
    Either (CircuitCostError primitiveError) (CircuitCostReport owner)
interpretCircuitCost limits interpreter circuit =
    case foldCircuitWithNodeLimit (maximumSyntaxNodes limits) algebra circuit of
        Left (CircuitFoldNodeLimitExceeded limit actual) ->
            Left (CircuitCostLimitExceeded CostSyntaxNodes limit actual)
        Left (CircuitFoldAlgebraError problem) -> Left problem
        Right (nodes, CostArrow report _ _) -> Right report{syntaxNodeCount = nodes}
  where
    algebra :: CircuitAlgebra primitive (CostArrow owner) (CircuitCostError primitiveError)
    algebra =
        CircuitAlgebra
            { algebraPrimitive = primitiveArrow
            , algebraDeterministicTable = tableArrow
            , algebraIdentity = \object -> structuralArrow (cardinality object) (cardinality object) identityIncrement
            , algebraCompose = composeArrow
            , algebraTensor = tensorArrow
            , algebraSymmetry = \left right -> structuralArrow (productCardinality left right) (productCardinality right left) symmetryIncrement
            , algebraAssociate = \first second third -> structuralArrow (tripleCardinality first second third) (tripleCardinality first second third) associatorIncrement
            , algebraUnassociate = \first second third -> structuralArrow (tripleCardinality first second third) (tripleCardinality first second third) unassociatorIncrement
            , algebraLeftUnitor = \object -> structuralArrow (cardinality object) (cardinality object) leftUnitorIncrement
            , algebraLeftUnitorInverse = \object -> structuralArrow (cardinality object) (cardinality object) leftUnitorInverseIncrement
            , algebraRightUnitor = \object -> structuralArrow (cardinality object) (cardinality object) rightUnitorIncrement
            , algebraRightUnitorInverse = \object -> structuralArrow (cardinality object) (cardinality object) rightUnitorInverseIncrement
            , algebraCopy = copyArrow
            , algebraDiscard = \object -> structuralArrow (cardinality object) 1 discardIncrement
            , algebraConvexChoice = convexArrow
            , algebraWeaken = weakenArrow
            }

    primitiveArrow ::
        forall innerPurity innerSource innerTarget.
        SPurity innerPurity ->
        FiniteSet innerSource ->
        FiniteSet innerTarget ->
        primitive innerPurity innerSource innerTarget ->
        Either
            (CircuitCostError primitiveError)
            (CostArrow owner innerPurity innerSource innerTarget)
    primitiveArrow purity source target primitive = do
        charge <-
            mapPrimitiveError $
                case purity of
                    SDeterministic -> costDeterministicPrimitive interpreter source target primitive
                    SStochastic -> costStochasticPrimitive interpreter source target primitive
        let primitiveCounts =
                case purity of
                    SDeterministic -> emptyReport{deterministicPrimitiveCount = 1}
                    SStochastic -> emptyReport{stochasticPrimitiveCount = 1}
        chargedArrow (cardinality source) (cardinality target) primitiveCounts charge

    tableArrow ::
        forall innerSource innerTarget.
        DeterministicMatrix NonNegativeRational innerSource innerTarget ->
        Either
            (CircuitCostError primitiveError)
            (CostArrow owner 'Deterministic innerSource innerTarget)
    tableArrow table = do
        charge <- mapPrimitiveError (costDeterministicTable interpreter table)
        chargedArrow
            (cardinality (deterministicSource table))
            (cardinality (deterministicTarget table))
            emptyReport{deterministicTableCount = 1}
            charge

    chargedArrow ::
        forall innerPurity innerSource innerTarget.
        Natural ->
        Natural ->
        CircuitCostReport owner ->
        PrimitiveCharge owner ->
        Either
            (CircuitCostError primitiveError)
            (CostArrow owner innerPurity innerSource innerTarget)
    chargedArrow source target report charge =
        checkedArrow source target $
            report
                { declaredPrimitiveWork = primitiveChargeWork charge
                , ownerCosts = [OwnerCost (primitiveChargeOwner charge) (primitiveChargeWork charge)]
                }

    structuralArrow ::
        forall innerSource innerTarget.
        Natural ->
        Natural ->
        (CircuitStructuralCounts -> CircuitStructuralCounts) ->
        Either
            (CircuitCostError primitiveError)
            (CostArrow owner 'Deterministic innerSource innerTarget)
    structuralArrow source target increment =
        checkedArrow source target emptyReport{structuralCounts = increment emptyCounts}

    copyArrow ::
        forall value.
        FiniteSet value ->
        Either
            (CircuitCostError primitiveError)
            (CostArrow owner 'Deterministic value (value, value))
    copyArrow object =
        checkedArrow
            (cardinality object)
            (squareCardinality object)
            emptyReport
                { structuralCounts = copyIncrement emptyCounts
                , totalRepresentedCopyRows = cardinality object
                }

    composeArrow ::
        forall leftPurity rightPurity innerSource middle innerTarget.
        CostArrow owner leftPurity innerSource middle ->
        CostArrow owner rightPurity middle innerTarget ->
        Either
            (CircuitCostError primitiveError)
            (CostArrow owner (JoinPurity leftPurity rightPurity) innerSource innerTarget)
    composeArrow (CostArrow left leftSource _) (CostArrow right _ rightTarget) = do
        combined <- combineReports left right
        checkedArrow leftSource rightTarget combined{structuralCounts = compositionIncrement (structuralCounts combined)}

    tensorArrow ::
        forall leftPurity rightPurity leftSource leftTarget rightSource rightTarget.
        CostArrow owner leftPurity leftSource leftTarget ->
        CostArrow owner rightPurity rightSource rightTarget ->
        Either
            (CircuitCostError primitiveError)
            (CostArrow owner (JoinPurity leftPurity rightPurity) (leftSource, rightSource) (leftTarget, rightTarget))
    tensorArrow (CostArrow left leftSource leftTarget) (CostArrow right rightSource rightTarget) = do
        combined <- combineReports left right
        checkedArrow
            (leftSource * rightSource)
            (leftTarget * rightTarget)
            combined{structuralCounts = tensorIncrement (structuralCounts combined)}

    convexArrow ::
        forall innerSource innerTarget.
        NonEmpty (NonNegativeRational, CostArrow owner 'Stochastic innerSource innerTarget) ->
        Either
            (CircuitCostError primitiveError)
            (CostArrow owner 'Stochastic innerSource innerTarget)
    convexArrow ((_, first) :| remaining) = do
        combined <- foldReports (arrowReport first) (fmap (arrowReport . snd) remaining)
        let CostArrow _ source target = first
        checkedArrow source target combined{structuralCounts = convexIncrement (structuralCounts combined)}

    weakenArrow ::
        forall innerSource innerTarget.
        CostArrow owner 'Deterministic innerSource innerTarget ->
        Either
            (CircuitCostError primitiveError)
            (CostArrow owner 'Stochastic innerSource innerTarget)
    weakenArrow (CostArrow report source target) =
        checkedArrow source target report{structuralCounts = weakeningIncrement (structuralCounts report)}

    checkedArrow ::
        forall innerPurity innerSource innerTarget.
        Natural ->
        Natural ->
        CircuitCostReport owner ->
        Either
            (CircuitCostError primitiveError)
            (CostArrow owner innerPurity innerSource innerTarget)
    checkedArrow source target report = do
        let layout = max source target
            cells = source * target
            widened =
                report
                    { maximumLiveLayoutCardinality = max layout (maximumLiveLayoutCardinality report)
                    , maximumRepresentedMatrixCells = max cells (maximumRepresentedMatrixCells report)
                    }
        checkReport limits widened
        Right (CostArrow widened source target)

    combineReports left right = do
        let owners = mergeOwnerCosts (ownerCosts left) (ownerCosts right)
            combined =
                CircuitCostReport
                    { syntaxNodeCount = 0
                    , deterministicPrimitiveCount = deterministicPrimitiveCount left + deterministicPrimitiveCount right
                    , stochasticPrimitiveCount = stochasticPrimitiveCount left + stochasticPrimitiveCount right
                    , deterministicTableCount = deterministicTableCount left + deterministicTableCount right
                    , declaredPrimitiveWork = declaredPrimitiveWork left + declaredPrimitiveWork right
                    , ownerCosts = owners
                    , structuralCounts = addCounts (structuralCounts left) (structuralCounts right)
                    , totalRepresentedCopyRows = totalRepresentedCopyRows left + totalRepresentedCopyRows right
                    , maximumLiveLayoutCardinality = max (maximumLiveLayoutCardinality left) (maximumLiveLayoutCardinality right)
                    , maximumRepresentedMatrixCells = max (maximumRepresentedMatrixCells left) (maximumRepresentedMatrixCells right)
                    }
        checkReport limits combined
        Right combined

    foldReports first [] = Right first
    foldReports first (next : remaining) = do
        combined <- combineReports first next
        foldReports combined remaining

    mapPrimitiveError = either (Left . CircuitCostPrimitiveError) Right

-- | Stable line-oriented rendering suitable for deterministic evidence files.
renderCircuitCostReport :: (Show owner) => CircuitCostReport owner -> String
renderCircuitCostReport report =
    unlines
        [ "syntax-nodes: " ++ show (syntaxNodeCount report)
        , "deterministic-primitives: " ++ show (deterministicPrimitiveCount report)
        , "stochastic-primitives: " ++ show (stochasticPrimitiveCount report)
        , "deterministic-tables: " ++ show (deterministicTableCount report)
        , "declared-work: " ++ show (declaredPrimitiveWork report)
        , "owners: " ++ show (ownerCosts report)
        , "structure: " ++ show (structuralCounts report)
        , "total-represented-copy-rows: " ++ show (totalRepresentedCopyRows report)
        , "maximum-live-layout-cardinality: " ++ show (maximumLiveLayoutCardinality report)
        , "maximum-represented-matrix-cells: " ++ show (maximumRepresentedMatrixCells report)
        ]

checkReport :: CircuitCostLimits -> CircuitCostReport owner -> Either (CircuitCostError primitiveError) ()
checkReport limits report
    | declaredPrimitiveWork report > maximumDeclaredWork limits = exceeded CostDeclaredWork (maximumDeclaredWork limits) (declaredPrimitiveWork report)
    | maximumLiveLayoutCardinality report > maximumLayoutCardinality limits = exceeded CostLayoutCardinality (maximumLayoutCardinality limits) (maximumLiveLayoutCardinality report)
    | maximumRepresentedMatrixCells report > maximumMatrixCells limits = exceeded CostMatrixCells (maximumMatrixCells limits) (maximumRepresentedMatrixCells report)
    | ownerCount > maximumOwnerEntries limits = exceeded CostOwnerEntries (maximumOwnerEntries limits) ownerCount
    | otherwise = Right ()
  where
    ownerCount = fromIntegral (length (ownerCosts report))
    exceeded dimension limit actual = Left (CircuitCostLimitExceeded dimension limit actual)

mergeOwnerCosts :: (Eq owner) => [OwnerCost owner] -> [OwnerCost owner] -> [OwnerCost owner]
mergeOwnerCosts = foldl addOwner
  where
    addOwner accumulated (OwnerCost owner work) = update accumulated
      where
        update [] = [OwnerCost owner work]
        update (entry@(OwnerCost existing existingWork) : remaining)
            | owner == existing = OwnerCost existing (existingWork + work) : remaining
            | otherwise = entry : update remaining

addCounts :: CircuitStructuralCounts -> CircuitStructuralCounts -> CircuitStructuralCounts
addCounts left right =
    CircuitStructuralCounts
        { identityCount = identityCount left + identityCount right
        , compositionCount = compositionCount left + compositionCount right
        , tensorCount = tensorCount left + tensorCount right
        , symmetryCount = symmetryCount left + symmetryCount right
        , associatorCount = associatorCount left + associatorCount right
        , unassociatorCount = unassociatorCount left + unassociatorCount right
        , leftUnitorCount = leftUnitorCount left + leftUnitorCount right
        , leftUnitorInverseCount = leftUnitorInverseCount left + leftUnitorInverseCount right
        , rightUnitorCount = rightUnitorCount left + rightUnitorCount right
        , rightUnitorInverseCount = rightUnitorInverseCount left + rightUnitorInverseCount right
        , copyCount = copyCount left + copyCount right
        , discardCount = discardCount left + discardCount right
        , convexChoiceCount = convexChoiceCount left + convexChoiceCount right
        , weakeningCount = weakeningCount left + weakeningCount right
        }

emptyReport :: CircuitCostReport owner
emptyReport =
    CircuitCostReport
        { syntaxNodeCount = 0
        , deterministicPrimitiveCount = 0
        , stochasticPrimitiveCount = 0
        , deterministicTableCount = 0
        , declaredPrimitiveWork = 0
        , ownerCosts = []
        , structuralCounts = emptyCounts
        , totalRepresentedCopyRows = 0
        , maximumLiveLayoutCardinality = 0
        , maximumRepresentedMatrixCells = 0
        }

emptyCounts :: CircuitStructuralCounts
emptyCounts = CircuitStructuralCounts 0 0 0 0 0 0 0 0 0 0 0 0 0 0

identityIncrement, compositionIncrement, tensorIncrement, symmetryIncrement :: CircuitStructuralCounts -> CircuitStructuralCounts
associatorIncrement, unassociatorIncrement, leftUnitorIncrement, leftUnitorInverseIncrement :: CircuitStructuralCounts -> CircuitStructuralCounts
rightUnitorIncrement, rightUnitorInverseIncrement, copyIncrement, discardIncrement :: CircuitStructuralCounts -> CircuitStructuralCounts
convexIncrement, weakeningIncrement :: CircuitStructuralCounts -> CircuitStructuralCounts
identityIncrement counts = counts{identityCount = identityCount counts + 1}
compositionIncrement counts = counts{compositionCount = compositionCount counts + 1}
tensorIncrement counts = counts{tensorCount = tensorCount counts + 1}
symmetryIncrement counts = counts{symmetryCount = symmetryCount counts + 1}
associatorIncrement counts = counts{associatorCount = associatorCount counts + 1}
unassociatorIncrement counts = counts{unassociatorCount = unassociatorCount counts + 1}
leftUnitorIncrement counts = counts{leftUnitorCount = leftUnitorCount counts + 1}
leftUnitorInverseIncrement counts = counts{leftUnitorInverseCount = leftUnitorInverseCount counts + 1}
rightUnitorIncrement counts = counts{rightUnitorCount = rightUnitorCount counts + 1}
rightUnitorInverseIncrement counts = counts{rightUnitorInverseCount = rightUnitorInverseCount counts + 1}
copyIncrement counts = counts{copyCount = copyCount counts + 1}
discardIncrement counts = counts{discardCount = discardCount counts + 1}
convexIncrement counts = counts{convexChoiceCount = convexChoiceCount counts + 1}
weakeningIncrement counts = counts{weakeningCount = weakeningCount counts + 1}

cardinality :: FiniteSet value -> Natural
cardinality = fromIntegral . finiteSetCardinality

productCardinality :: FiniteSet left -> FiniteSet right -> Natural
productCardinality left right = cardinality left * cardinality right

squareCardinality :: FiniteSet value -> Natural
squareCardinality object = cardinality object * cardinality object

tripleCardinality :: FiniteSet first -> FiniteSet second -> FiniteSet third -> Natural
tripleCardinality first second third = cardinality first * cardinality second * cardinality third

arrowReport :: CostArrow owner purity source target -> CircuitCostReport owner
arrowReport (CostArrow report _ _) = report
