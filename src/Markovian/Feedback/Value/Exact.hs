{-# LANGUAGE CPP #-}
{-# LANGUAGE RoleAnnotations #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

{- | Exact strict-discount affine value coefficients for finite feedback.

For a normalized event channel @X + U -> Event U Y@ and @0 <= gamma < 1@,
this module computes the affine value map

@V(x) = A(x) + sum_y K(x,y) v(y)@.

An event pays its reward immediately. An exit continuation is therefore
multiplied by @gamma@. The first-exit time may be infinite; on that event the
exit-continuation coefficient is zero, while the bounded discounted reward
sum remains defined. Reward and route stay in one event until their exact
joint expectation is aggregated.

This is not an evaluator, a normalized output channel, a trace, an
undiscounted solver, or an adapter for cyclic open systems.
-}
module Markovian.Feedback.Value.Exact (
    FeedbackLimits,
    feedbackLimits,
    FeedbackLimitDimension (..),
    FeedbackLimitError (..),
    FeedbackAccounting (..),
    LoopLayout,
    loopLayout,
    FeedbackEvent (..),
    AffineFeedbackError (..),
    AffineFeedbackReport (..),
    AffineFeedbackCoefficients,
    affineConstantCoefficient,
    affineContinuationCoefficient,
    CheckedAffineFeedback,
    closeAffineFeedback,
    affineFeedbackExternalCoefficients,
    affineFeedbackInternalCoefficients,
    affineFeedbackReport,
) where

import Data.List (findIndex)
import Markovian.Algebra.NonNegativeRational (NonNegativeRational, getNonNegativeRational)
import Markovian.Category.Finite.Set (FiniteSet, finiteSetCardinality, finiteSetValues, sameFiniteLayout)
import Markovian.Category.Matrix (matrixRows)
import Markovian.Category.Matrix.Stochastic
import Markovian.Feedback.Internal
import Markovian.Feedback.Timed.Exact (FeedbackEvent (..))
import Markovian.Objective.Exact (ExactContractionDiscount, exactContractionDiscountValue)
import Markovian.Reward.Exact (ExactReward, exactReward, exactRewardValue)
import Numeric.Natural (Natural)

-- | Failure from strict-discount affine feedback construction.
data AffineFeedbackError loop output
    = AffineFeedbackLimitError !FeedbackLimitError
    | AffineFeedbackSourceLayoutMismatch
    | AffineFeedbackContinueOutsideLoop !loop
    | AffineFeedbackExitOutsideOutput !output
    | AffineFeedbackSingularInternalSystem
    | AffineFeedbackExternalConstantEquationFailure !Int
    | AffineFeedbackInternalConstantEquationFailure !loop
    | AffineFeedbackExternalContinuationEquationFailure !Int !output
    | AffineFeedbackInternalContinuationEquationFailure !loop !output
    deriving (Eq, Show)

-- | Deterministic dimensions, validation evidence, and cumulative accounting.
data AffineFeedbackReport owner = AffineFeedbackReport
    { affineFeedbackOwner :: !owner
    , affineFeedbackDiscount :: !Rational
    , affineFeedbackSourceCount :: !Natural
    , affineFeedbackLoopCount :: !Natural
    , affineFeedbackOutputCount :: !Natural
    , affineFeedbackEventCount :: !Natural
    , affineFeedbackMatrixCellCount :: !Natural
    , affineFeedbackGraphWorkCharged :: !Natural
    , affineFeedbackValidatedEquationCount :: !Natural
    , affineFeedbackArithmeticWorkCharged :: !Natural
    , affineFeedbackMaximumResultBits :: !Natural
    , affineFeedbackAccounting :: !FeedbackAccounting
    }
    deriving (Eq, Show)

-- | Opaque coefficients @A@ and @K@ on one nominal source and output layout.
type role AffineFeedbackCoefficients nominal nominal

data AffineFeedbackCoefficients source output
    = UnsafeAffineFeedbackCoefficients
        !(FiniteSet source)
        !(FiniteSet output)
        ![Rational]
        ![[Rational]]

-- | Observe @A(source)@ when the source is in the retained nominal layout.
affineConstantCoefficient :: (Eq source) => AffineFeedbackCoefficients source output -> source -> Maybe ExactReward
affineConstantCoefficient (UnsafeAffineFeedbackCoefficients sources _ constants _) source =
    exactReward <$> lookupLayout source (finiteSetValues sources) constants

-- | Observe @K(source,output)@ when both labels are in the retained layouts.
affineContinuationCoefficient ::
    (Eq source, Eq output) =>
    AffineFeedbackCoefficients source output ->
    source ->
    output ->
    Maybe Rational
affineContinuationCoefficient (UnsafeAffineFeedbackCoefficients sources outputs _ continuations) source output = do
    row <- lookupLayout source (finiteSetValues sources) continuations
    lookupLayout output (finiteSetValues outputs) row

-- | Opaque evidence that construction and all four literal equations passed.
type role CheckedAffineFeedback nominal nominal nominal nominal

data CheckedAffineFeedback owner input loop output
    = UnsafeCheckedAffineFeedback
        !(AffineFeedbackCoefficients input output)
        !(AffineFeedbackCoefficients loop output)
        !(AffineFeedbackReport owner)

{- | Construct exact affine feedback coefficients.

Failure precedence is dimension limits, the shared event-count limit, combined
cell and graph preflight, malformed event targets, source layout, then the
single atomic rational ledger. For @S=|X|+|U|@, @E=|events|@, and
@T=|U|+|Y|@, graph work is bounded by @E*T + S + E + 4*S*E + S*E*T@.
This covers event-target validation, source-layout comparison, event rewards,
row-major channel extraction and observation, reward aggregation, and every
continuation/exit event-target scan. The event-count limit uses
'maximumFeedbackTraceOutcomes' from the shared feedback limit record; no trace
or outcome law is constructed.
-}
closeAffineFeedback ::
    (Eq input, Eq output) =>
    FeedbackLimits ->
    ExactContractionDiscount ->
    FiniteSet input ->
    LoopLayout owner loop ->
    FiniteSet output ->
    StochasticMatrix NonNegativeRational (Either input loop) (FeedbackEvent loop output) ->
    Either (AffineFeedbackError loop output) (CheckedAffineFeedback owner input loop output)
closeAffineFeedback limits discount inputs (UnsafeLoopLayout owner loops) outputs channel = do
    let sourceCount = cardinality inputs
        loopCount = cardinality loops
        outputCount = cardinality outputs
        events = finiteSetValues (stochasticTarget channel)
        eventCount = fromIntegral (length events)
    mapLimit $ checkLimit FeedbackSourceCount (maximumFeedbackSources limits) sourceCount
    mapLimit $ checkLimit FeedbackLoopCount (maximumFeedbackLoops limits) loopCount
    mapLimit $ checkLimit FeedbackOutputCount (maximumFeedbackOutputs limits) outputCount
    mapLimit $ checkLimit FeedbackTraceOutcomeCount (maximumFeedbackTraceOutcomes limits) eventCount
    combinedSources <- mapLimit $ checkedSum FeedbackMatrixCellCount [sourceCount, loopCount]
    cells <- mapLimit $ checkedProduct FeedbackMatrixCellCount [combinedSources, eventCount]
    mapLimit $ checkLimit FeedbackMatrixCellCount (maximumFeedbackMatrixCells limits) cells
    targetWidth <- mapLimit $ checkedSum FeedbackGraphWork [loopCount, outputCount]
    validationVisits <- mapLimit $ checkedProduct FeedbackGraphWork [eventCount, targetWidth]
    inputCellVisits <- mapLimit $ checkedProduct FeedbackGraphWork [4, cells]
    aggregateTargetVisits <- mapLimit $ checkedProduct FeedbackGraphWork [cells, targetWidth]
    graphWork <- mapLimit $ checkedSum FeedbackGraphWork [validationVisits, combinedSources, eventCount, inputCellVisits, aggregateTargetVisits]
    mapLimit $ checkLimit FeedbackGraphWork (maximumFeedbackGraphWork limits) graphWork
    mapM_ validateEvent events
    let expectedSource = sumFiniteSet inputs loops
    if sameFiniteLayout expectedSource (stochasticSource channel)
        then Right ()
        else Left AffineFeedbackSourceLayoutMismatch

    let gamma = exactContractionDiscountValue discount
        raw = forgetStochastic channel
        rationalRows = map (map getNonNegativeRational) (matrixRows raw)
        (inputRows, internalRows) = splitAt (finiteSetCardinality inputs) rationalRows
        loopValues = finiteSetValues loops
        outputValues = finiteSetValues outputs
    ((externalA, internalA, externalK, internalK), accounting) <-
        runFeedbackMeter limits AffineFeedbackLimitError $ do
            _ <- recordFeedbackRational FeedbackInputPhase "affine feedback discount input" gamma
            mapM_ (recordEventReward "affine feedback event reward") events
            mapM_
                (recordFeedbackRational FeedbackInputPhase "affine feedback channel input")
                (concat rationalRows)
            (mX, b, eX) <- aggregateRows events inputRows loopValues outputValues
            (mU, d, eU) <- aggregateRows events internalRows loopValues outputValues
            (internalA, internalK) <- solveInternal gamma d mU eU
            (externalA, externalK) <- deriveExternal gamma mX b eX internalA internalK
            validateExternalConstant gamma mX b internalA externalA
            validateInternalConstant gamma loopValues mU d internalA
            validateExternalContinuation gamma outputValues eX b internalK externalK
            validateInternalContinuation gamma loopValues outputValues eU d internalK
            mapM_ (recordFeedbackRational FeedbackRetainedResultPhase "affine feedback retained external A") externalA
            mapM_ (recordFeedbackRational FeedbackRetainedResultPhase "affine feedback retained internal A") internalA
            mapM_ (mapM_ (recordFeedbackRational FeedbackRetainedResultPhase "affine feedback retained external K")) externalK
            mapM_ (mapM_ (recordFeedbackRational FeedbackRetainedResultPhase "affine feedback retained internal K")) internalK
            pure (externalA, internalA, externalK, internalK)
    let external = UnsafeAffineFeedbackCoefficients inputs outputs externalA externalK
        internal = UnsafeAffineFeedbackCoefficients loops outputs internalA internalK
        report =
            AffineFeedbackReport
                { affineFeedbackOwner = owner
                , affineFeedbackDiscount = gamma
                , affineFeedbackSourceCount = sourceCount
                , affineFeedbackLoopCount = loopCount
                , affineFeedbackOutputCount = outputCount
                , affineFeedbackEventCount = eventCount
                , affineFeedbackMatrixCellCount = cells
                , affineFeedbackGraphWorkCharged = graphWork
                , affineFeedbackValidatedEquationCount = 4
                , affineFeedbackArithmeticWorkCharged = feedbackArithmeticWork accounting
                , affineFeedbackMaximumResultBits = feedbackMaximumRetainedResultBits accounting
                , affineFeedbackAccounting = accounting
                }
    Right (UnsafeCheckedAffineFeedback external internal report)
  where
    mapLimit = either (Left . AffineFeedbackLimitError) Right
    validateEvent (Continue _ loopValue)
        | loopValue `elem` finiteSetValues loops = Right ()
        | otherwise = Left (AffineFeedbackContinueOutsideLoop loopValue)
    validateEvent (Exit _ output)
        | output `elem` finiteSetValues outputs = Right ()
        | otherwise = Left (AffineFeedbackExitOutsideOutput output)

-- | Read the checked external coefficients @A_X@ and @K_X@.
affineFeedbackExternalCoefficients :: CheckedAffineFeedback owner input loop output -> AffineFeedbackCoefficients input output
affineFeedbackExternalCoefficients (UnsafeCheckedAffineFeedback external _ _) = external

-- | Read the checked internal coefficients @A_U@ and @K_U@.
affineFeedbackInternalCoefficients :: CheckedAffineFeedback owner input loop output -> AffineFeedbackCoefficients loop output
affineFeedbackInternalCoefficients (UnsafeCheckedAffineFeedback _ internal _) = internal

-- | Read deterministic cumulative construction evidence.
affineFeedbackReport :: CheckedAffineFeedback owner input loop output -> AffineFeedbackReport owner
affineFeedbackReport (UnsafeCheckedAffineFeedback _ _ report) = report

aggregateRows ::
    (Eq loop, Eq output) =>
    [FeedbackEvent loop output] ->
    [[Rational]] ->
    [loop] ->
    [output] ->
    FeedbackMeter (AffineFeedbackError loop output) ([Rational], [[Rational]], [[Rational]])
aggregateRows events rows loops outputs = do
    rewards <- traverse expectedReward rows
    continuation <- traverse (\row -> traverse (continuationMass row) loops) rows
    exits <- traverse (\row -> traverse (exitMass row) outputs) rows
    pure (rewards, continuation, exits)
  where
    expectedReward row = foldlM rewardTerm 0 (zip row events)
      where
        rewardTerm total (mass, event) = do
            weighted <- feedbackMultiply FeedbackOtherIntermediatePhase "affine reward product" mass (eventReward event)
            feedbackAdd FeedbackOtherIntermediatePhase "affine reward sum" total weighted
    continuationMass row target =
        foldlM
            (feedbackAdd FeedbackOtherIntermediatePhase "affine continuation aggregate")
            0
            [mass | (mass, Continue _ candidate) <- zip row events, candidate == target]
    exitMass row target =
        foldlM
            (feedbackAdd FeedbackOtherIntermediatePhase "affine exit aggregate")
            0
            [mass | (mass, Exit _ candidate) <- zip row events, candidate == target]

eventReward :: FeedbackEvent loop output -> Rational
eventReward (Continue reward _) = exactRewardValue reward
eventReward (Exit reward _) = exactRewardValue reward

recordEventReward :: String -> FeedbackEvent loop output -> FeedbackMeter error Rational
recordEventReward label = recordFeedbackRational FeedbackInputPhase label . eventReward

-- The solver handles A and every column of K in one augmented elimination.
-- Its only call site has already bounded dimensions, matrix cells, work, and
-- every input rational under the operation-wide meter.
solveInternal ::
    Rational ->
    [[Rational]] ->
    [Rational] ->
    [[Rational]] ->
    FeedbackMeter (AffineFeedbackError loop output) ([Rational], [[Rational]])
solveInternal _ [] [] [] = pure ([], [])
solveInternal gamma d rewards exits = do
    coefficient <- traverse coefficientRow [0 .. size - 1]
    continuationRhs <- traverse (traverse (feedbackMultiply FeedbackGaussianPhase "affine Gaussian exit RHS" gamma)) exits
    let rhs = zipWith (:) rewards continuationRhs
    mapM_ (mapM_ (recordFeedbackRational FeedbackGaussianPhase "affine Gaussian augmented input")) rhs
    solved <- gaussian (zipWith (++) coefficient rhs) 0
    pure (map head solved, map tail solved)
  where
    size = length d
    coefficientRow row = traverse (coefficientEntry row) [0 .. size - 1]
    coefficientEntry row column = do
        scaled <- feedbackMultiply FeedbackGaussianPhase "affine Gaussian scaled continuation" gamma (d !! row !! column)
        feedbackSubtract FeedbackGaussianPhase "affine Gaussian coefficient" (if row == column then 1 else 0) scaled
    gaussian rows column
        | column == size = pure (map (drop size) rows)
        | otherwise = do
            pivotOffset <- maybe (throwFeedback AffineFeedbackSingularInternalSystem) pure (findIndex ((/= 0) . (!! column)) (drop column rows))
            let pivotIndex = column + pivotOffset
                swapped = swapRows column pivotIndex rows
                pivot = swapped !! column !! column
            _ <- recordFeedbackRational FeedbackGaussianPhase "affine Gaussian pivot" pivot
            pivotRow <- traverse (\value -> feedbackDivide FeedbackGaussianPhase "affine Gaussian quotient" value pivot) (swapped !! column)
            eliminated <- traverse (eliminate column pivotRow) (zip [0 ..] swapped)
            gaussian eliminated (column + 1)
    eliminate pivotColumn pivotRow (rowIndex, row)
        | rowIndex == pivotColumn = pure pivotRow
        | otherwise = do
            let factor = row !! pivotColumn
            _ <- recordFeedbackRational FeedbackGaussianPhase "affine Gaussian elimination factor" factor
            traverse (eliminateEntry factor) (zip row pivotRow)
    eliminateEntry factor (value, pivotValue) = do
        productValue <- feedbackMultiply FeedbackGaussianPhase "affine Gaussian discarded product" factor pivotValue
        feedbackSubtract FeedbackGaussianPhase "affine Gaussian elimination difference" value productValue

deriveExternal ::
    Rational ->
    [Rational] ->
    [[Rational]] ->
    [[Rational]] ->
    [Rational] ->
    [[Rational]] ->
    FeedbackMeter error ([Rational], [[Rational]])
deriveExternal gamma rewards continuation exits internalA internalK = do
    externalA <- traverse deriveA (zip rewards continuation)
    externalK <- traverse deriveK (zip exits continuation)
    pure (externalA, externalK)
  where
    deriveA (reward, row) = do
        internal <- dot "affine external A" row internalA
        discounted <- feedbackMultiply FeedbackOtherIntermediatePhase "affine external A discount" gamma internal
        feedbackAdd FeedbackOtherIntermediatePhase "affine external A sum" reward discounted
    deriveK (exitRow, row) = traverse deriveColumn (zip exitRow (columnsFor (length exitRow) internalK))
      where
        deriveColumn (exitMass, column) = do
            direct <- feedbackMultiply FeedbackOtherIntermediatePhase "affine external K direct discount" gamma exitMass
            internal <- dot "affine external K" row column
            discounted <- feedbackMultiply FeedbackOtherIntermediatePhase "affine external K feedback discount" gamma internal
            feedbackAdd FeedbackOtherIntermediatePhase "affine external K sum" direct discounted

validateExternalConstant ::
    Rational -> [Rational] -> [[Rational]] -> [Rational] -> [Rational] -> FeedbackMeter (AffineFeedbackError loop output) ()
validateExternalConstant gamma rewards continuation internalA actual =
    mapM_ validate (zip4 [0 ..] rewards continuation actual)
  where
    validate (rowIndex, reward, row, result) = do
        productValue <- dot "affine external A equation" row internalA
        discounted <- feedbackMultiply FeedbackOtherIntermediatePhase "affine external A equation discount" gamma productValue
        rhs <- feedbackAdd FeedbackOtherIntermediatePhase "affine external A equation RHS" reward discounted
        if result == rhs then pure () else throwFeedback (AffineFeedbackExternalConstantEquationFailure rowIndex)

validateInternalConstant ::
    Rational -> [loop] -> [Rational] -> [[Rational]] -> [Rational] -> FeedbackMeter (AffineFeedbackError loop output) ()
validateInternalConstant gamma loops rewards continuation actual =
    mapM_ validate (zip4 loops rewards continuation actual)
  where
    validate (loopValue, reward, row, result) = do
        productValue <- dot "affine internal A equation" row actual
        discounted <- feedbackMultiply FeedbackOtherIntermediatePhase "affine internal A equation discount" gamma productValue
        rhs <- feedbackAdd FeedbackOtherIntermediatePhase "affine internal A equation RHS" reward discounted
        if result == rhs then pure () else throwFeedback (AffineFeedbackInternalConstantEquationFailure loopValue)

validateExternalContinuation ::
    Rational ->
    [output] ->
    [[Rational]] ->
    [[Rational]] ->
    [[Rational]] ->
    [[Rational]] ->
    FeedbackMeter (AffineFeedbackError loop output) ()
validateExternalContinuation gamma outputs exits continuation internalK actual =
    mapM_ validateRow (zip3 [0 ..] exits (zip continuation actual))
  where
    columns = columnsFor (length outputs) internalK
    validateRow (rowIndex, exitRow, (continuationRow, actualRow)) =
        mapM_ (validateEntry rowIndex continuationRow) (zip4 outputs exitRow columns actualRow)
    validateEntry rowIndex continuationRow (output, exitMass, column, result) = do
        direct <- feedbackMultiply FeedbackOtherIntermediatePhase "affine external K equation direct" gamma exitMass
        productValue <- dot "affine external K equation" continuationRow column
        discounted <- feedbackMultiply FeedbackOtherIntermediatePhase "affine external K equation discount" gamma productValue
        rhs <- feedbackAdd FeedbackOtherIntermediatePhase "affine external K equation RHS" direct discounted
        if result == rhs then pure () else throwFeedback (AffineFeedbackExternalContinuationEquationFailure rowIndex output)

validateInternalContinuation ::
    Rational ->
    [loop] ->
    [output] ->
    [[Rational]] ->
    [[Rational]] ->
    [[Rational]] ->
    FeedbackMeter (AffineFeedbackError loop output) ()
validateInternalContinuation gamma loops outputs exits continuation actual =
    mapM_ validateRow (zip4 loops exits continuation actual)
  where
    columns = columnsFor (length outputs) actual
    validateRow (loopValue, exitRow, continuationRow, actualRow) =
        mapM_ (validateEntry loopValue continuationRow) (zip4 outputs exitRow columns actualRow)
    validateEntry loopValue continuationRow (output, exitMass, column, result) = do
        direct <- feedbackMultiply FeedbackOtherIntermediatePhase "affine internal K equation direct" gamma exitMass
        productValue <- dot "affine internal K equation" continuationRow column
        discounted <- feedbackMultiply FeedbackOtherIntermediatePhase "affine internal K equation discount" gamma productValue
        rhs <- feedbackAdd FeedbackOtherIntermediatePhase "affine internal K equation RHS" direct discounted
        if result == rhs then pure () else throwFeedback (AffineFeedbackInternalContinuationEquationFailure loopValue output)

dot :: String -> [Rational] -> [Rational] -> FeedbackMeter error Rational
dot label left right = foldlM addTerm 0 (zip left right)
  where
    addTerm total (x, y) = do
        productValue <- feedbackMultiply FeedbackOtherIntermediatePhase (label ++ " product") x y
        feedbackAdd FeedbackOtherIntermediatePhase (label ++ " sum") total productValue

columnsFor :: Int -> [[Rational]] -> [[Rational]]
columnsFor count rows =
    [ [row !! column | row <- rows]
    | column <- [0 .. count - 1]
    ]

swapRows :: Int -> Int -> [a] -> [a]
swapRows left right rows
    | left == right = rows
    | otherwise =
        [ if index == left
            then rows !! right
            else if index == right then rows !! left else row
        | (index, row) <- zip [0 ..] rows
        ]

lookupLayout :: (Eq key) => key -> [key] -> [value] -> Maybe value
lookupLayout key keys values = lookup key (zip keys values)

foldlM :: (accumulator -> value -> FeedbackMeter error accumulator) -> accumulator -> [value] -> FeedbackMeter error accumulator
foldlM _ initial [] = pure initial
foldlM step initial (value : remaining) = do
    next <- step initial value
    foldlM step next remaining

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 as bs cs ds = [(a, b, c, d) | ((a, b), (c, d)) <- zip (zip as bs) (zip cs ds)]

cardinality :: FiniteSet value -> Natural
cardinality = fromIntegral . finiteSetCardinality
