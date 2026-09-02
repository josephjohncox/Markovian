{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

{- | Checked exact first-exit feedback for finite normalized channels.

This module implements coproduct path elimination. It is not the Cartesian
matrix trace and does not define a @Traced@ instance. A checked channel

@K : input + loop -> output + loop@

is closed only when every represented loop state has a positive-mass path to
an output. The implementation solves @H = C + D H@ exactly and validates the
result and normalization before returning an opaque witness. It defines no
@Traced@ instance.
-}
module Markovian.Feedback.Channel.Exact (
    FeedbackLimits,
    feedbackLimits,
    FeedbackLimitDimension (..),
    FeedbackLimitError (..),
    FeedbackAccounting (..),
    LoopLayout,
    loopLayout,
    FeedbackClass (..),
    ExactFeedbackError (..),
    ExactFeedbackReport (..),
    CheckedFeedback,
    closeProperFeedback,
    feedbackChannel,
    feedbackReport,
) where

import Data.List (findIndex, foldl', transpose)
import Data.Maybe (listToMaybe)
import Markovian.Algebra.NonNegativeRational (NonNegativeRational, nonNegativeRational)
import Markovian.Category.Finite.Set (FiniteSet, finiteSetCardinality, finiteSetValues, sameFiniteLayout)
import Markovian.Category.Matrix (Matrix, matrixFromRows, matrixSource, matrixTarget)
import Markovian.Category.Matrix.Stochastic
import Markovian.Feedback.Internal
import Numeric.Natural (Natural)

-- | Classification derived from the represented internal block.
data FeedbackClass
    = EmptyLoop
    | NilpotentLoop !Natural
    | ProperTransientLoop !Natural !Rational
    deriving (Eq, Show)

-- | Exact first-exit construction failure.
data ExactFeedbackError loop
    = ExactFeedbackLimitError !FeedbackLimitError
    | FeedbackSourceLayoutMismatch
    | FeedbackTargetLayoutMismatch
    | FeedbackClosedInternalClass ![loop]
    | FeedbackSingularInternalSystem
    | FeedbackNegativeAbsorptionMass !loop !Rational
    | FeedbackAbsorptionIdentityFailure !loop
    | FeedbackOutputNormalizationFailure !Int !Rational
    | FeedbackInternalConstructionFailure !String
    deriving (Eq, Show)

-- | Deterministic semantic report. It intentionally contains no clock time.
data ExactFeedbackReport owner = ExactFeedbackReport
    { exactFeedbackOwner :: !owner
    , exactFeedbackClass :: !FeedbackClass
    , exactFeedbackSourceCount :: !Natural
    , exactFeedbackLoopCount :: !Natural
    , exactFeedbackOutputCount :: !Natural
    , exactFeedbackGraphWorkCharged :: !Natural
    , exactFeedbackArithmeticWorkCharged :: !Natural
    , exactFeedbackMaximumResultBits :: !Natural
    , exactFeedbackAccounting :: !FeedbackAccounting
    }
    deriving (Eq, Show)

-- | Opaque evidence that properness, exact solve identities, and normalization passed.
type role CheckedFeedback nominal nominal nominal nominal

data CheckedFeedback owner input loop output
    = UnsafeCheckedFeedback
        !(StochasticMatrix NonNegativeRational input output)
        !(ExactFeedbackReport owner)

{- | Close a finite normalized routing channel by proper first exit.

Every represented loop state is checked, including states unreachable from
an external input. Rational-size checks occur after each exact operation;
they bound accepted results, not transient GHC heap allocation.
-}
closeProperFeedback ::
    FeedbackLimits ->
    FiniteSet input ->
    LoopLayout owner loop ->
    FiniteSet output ->
    StochasticMatrix NonNegativeRational (Either input loop) (Either output loop) ->
    Either (ExactFeedbackError loop) (CheckedFeedback owner input loop output)
closeProperFeedback limits inputs (UnsafeLoopLayout owner loops) outputs channel = do
    let sourceCount = naturalCardinality inputs
        loopCount = naturalCardinality loops
        outputCount = naturalCardinality outputs
        raw = forgetStochastic channel
    mapLimit $ checkLimit FeedbackSourceCount (maximumFeedbackSources limits) sourceCount
    mapLimit $ checkLimit FeedbackLoopCount (maximumFeedbackLoops limits) loopCount
    mapLimit $ checkLimit FeedbackOutputCount (maximumFeedbackOutputs limits) outputCount
    combinedSources <- mapLimit $ checkedSum FeedbackMatrixCellCount [sourceCount, loopCount]
    combinedTargets <- mapLimit $ checkedSum FeedbackMatrixCellCount [outputCount, loopCount]
    cells <- mapLimit $ checkedProduct FeedbackMatrixCellCount [combinedSources, combinedTargets]
    mapLimit $ checkLimit FeedbackMatrixCellCount (maximumFeedbackMatrixCells limits) cells
    graphWidth <- mapLimit $ checkedSum FeedbackGraphWork [loopCount, outputCount]
    graphWork <- mapLimit $ checkedProduct FeedbackGraphWork [loopCount, graphWidth]
    mapLimit $ checkLimit FeedbackGraphWork (maximumFeedbackGraphWork limits) graphWork
    -- Layout construction occurs only after all combined cardinalities and
    -- matrix cells have passed preflight.
    let expectedSource = sumFiniteSet inputs loops
        expectedTarget = sumFiniteSet outputs loops
    if sameFiniteLayout expectedSource (matrixSource raw)
        then Right ()
        else Left FeedbackSourceLayoutMismatch
    if sameFiniteLayout expectedTarget (matrixTarget raw)
        then Right ()
        else Left FeedbackTargetLayoutMismatch

    let d = [[entry raw (Right sourceLoop) (Right targetLoop) | targetLoop <- finiteSetValues loops] | sourceLoop <- finiteSetValues loops]
        c = [[entry raw (Right sourceLoop) (Left targetOutput) | targetOutput <- finiteSetValues outputs] | sourceLoop <- finiteSetValues loops]
        a = [[entry raw (Left sourceInput) (Left targetOutput) | targetOutput <- finiteSetValues outputs] | sourceInput <- finiteSetValues inputs]
        b = [[entry raw (Left sourceInput) (Right targetLoop) | targetLoop <- finiteSetValues loops] | sourceInput <- finiteSetValues inputs]
        reachable = reachableExits loops outputs raw
        closed = [loopValue | (loopValue, canExit) <- zip (finiteSetValues loops) reachable, not canExit]

    ((feedbackClass, f), accounting) <-
        runFeedbackMeter limits ExactFeedbackLimitError $ do
            mapM_ (mapM_ (recordFeedbackRational FeedbackInputPhase "feedback input")) [concat d, concat c, concat a, concat b]
            if null closed
                then pure ()
                else throwFeedback (FeedbackClosedInternalClass closed)
            powers <- matrixPowerSequenceAccounted d
            let nilpotent = listToMaybe [index | (index, power) <- powers, all (all (== 0)) power]
            feedbackClass <- case (loopCount, nilpotent) of
                (0, _) -> pure EmptyLoop
                (_, Just index) -> pure (NilpotentLoop index)
                (_, Nothing) -> case reverse powers of
                    [] -> throwFeedback (FeedbackInternalConstructionFailure "missing transience power")
                    (_, power) : _ -> do
                        beta <- maximumRowMassAccounted FeedbackMatrixPowerPhase "transience witness sum" power
                        if beta < 1
                            then pure (ProperTransientLoop loopCount beta)
                            else throwFeedback (FeedbackInternalConstructionFailure "proper graph did not yield beta < 1")
            h <- if loopCount == 0 then pure [] else solveAbsorption d c
            validateAbsorption loops outputs raw h
            f <-
                if loopCount == 0
                    then pure a
                    else do
                        bh <- multiplyRect "first-exit composition" b h
                        addRect "first-exit composition" a bh
            case validateNonnegative loops outputs h of
                Left failure -> throwFeedback failure
                Right () -> pure ()
            validateRows f
            mapM_ (mapM_ (recordFeedbackRational FeedbackRetainedResultPhase "feedback retained result")) f
            pure (feedbackClass, f)
    outputMatrix <- buildStochastic inputs outputs f
    let maximumBits = feedbackMaximumRetainedResultBits accounting
        report =
            ExactFeedbackReport
                { exactFeedbackOwner = owner
                , exactFeedbackClass = feedbackClass
                , exactFeedbackSourceCount = sourceCount
                , exactFeedbackLoopCount = loopCount
                , exactFeedbackOutputCount = outputCount
                , exactFeedbackGraphWorkCharged = graphWork
                , exactFeedbackArithmeticWorkCharged = feedbackArithmeticWork accounting
                , exactFeedbackMaximumResultBits = maximumBits
                , exactFeedbackAccounting = accounting
                }
    Right (UnsafeCheckedFeedback outputMatrix report)
  where
    mapLimit = either (Left . ExactFeedbackLimitError) Right
    entry = matrixRationalEntry

-- | Read the normalized first-exit channel.
feedbackChannel :: CheckedFeedback owner input loop output -> StochasticMatrix NonNegativeRational input output
feedbackChannel (UnsafeCheckedFeedback channel _) = channel

-- | Read deterministic properness and work evidence.
feedbackReport :: CheckedFeedback owner input loop output -> ExactFeedbackReport owner
feedbackReport (UnsafeCheckedFeedback _ report) = report

naturalCardinality :: FiniteSet value -> Natural
naturalCardinality = fromIntegral . finiteSetCardinality

reachableExits ::
    FiniteSet loop ->
    FiniteSet output ->
    Matrix NonNegativeRational (Either input loop) (Either output loop) ->
    [Bool]
reachableExits loops outputs matrix = visit initial seedQueue
  where
    loopValues = finiteSetValues loops
    outputValues = finiteSetValues outputs
    indexedLoops = zip [0 ..] loopValues
    direct source = any (\output -> matrixRationalEntry matrix (Right source) (Left output) > 0) outputValues
    initial = map direct loopValues
    seedQueue = [index | (index, canExit) <- zip [0 ..] initial, canExit]
    predecessors =
        [ [sourceIndex | (sourceIndex, source) <- indexedLoops, matrixRationalEntry matrix (Right source) (Right target) > 0]
        | target <- loopValues
        ]
    visit known [] = known
    visit known (targetIndex : remaining) =
        let newlyReached = [index | index <- predecessors !! targetIndex, not (known !! index)]
            nextKnown = foldl' (\flags index -> replaceAt index True flags) known newlyReached
         in visit nextKnown (remaining ++ newlyReached)
    replaceAt index value values = [if current == index then value else old | (current, old) <- zip [0 ..] values]

solveAbsorption :: [[Rational]] -> [[Rational]] -> FeedbackMeter (ExactFeedbackError loop) [[Rational]]
solveAbsorption d c = do
    coefficient <-
        traverse
            (\row -> traverse (\column -> feedbackSubtract FeedbackGaussianPhase "Gaussian coefficient" (if row == column then 1 else 0) (d !! row !! column)) [0 .. size - 1])
            [0 .. size - 1]
    mapM_ (mapM_ (recordFeedbackRational FeedbackGaussianPhase "Gaussian augmented input")) c
    gaussian (zipWith (++) coefficient c) 0
  where
    size = length d

    gaussian rows column
        | column == size = pure (map (drop size) rows)
        | otherwise = do
            pivotOffset <- maybe (throwFeedback FeedbackSingularInternalSystem) pure (findIndex ((/= 0) . (!! column)) (drop column rows))
            let pivotIndex = column + pivotOffset
                swapped = swapRows column pivotIndex rows
                pivot = swapped !! column !! column
            _ <- recordFeedbackRational FeedbackGaussianPhase "Gaussian pivot" pivot
            pivotRow <- traverse (\value -> feedbackDivide FeedbackGaussianPhase "Gaussian quotient" value pivot) (swapped !! column)
            eliminated <- traverse (eliminate column pivotRow) (zip [0 ..] swapped)
            gaussian eliminated (column + 1)

    eliminate pivotColumn pivotRow (rowIndex, row)
        | rowIndex == pivotColumn = pure pivotRow
        | otherwise = do
            let factor = row !! pivotColumn
            _ <- recordFeedbackRational FeedbackGaussianPhase "Gaussian elimination factor" factor
            traverse (eliminateEntry factor) (zip row pivotRow)

    eliminateEntry factor (value, pivotValue) = do
        productValue <- feedbackMultiply FeedbackGaussianPhase "Gaussian discarded product" factor pivotValue
        feedbackSubtract FeedbackGaussianPhase "Gaussian elimination difference" value productValue

swapRows :: Int -> Int -> [a] -> [a]
swapRows left right rows
    | left == right = rows
    | otherwise =
        [ if index == left
            then rows !! right
            else if index == right then rows !! left else row
        | (index, row) <- zip [0 ..] rows
        ]

multiplyRect :: String -> [[Rational]] -> [[Rational]] -> FeedbackMeter (ExactFeedbackError loop) [[Rational]]
multiplyRect _ [] _ = pure []
multiplyRect _ rows [] = pure (replicate (length rows) [])
multiplyRect phase left right = traverse rowProduct left
  where
    rightColumns = transpose right
    rowProduct row = traverse (dot row) rightColumns
    dot row column = foldlM addTerm 0 (zip row column)
    addTerm total (x, y) = do
        productValue <- feedbackMultiply FeedbackOtherIntermediatePhase (phase ++ " product") x y
        feedbackAdd FeedbackOtherIntermediatePhase (phase ++ " sum") total productValue

addRect :: String -> [[Rational]] -> [[Rational]] -> FeedbackMeter (ExactFeedbackError loop) [[Rational]]
addRect phase left right = traverse (traverse addPair) (zipWith zip left right)
  where
    addPair (x, y) = feedbackAdd FeedbackOtherIntermediatePhase (phase ++ " sum") x y

validateAbsorption ::
    FiniteSet loop ->
    FiniteSet output ->
    Matrix NonNegativeRational (Either input loop) (Either output loop) ->
    [[Rational]] ->
    FeedbackMeter (ExactFeedbackError loop) ()
validateAbsorption loops outputs matrix h = mapM_ validateRow (zip4 [0 ..] loopValues c h)
  where
    loopValues = finiteSetValues loops
    outputValues = finiteSetValues outputs
    c = [[matrixRationalEntry matrix (Right source) (Left output) | output <- outputValues] | source <- loopValues]
    d = [[matrixRationalEntry matrix (Right source) (Right target) | target <- loopValues] | source <- loopValues]
    validateRow (rowIndex, source, cRow, hRow) = do
        products <- traverse (dot (d !! rowIndex)) (transpose h)
        rhs <- traverse checkedAdd (zip cRow products)
        if rhs == hRow then pure () else throwFeedback (FeedbackAbsorptionIdentityFailure source)
    dot row column = foldlM addProduct 0 (zip row column)
    addProduct total (left, right) = do
        productValue <- feedbackMultiply FeedbackOtherIntermediatePhase "absorption validation product" left right
        feedbackAdd FeedbackOtherIntermediatePhase "absorption validation sum" total productValue
    checkedAdd (left, right) = feedbackAdd FeedbackOtherIntermediatePhase "absorption validation rhs" left right

validateNonnegative :: FiniteSet loop -> FiniteSet output -> [[Rational]] -> Either (ExactFeedbackError loop) ()
validateNonnegative loops _ h =
    mapM_
        ( \(loopValue, row) -> case filter (< 0) row of
            value : _ -> Left (FeedbackNegativeAbsorptionMass loopValue value)
            [] -> Right ()
        )
        (zip (finiteSetValues loops) h)

validateRows :: [[Rational]] -> FeedbackMeter (ExactFeedbackError loop) ()
validateRows rows = mapM_ validate (zip [0 ..] rows)
  where
    validate (index, row) = do
        mass <- foldlM (feedbackAdd FeedbackOtherIntermediatePhase "output normalization") 0 row
        if mass == 1 then pure () else throwFeedback (FeedbackOutputNormalizationFailure index mass)

buildStochastic ::
    FiniteSet input ->
    FiniteSet output ->
    [[Rational]] ->
    Either (ExactFeedbackError loop) (StochasticMatrix NonNegativeRational input output)
buildStochastic inputs outputs rows = do
    converted <- traverse (traverse convert) rows
    matrix <- case matrixFromRows inputs outputs converted of
        Left failure -> Left (FeedbackInternalConstructionFailure (show failure))
        Right result -> Right result
    case stochasticMatrix matrix of
        Left failure -> Left (FeedbackInternalConstructionFailure (show failure))
        Right result -> Right result
  where
    convert value = case nonNegativeRational value of
        Left failure -> Left (FeedbackInternalConstructionFailure (show failure))
        Right scalar -> Right scalar

foldlM :: (accumulator -> value -> FeedbackMeter error accumulator) -> accumulator -> [value] -> FeedbackMeter error accumulator
foldlM _ initial [] = pure initial
foldlM step initial (value : remaining) = do
    next <- step initial value
    foldlM step next remaining

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 as bs cs ds = [(a, b, c, d) | ((a, b), (c, d)) <- zip (zip as bs) (zip cs ds)]
