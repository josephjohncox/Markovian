{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

{- | Reward- and duration-preserving exact feedback for nilpotent loops.

A cyclic transient loop can have unbounded reward and duration support, so this
finite-distribution API rejects it even when its marginal first-exit channel is
proper. Rewards, durations, and outputs remain one joint outcome.
-}
module Markovian.Feedback.Timed.Exact (
    FeedbackLimits,
    feedbackLimits,
    FeedbackLimitDimension (..),
    FeedbackLimitError (..),
    FeedbackAccounting (..),
    LoopLayout,
    loopLayout,
    FeedbackEvent (..),
    TimedFeedbackExit (..),
    TimedFeedbackError (..),
    TimedFeedbackReport (..),
    CheckedTimedFeedback,
    closeNilpotentTimedFeedback,
    timedFeedbackChannel,
    timedFeedbackReport,
) where

import Data.List (findIndex, nub)
import Data.Maybe (listToMaybe)
import Markovian.Algebra.NonNegativeRational (NonNegativeRational, nonNegativeRational)
import Markovian.Category.Finite.Set (FiniteSet, finiteSetCardinality, finiteSetValues, sameFiniteLayout)
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))
import Markovian.Category.Matrix (Matrix, matrixFromRows)
import Markovian.Category.Matrix.Stochastic
import Markovian.Feedback.Internal
import Markovian.Objective.Exact (ExactDiscount, exactDiscountValue)
import Markovian.Reward.Exact (ExactReward, exactReward, exactRewardValue)
import Numeric.Natural (Natural)

-- | One feedback microstep. Reward is paid exactly once on this event.
data FeedbackEvent loop output
    = Continue !ExactReward !loop
    | Exit !ExactReward !output
    deriving (Eq, Show)

-- | A finite first-exit outcome retaining accumulated reward and duration.
data TimedFeedbackExit output = TimedFeedbackExit
    { timedFeedbackReward :: !ExactReward
    , timedFeedbackDuration :: !Natural
    , timedFeedbackOutput :: !output
    }
    deriving (Eq, Show)

-- | Timed feedback construction failure.
data TimedFeedbackError loop output
    = TimedFeedbackLimitError !FeedbackLimitError
    | TimedFeedbackSourceLayoutMismatch
    | TimedFeedbackContinueOutsideLoop !loop
    | TimedFeedbackExitOutsideOutput !output
    | TimedFeedbackRequiresNilpotent
    | TimedFeedbackPathExceededWitness !Natural
    | TimedFeedbackNoExitForInput !Int
    | TimedFeedbackInternalNormalizationFailure !String
    deriving (Eq, Show)

-- | Deterministic evidence for a nilpotent timed closure.
data TimedFeedbackReport owner = TimedFeedbackReport
    { timedFeedbackOwner :: !owner
    , timedFeedbackNilpotenceIndex :: !Natural
    , timedFeedbackMaximumDuration :: !Natural
    , timedFeedbackOutcomeCount :: !Natural
    , timedFeedbackArithmeticWorkCharged :: !Natural
    , timedFeedbackMaximumResultBits :: !Natural
    , timedFeedbackAccounting :: !FeedbackAccounting
    }
    deriving (Eq, Show)

-- | Opaque validated timed feedback result.
type role CheckedTimedFeedback nominal nominal nominal nominal

data CheckedTimedFeedback owner input loop output
    = UnsafeCheckedTimedFeedback
        !(StochasticMatrix NonNegativeRational input (TimedFeedbackExit output))
        !(TimedFeedbackReport owner)

{- | Close a rewardful feedback channel only when its internal continuation
block is nilpotent.

For duration @d@ and discount @gamma@, the returned reward is
@r0 + gamma*r1 + ... + gamma^(d-1)*r(d-1)@. An outer continuation must use
@G + gamma^d V(output)@.
-}
closeNilpotentTimedFeedback ::
    (Eq input, Eq output) =>
    FeedbackLimits ->
    ExactDiscount ->
    FiniteSet input ->
    LoopLayout owner loop ->
    FiniteSet output ->
    StochasticMatrix NonNegativeRational (Either input loop) (FeedbackEvent loop output) ->
    Either (TimedFeedbackError loop output) (CheckedTimedFeedback owner input loop output)
closeNilpotentTimedFeedback limits discount inputs (UnsafeLoopLayout owner loops) outputs channel = do
    let sourceCount = cardinality inputs
        loopCount = cardinality loops
        outputCount = cardinality outputs
        events = finiteSetValues (stochasticTarget channel)
    mapLimit $ checkLimit FeedbackSourceCount (maximumFeedbackSources limits) sourceCount
    mapLimit $ checkLimit FeedbackLoopCount (maximumFeedbackLoops limits) loopCount
    mapLimit $ checkLimit FeedbackOutputCount (maximumFeedbackOutputs limits) outputCount
    mapM_ validateEvent events
    combinedSources <- mapLimit $ checkedSum FeedbackMatrixCellCount [sourceCount, loopCount]
    cells <- mapLimit $ checkedProduct FeedbackMatrixCellCount [combinedSources, fromIntegral (length events)]
    mapLimit $ checkLimit FeedbackMatrixCellCount (maximumFeedbackMatrixCells limits) cells
    loopFactor <- mapLimit $ checkedSum FeedbackGraphWork [loopCount, 1]
    eventFactor <- mapLimit $ checkedSum FeedbackGraphWork [fromIntegral (length events), 1]
    graphWork <- mapLimit $ checkedProduct FeedbackGraphWork [loopFactor, eventFactor]
    mapLimit $ checkLimit FeedbackGraphWork (maximumFeedbackGraphWork limits) graphWork
    let expectedSource = sumFiniteSet inputs loops
    if sameFiniteLayout expectedSource (stochasticSource channel) then Right () else Left TimedFeedbackSourceLayoutMismatch
    let raw = forgetStochastic channel
    ((indexValue, maximumDuration, outcomeValues, rationalRows), accounting) <-
        runFeedbackMeter limits TimedFeedbackLimitError $ do
            _ <- recordFeedbackRational FeedbackInputPhase "timed discount input" (exactDiscountValue discount)
            mapM_ (recordEventReward "timed event reward") events
            mapM_
                (recordFeedbackRational FeedbackInputPhase "timed channel input")
                [matrixRationalEntry raw source event | source <- finiteSetValues expectedSource, event <- events]
            d <- traverse (\source -> traverse (continuationMass events raw source) (finiteSetValues loops)) (finiteSetValues loops)
            powers <- matrixPowerSequenceAccounted d
            let nilpotent = listToMaybe [index | (index, power) <- powers, all (all (== 0)) power]
            indexValue <- case (loopCount, nilpotent) of
                (0, _) -> pure 0
                (_, Just value) -> pure value
                (_, Nothing) -> throwFeedback TimedFeedbackRequiresNilpotent
            maximumDuration <- either (throwFeedback . TimedFeedbackLimitError) pure (checkedSum FeedbackTickCount [indexValue, 1])
            pathBound <- either (throwFeedback . TimedFeedbackLimitError) pure (checkedPower FeedbackTraceOutcomeCount (fromIntegral (length events)) maximumDuration)
            case checkLimit FeedbackTraceOutcomeCount (maximumFeedbackTraceOutcomes limits) pathBound of
                Left failure -> throwFeedback (TimedFeedbackLimitError failure)
                Right () -> pure ()
            rows <- traverse (enumerateInput discount indexValue events raw) (finiteSetValues inputs)
            case findIndex null rows of
                Just row -> throwFeedback (TimedFeedbackNoExitForInput row)
                Nothing -> pure ()
            let outcomeValues = nub [outcome | row <- rows, (outcome, _) <- row]
            case checkLimit FeedbackTraceOutcomeCount (maximumFeedbackTraceOutcomes limits) (fromIntegral (length outcomeValues)) of
                Left failure -> throwFeedback (TimedFeedbackLimitError failure)
                Right () -> pure ()
            rationalRows <- traverse (\row -> traverse (aggregateMass row) outcomeValues) rows
            mapM_ (recordOutcome rationalRows) (zip [0 ..] outcomeValues)
            validateAccountedRows rationalRows
            pure (indexValue, maximumDuration, outcomeValues, rationalRows)
    let outcomeSet = UnsafeFiniteSet outcomeValues
    result <- buildStochastic inputs outcomeSet rationalRows
    let report =
            TimedFeedbackReport
                { timedFeedbackOwner = owner
                , timedFeedbackNilpotenceIndex = indexValue
                , timedFeedbackMaximumDuration = maximumDuration
                , timedFeedbackOutcomeCount = fromIntegral (length outcomeValues)
                , timedFeedbackArithmeticWorkCharged = feedbackArithmeticWork accounting
                , timedFeedbackMaximumResultBits = feedbackMaximumRetainedResultBits accounting
                , timedFeedbackAccounting = accounting
                }
    Right (UnsafeCheckedTimedFeedback result report)
  where
    mapLimit = either (Left . TimedFeedbackLimitError) Right
    validateEvent (Continue _ loopValue)
        | loopValue `elem` finiteSetValues loops = Right ()
        | otherwise = Left (TimedFeedbackContinueOutsideLoop loopValue)
    validateEvent (Exit _ output)
        | output `elem` finiteSetValues outputs = Right ()
        | otherwise = Left (TimedFeedbackExitOutsideOutput output)
    recordEventReward label event =
        recordFeedbackRational FeedbackInputPhase label $ case event of
            Continue reward _ -> exactRewardValue reward
            Exit reward _ -> exactRewardValue reward
    continuationMass events raw source targetLoop =
        foldlM
            (feedbackAdd FeedbackMatrixPowerPhase "timed continuation aggregate")
            0
            [matrixRationalEntry raw (Right source) event | event@(Continue _ target) <- events, target == targetLoop]
    aggregateMass row outcome =
        foldlM
            (feedbackAdd FeedbackTimedPathPhase "timed outcome aggregate")
            0
            [mass | (candidate, mass) <- row, candidate == outcome]
    recordOutcome rows (column, outcome) = do
        _ <- recordFeedbackRational FeedbackRetainedResultPhase "timed retained reward" (exactRewardValue (timedFeedbackReward outcome))
        mapM_ (\row -> recordFeedbackRational FeedbackRetainedResultPhase "timed retained mass" (row !! column)) rows
    validateAccountedRows = mapM_ validate
      where
        validate row = do
            mass <- foldlM (feedbackAdd FeedbackOtherIntermediatePhase "timed output normalization") 0 row
            if mass == 1
                then pure ()
                else throwFeedback (TimedFeedbackInternalNormalizationFailure ("timed output normalization: row mass " ++ show mass))

-- | Read the finite joint law of accumulated reward, duration, and output.
timedFeedbackChannel :: CheckedTimedFeedback owner input loop output -> StochasticMatrix NonNegativeRational input (TimedFeedbackExit output)
timedFeedbackChannel (UnsafeCheckedTimedFeedback channel _) = channel

-- | Read deterministic nilpotence and resource evidence.
timedFeedbackReport :: CheckedTimedFeedback owner input loop output -> TimedFeedbackReport owner
timedFeedbackReport (UnsafeCheckedTimedFeedback _ report) = report

enumerateInput ::
    ExactDiscount ->
    Natural ->
    [FeedbackEvent loop output] ->
    Matrix NonNegativeRational (Either input loop) (FeedbackEvent loop output) ->
    input ->
    FeedbackMeter (TimedFeedbackError loop output) [(TimedFeedbackExit output, Rational)]
enumerateInput discount nilpotentIndex events matrix input =
    walk 0 0 1 (Left input)
  where
    gamma = exactDiscountValue discount
    walk duration reward probability source
        | duration > nilpotentIndex = throwFeedback (TimedFeedbackPathExceededWitness duration)
        | otherwise = fmap concat . traverse (branch duration reward probability source) $ events
    branch duration reward probability source event = do
        -- Zero-probability branches still consume their visit and product
        -- before the implementation decides not to descend.
        chargeFeedbackWork 1
        nextProbability <- feedbackMultiply FeedbackTimedPathPhase "timed path probability" probability (matrixRationalEntry matrix source event)
        if nextProbability == 0
            then pure []
            else do
                let eventReward = case event of
                        Continue value _ -> exactRewardValue value
                        Exit value _ -> exactRewardValue value
                gammaPower <- feedbackPower FeedbackTimedPathPhase "timed discount power" gamma duration
                discounted <- feedbackMultiply FeedbackTimedPathPhase "timed reward discount" gammaPower eventReward
                nextReward <- feedbackAdd FeedbackTimedPathPhase "timed accumulated reward" reward discounted
                case event of
                    Exit _ output -> pure [(TimedFeedbackExit (exactReward nextReward) (duration + 1) output, nextProbability)]
                    Continue _ loopValue -> walk (duration + 1) nextReward nextProbability (Right loopValue)

buildStochastic ::
    (Eq input, Eq output) =>
    FiniteSet input ->
    FiniteSet output ->
    [[Rational]] ->
    Either (TimedFeedbackError loop originalOutput) (StochasticMatrix NonNegativeRational input output)
buildStochastic inputs outputs rows = do
    converted <- traverse (traverse convert) rows
    matrix <- case matrixFromRows inputs outputs converted of
        Left failure -> Left (TimedFeedbackInternalNormalizationFailure (show failure))
        Right result -> Right result
    case stochasticMatrix matrix of
        Left failure -> Left (TimedFeedbackInternalNormalizationFailure (show failure))
        Right result -> Right result
  where
    convert value = case nonNegativeRational value of
        Left failure -> Left (TimedFeedbackInternalNormalizationFailure (show failure))
        Right result -> Right result

foldlM :: (accumulator -> value -> FeedbackMeter error accumulator) -> accumulator -> [value] -> FeedbackMeter error accumulator
foldlM _ initial [] = pure initial
foldlM step initial (value : remaining) = do
    next <- step initial value
    foldlM step next remaining

cardinality :: FiniteSet value -> Natural
cardinality = fromIntegral . finiteSetCardinality
