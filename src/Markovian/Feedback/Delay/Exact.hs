{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

{- | Explicit one-tick delayed finite feedback.

The external input is held constant. A seed channel chooses @S0@ and each body
step jointly chooses @(Yt, S(t+1))@. This is causal state-machine execution,
not an instantaneous fixed point and not a universal guarded trace.
-}
module Markovian.Feedback.Delay.Exact (
    FeedbackLimits,
    feedbackLimits,
    FeedbackLimitDimension (..),
    FeedbackLimitError (..),
    FeedbackAccounting (..),
    LoopLayout,
    loopLayout,
    FeedbackTicks,
    FeedbackTicksError (..),
    feedbackTicks,
    feedbackTicksValue,
    FeedbackStep (..),
    DelayedFeedbackTrace (..),
    DelayedFeedbackError (..),
    DelayedFeedbackReport (..),
    CheckedDelayedFeedback,
    closeDelayedFeedback,
    observeDelayedTrace,
    observeDelayedFinal,
) where

import Markovian.Algebra.NonNegativeRational (NonNegativeRational, nonNegativeRational)
import Markovian.Category.Finite.Set (FiniteSet, finiteSetCardinality, finiteSetValues, sameFiniteLayout)
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))
import Markovian.Category.Matrix (Matrix, matrixFromRows)
import Markovian.Category.Matrix.Stochastic
import Markovian.Feedback.Internal
import Markovian.Horizon (Horizon, horizonValue)
import Numeric.Natural (Natural)

-- | A positive number of delayed ticks, needed when an output is requested.
newtype FeedbackTicks = UnsafeFeedbackTicks Natural
    deriving (Eq, Ord, Show)

-- | Zero ticks have an initial state but no final output.
data FeedbackTicksError = FeedbackTicksMustBePositive
    deriving (Eq, Show)

-- | Validate a positive tick count.
feedbackTicks :: Natural -> Either FeedbackTicksError FeedbackTicks
feedbackTicks 0 = Left FeedbackTicksMustBePositive
feedbackTicks value = Right (UnsafeFeedbackTicks value)

-- | Read a validated positive tick count.
feedbackTicksValue :: FeedbackTicks -> Natural
feedbackTicksValue (UnsafeFeedbackTicks value) = value

-- | One joint output and successor-state observation.
data FeedbackStep output state = FeedbackStep
    { feedbackStepOutput :: !output
    , feedbackStepSuccessor :: !state
    }
    deriving (Eq, Show)

-- | A complete bounded delayed execution, including the sampled initial state.
data DelayedFeedbackTrace state output = DelayedFeedbackTrace
    { delayedTraceInitialState :: !state
    , delayedTraceSteps :: ![FeedbackStep output state]
    }
    deriving (Eq, Show)

-- | Construction or bounded observation failure.
data DelayedFeedbackError
    = DelayedFeedbackLimitError !FeedbackLimitError
    | DelayedSeedSourceLayoutMismatch
    | DelayedSeedTargetLayoutMismatch
    | DelayedBodySourceLayoutMismatch
    | DelayedBodyTargetLayoutMismatch
    | DelayedInternalNormalizationFailure !String
    deriving (Eq, Show)

-- | Deterministic delayed-execution report.
data DelayedFeedbackReport owner = DelayedFeedbackReport
    { delayedFeedbackOwner :: !owner
    , delayedFeedbackTicks :: !Natural
    , delayedFeedbackTraceOutcomes :: !Natural
    , delayedFeedbackArithmeticWorkCharged :: !Natural
    , delayedFeedbackMaximumResultBits :: !Natural
    , delayedFeedbackAccounting :: !FeedbackAccounting
    }
    deriving (Eq, Show)

-- | Opaque checked delayed body and seed.
type role CheckedDelayedFeedback nominal nominal nominal nominal

data CheckedDelayedFeedback owner input state output where
    UnsafeCheckedDelayedFeedback ::
        (Eq state) =>
        !owner ->
        !(FiniteSet input) ->
        !(FiniteSet state) ->
        !(FiniteSet output) ->
        !(StochasticMatrix NonNegativeRational input state) ->
        !(StochasticMatrix NonNegativeRational (input, state) (output, state)) ->
        CheckedDelayedFeedback owner input state output

-- | Check the seed, one-tick body, endpoint layouts, and base dimensions.
closeDelayedFeedback ::
    (Eq input, Eq output) =>
    FeedbackLimits ->
    FiniteSet input ->
    LoopLayout owner state ->
    FiniteSet output ->
    StochasticMatrix NonNegativeRational input state ->
    StochasticMatrix NonNegativeRational (input, state) (output, state) ->
    Either DelayedFeedbackError (CheckedDelayedFeedback owner input state output)
closeDelayedFeedback limits inputs (UnsafeLoopLayout owner states) outputs seed body = do
    checkDimensions limits inputs states outputs
    if sameFiniteLayout inputs (stochasticSource seed) then Right () else Left DelayedSeedSourceLayoutMismatch
    if sameFiniteLayout states (stochasticTarget seed) then Right () else Left DelayedSeedTargetLayoutMismatch
    if sameFiniteLayout (productFiniteSet inputs states) (stochasticSource body) then Right () else Left DelayedBodySourceLayoutMismatch
    if sameFiniteLayout (productFiniteSet outputs states) (stochasticTarget body) then Right () else Left DelayedBodyTargetLayoutMismatch
    validateMatrixRationals limits "delayed seed input" seed
    validateMatrixRationals limits "delayed body input" body
    Right (UnsafeCheckedDelayedFeedback owner inputs states outputs seed body)

{- | Enumerate the complete joint trace law for a nonnegative horizon.

Horizon zero returns the seed-distributed initial state and an empty trace.
-}
observeDelayedTrace ::
    (Eq input, Eq output) =>
    FeedbackLimits ->
    Horizon ->
    CheckedDelayedFeedback owner input state output ->
    Either
        DelayedFeedbackError
        ( StochasticMatrix NonNegativeRational input (DelayedFeedbackTrace state output)
        , DelayedFeedbackReport owner
        )
observeDelayedTrace limits horizon (UnsafeCheckedDelayedFeedback owner inputs states outputs seed body) = do
    checkDimensions limits inputs states outputs
    validateMatrixRationals limits "delayed seed input" seed
    validateMatrixRationals limits "delayed body input" body
    let ticks = horizonValue horizon
        stateCount = cardinality states
        outputCount = cardinality outputs
    mapLimit $ checkLimit FeedbackTickCount (maximumFeedbackTicks limits) ticks
    branchCount <- mapLimit $ checkedProduct FeedbackTraceOutcomeCount [outputCount, stateCount]
    suffixCount <- mapLimit $ checkedPower FeedbackTraceOutcomeCount branchCount ticks
    outcomeCount <- mapLimit $ checkedProduct FeedbackTraceOutcomeCount [stateCount, suffixCount]
    mapLimit $ checkLimit FeedbackTraceOutcomeCount (maximumFeedbackTraceOutcomes limits) outcomeCount
    cells <- mapLimit $ checkedProduct FeedbackMatrixCellCount [cardinality inputs, outcomeCount]
    mapLimit $ checkLimit FeedbackMatrixCellCount (maximumFeedbackMatrixCells limits) cells
    let traces = traceValues ticks (finiteSetValues states) (finiteSetValues outputs)
        traceSet = UnsafeFiniteSet traces
        rawSeed = forgetStochastic seed
        rawBody = forgetStochastic body
    (rows, accounting) <- runFeedbackMeter limits DelayedFeedbackLimitError $ do
        recordMatrixInputs "delayed seed input" seed
        recordMatrixInputs "delayed body input" body
        probabilities <- traverse (\input -> traverse (traceProbability rawSeed rawBody input) traces) (finiteSetValues inputs)
        mapM_ (mapM_ (recordFeedbackRational FeedbackRetainedResultPhase "delayed retained trace mass")) probabilities
        validateAccountedRows "delayed trace normalization" probabilities
        pure probabilities
    channel <- buildStochastic inputs traceSet rows
    let report = DelayedFeedbackReport owner ticks outcomeCount (feedbackArithmeticWork accounting) (feedbackMaximumRetainedResultBits accounting) accounting
    Right (channel, report)
  where
    mapLimit = either (Left . DelayedFeedbackLimitError) Right

{- | Compute the final joint @(output,state)@ law after a positive tick count.

Output marginalization is deliberately not implicit: the returned channel
keeps the final output and successor state correlated.
-}
observeDelayedFinal ::
    (Eq input, Eq output) =>
    FeedbackLimits ->
    FeedbackTicks ->
    CheckedDelayedFeedback owner input state output ->
    Either
        DelayedFeedbackError
        ( StochasticMatrix NonNegativeRational input (output, state)
        , DelayedFeedbackReport owner
        )
observeDelayedFinal limits (UnsafeFeedbackTicks ticks) (UnsafeCheckedDelayedFeedback owner inputs states outputs seed body) = do
    checkDimensions limits inputs states outputs
    validateMatrixRationals limits "delayed seed input" seed
    validateMatrixRationals limits "delayed body input" body
    mapLimit $ checkLimit FeedbackTickCount (maximumFeedbackTicks limits) ticks
    let inputCount = cardinality inputs
        stateCount = cardinality states
        outputCount = cardinality outputs
    targetCount <- mapLimit $ checkedProduct FeedbackMatrixCellCount [outputCount, stateCount]
    cells <- mapLimit $ checkedProduct FeedbackMatrixCellCount [inputCount, targetCount]
    mapLimit $ checkLimit FeedbackMatrixCellCount (maximumFeedbackMatrixCells limits) cells
    let target = productFiniteSet outputs states
    (rows, accounting) <- runFeedbackMeter limits DelayedFeedbackLimitError $ do
        recordMatrixInputs "delayed seed input" seed
        recordMatrixInputs "delayed body input" body
        probabilities <- traverse (finalRow ticks states outputs seed body) (finiteSetValues inputs)
        mapM_ (mapM_ (recordFeedbackRational FeedbackRetainedResultPhase "delayed retained final mass")) probabilities
        validateAccountedRows "delayed final normalization" probabilities
        pure probabilities
    channel <- buildStochastic inputs target rows
    let report = DelayedFeedbackReport owner ticks targetCount (feedbackArithmeticWork accounting) (feedbackMaximumRetainedResultBits accounting) accounting
    Right (channel, report)
  where
    mapLimit = either (Left . DelayedFeedbackLimitError) Right

checkDimensions :: FeedbackLimits -> FiniteSet input -> FiniteSet state -> FiniteSet output -> Either DelayedFeedbackError ()
checkDimensions limits inputs states outputs = do
    let inputCount = cardinality inputs
        stateCount = cardinality states
        outputCount = cardinality outputs
    mapLimit $ checkLimit FeedbackSourceCount (maximumFeedbackSources limits) inputCount
    mapLimit $ checkLimit FeedbackLoopCount (maximumFeedbackLoops limits) stateCount
    mapLimit $ checkLimit FeedbackOutputCount (maximumFeedbackOutputs limits) outputCount
    seedCells <- mapLimit $ checkedProduct FeedbackMatrixCellCount [inputCount, stateCount]
    bodyCells <- mapLimit $ checkedProduct FeedbackMatrixCellCount [inputCount, stateCount, outputCount, stateCount]
    totalCells <- mapLimit $ checkedSum FeedbackMatrixCellCount [seedCells, bodyCells]
    mapLimit $ checkLimit FeedbackMatrixCellCount (maximumFeedbackMatrixCells limits) totalCells
  where
    mapLimit = either (Left . DelayedFeedbackLimitError) Right

traceValues :: Natural -> [state] -> [output] -> [DelayedFeedbackTrace state output]
traceValues ticks states outputs =
    [DelayedFeedbackTrace initial steps | initial <- states, steps <- suffixes ticks]
  where
    suffixes 0 = [[]]
    suffixes remaining =
        [ FeedbackStep output successor : rest
        | output <- outputs
        , successor <- states
        , rest <- suffixes (remaining - 1)
        ]

traceProbability ::
    Matrix NonNegativeRational input state ->
    Matrix NonNegativeRational (input, state) (output, state) ->
    input ->
    DelayedFeedbackTrace state output ->
    FeedbackMeter DelayedFeedbackError Rational
traceProbability seed body input (DelayedFeedbackTrace initial steps) = do
    start <- recordFeedbackRational FeedbackDelayedPathPhase "delayed trace seed" (matrixRationalEntry seed input initial)
    go start initial steps
  where
    go probability _ [] = pure probability
    go probability current (FeedbackStep output successor : remaining) = do
        -- Charge the represented branch before inspecting it or descending.
        chargeFeedbackWork 1
        next <- feedbackMultiply FeedbackDelayedPathPhase "delayed trace probability" probability (matrixRationalEntry body (input, current) (output, successor))
        go next successor remaining

finalRow ::
    Natural ->
    FiniteSet state ->
    FiniteSet output ->
    StochasticMatrix NonNegativeRational input state ->
    StochasticMatrix NonNegativeRational (input, state) (output, state) ->
    input ->
    FeedbackMeter DelayedFeedbackError [Rational]
finalRow ticks states outputs seed body input = do
    let seedMatrix = forgetStochastic seed
    initial <- traverse (recordFeedbackRational FeedbackDelayedPathPhase "delayed final seed" . matrixRationalEntry seedMatrix input) stateValues
    beforeFinal <- iterateState (ticks - 1) initial
    traverse
        (\(output, successor) -> dot beforeFinal [matrixRationalEntry bodyMatrix (input, state) (output, successor) | state <- stateValues])
        [(output, successor) | output <- finiteSetValues outputs, successor <- stateValues]
  where
    stateValues = finiteSetValues states
    bodyMatrix = forgetStochastic body
    iterateState 0 distribution = pure distribution
    iterateState remaining distribution = do
        next <- traverse nextMass stateValues
        iterateState (remaining - 1) next
      where
        nextMass successor = do
            transitions <- traverse (`transition` successor) stateValues
            dot distribution transitions
    transition state successor = foldlM addProbability 0 [matrixRationalEntry bodyMatrix (input, state) (output, successor) | output <- finiteSetValues outputs]
    addProbability total value = do
        chargeFeedbackWork 1
        feedbackAdd FeedbackDelayedPathPhase "delayed transition aggregate" total value
    dot left right = foldlM addTerm 0 (zip left right)
    addTerm total (x, y) = do
        chargeFeedbackWork 1
        productValue <- feedbackMultiply FeedbackDelayedPathPhase "delayed final product" x y
        feedbackAdd FeedbackDelayedPathPhase "delayed final sum" total productValue

validateAccountedRows :: String -> [[Rational]] -> FeedbackMeter DelayedFeedbackError ()
validateAccountedRows label = mapM_ validate
  where
    validate row = do
        mass <- foldlM (feedbackAdd FeedbackOtherIntermediatePhase label) 0 row
        if mass == 1
            then pure ()
            else throwFeedback (DelayedInternalNormalizationFailure (label ++ ": row mass " ++ show mass))

recordMatrixInputs :: String -> StochasticMatrix NonNegativeRational source target -> FeedbackMeter DelayedFeedbackError ()
recordMatrixInputs label channel =
    mapM_
        (recordFeedbackRational FeedbackInputPhase label)
        [ matrixRationalEntry raw source target
        | source <- finiteSetValues (stochasticSource channel)
        , target <- finiteSetValues (stochasticTarget channel)
        ]
  where
    raw = forgetStochastic channel

validateMatrixRationals :: FeedbackLimits -> String -> StochasticMatrix NonNegativeRational source target -> Either DelayedFeedbackError ()
validateMatrixRationals limits phase channel =
    mapM_
        ( either (Left . DelayedFeedbackLimitError) (const (Right ()))
            . checkRational (maximumFeedbackRationalBits limits) phase
        )
        [ matrixRationalEntry raw source target
        | source <- finiteSetValues (stochasticSource channel)
        , target <- finiteSetValues (stochasticTarget channel)
        ]
  where
    raw = forgetStochastic channel

buildStochastic ::
    (Eq source, Eq target) =>
    FiniteSet source ->
    FiniteSet target ->
    [[Rational]] ->
    Either DelayedFeedbackError (StochasticMatrix NonNegativeRational source target)
buildStochastic sources targets rows = do
    converted <- traverse (traverse convert) rows
    matrix <- case matrixFromRows sources targets converted of
        Left failure -> Left (DelayedInternalNormalizationFailure (show failure))
        Right result -> Right result
    case stochasticMatrix matrix of
        Left failure -> Left (DelayedInternalNormalizationFailure (show failure))
        Right channel -> Right channel
  where
    convert value = case nonNegativeRational value of
        Left failure -> Left (DelayedInternalNormalizationFailure (show failure))
        Right result -> Right result

foldlM :: (accumulator -> value -> FeedbackMeter error accumulator) -> accumulator -> [value] -> FeedbackMeter error accumulator
foldlM _ initial [] = pure initial
foldlM step initial (value : remaining) = do
    next <- step initial value
    foldlM step next remaining

cardinality :: FiniteSet value -> Natural
cardinality = fromIntegral . finiteSetCardinality
