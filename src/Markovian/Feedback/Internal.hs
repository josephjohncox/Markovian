{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Markovian.Feedback.Internal (
    FeedbackLimits (..),
    FeedbackLimitDimension (..),
    FeedbackLimitError (..),
    FeedbackPhase (..),
    FeedbackAccounting (..),
    FeedbackMeter,
    feedbackLimits,
    runFeedbackMeter,
    throwFeedback,
    chargeFeedbackWork,
    recordFeedbackRational,
    feedbackAdd,
    feedbackSubtract,
    feedbackMultiply,
    feedbackDivide,
    feedbackPower,
    LoopLayout (..),
    loopLayout,
    checkLimit,
    checkedProduct,
    checkedSum,
    checkedPower,
    rationalBits,
    checkRational,
    checkedRationalPower,
    sumFiniteSet,
    productFiniteSet,
    matrixRationalEntry,
    nilpotenceIndex,
    matrixMultiply,
    matrixPower,
    matrixPowerSequence,
    matrixPowerSequenceAccounted,
    maximumRowMass,
    maximumRowMassChecked,
    maximumRowMassAccounted,
) where

import Data.Bits (shiftR)
import Data.List (foldl')
import Data.Ratio (denominator, numerator)
import Markovian.Algebra.NonNegativeRational (NonNegativeRational, getNonNegativeRational)
import Markovian.Category.Finite.Set (FiniteSet)
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))
import Markovian.Category.Matrix (Matrix, matrixEntry)
import Numeric.Natural (Natural)

-- | Operation-wide limits. Known dimensions are checked before result allocation.
data FeedbackLimits = FeedbackLimits
    { maximumFeedbackSources :: !Natural
    , maximumFeedbackLoops :: !Natural
    , maximumFeedbackOutputs :: !Natural
    , maximumFeedbackTicks :: !Natural
    , maximumFeedbackMatrixCells :: !Natural
    , maximumFeedbackGraphWork :: !Natural
    , maximumFeedbackArithmeticWork :: !Natural
    , maximumFeedbackTraceOutcomes :: !Natural
    , maximumFeedbackRationalBits :: !Natural
    }
    deriving (Eq, Show)

-- | Construct a limit record. All fields are independent hard limits.
feedbackLimits :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> FeedbackLimits
feedbackLimits = FeedbackLimits

-- | A dimension charged by a feedback operation.
data FeedbackLimitDimension
    = FeedbackSourceCount
    | FeedbackLoopCount
    | FeedbackOutputCount
    | FeedbackMatrixCellCount
    | FeedbackGraphWork
    | FeedbackArithmeticWork
    | FeedbackTraceOutcomeCount
    | FeedbackTickCount
    deriving (Eq, Ord, Show)

-- | Common bounded-execution failure.
data FeedbackLimitError
    = FeedbackLimitExceeded !FeedbackLimitDimension !Natural !Natural
    | FeedbackRationalBitsExceeded !String !Natural !Natural
    | FeedbackNaturalOverflow !FeedbackLimitDimension
    deriving (Eq, Show)

{- | Phase assigned to each admitted rational value. Input and retained-result
observations do not consume arithmetic work; operations and branch visits do.
-}
data FeedbackPhase
    = FeedbackInputPhase
    | FeedbackMatrixPowerPhase
    | FeedbackGaussianPhase
    | FeedbackDelayedPathPhase
    | FeedbackTimedPathPhase
    | FeedbackOtherIntermediatePhase
    | FeedbackRetainedResultPhase
    deriving (Eq, Ord, Show)

{- | Exact operation-wide accounting. Maxima include values that are later
discarded. The overall maximum is the maximum of all phase maxima.
-}
data FeedbackAccounting = FeedbackAccounting
    { feedbackArithmeticWork :: !Natural
    , feedbackMaximumInputBits :: !Natural
    , feedbackMaximumMatrixPowerBits :: !Natural
    , feedbackMaximumGaussianIntermediateBits :: !Natural
    , feedbackMaximumDelayedPathIntermediateBits :: !Natural
    , feedbackMaximumTimedPathIntermediateBits :: !Natural
    , feedbackMaximumOtherIntermediateBits :: !Natural
    , feedbackMaximumRetainedResultBits :: !Natural
    , feedbackMaximumObservedBits :: !Natural
    }
    deriving (Eq, Show)

emptyFeedbackAccounting :: FeedbackAccounting
emptyFeedbackAccounting = FeedbackAccounting 0 0 0 0 0 0 0 0 0

{- | Private strict accounting computation. A failed computation returns
neither its value nor its partial accounting state.
-}
newtype FeedbackMeter error value = FeedbackMeter
    { unFeedbackMeter ::
        FeedbackLimits ->
        (FeedbackLimitError -> error) ->
        FeedbackAccounting ->
        Either error (value, FeedbackAccounting)
    }

instance Functor (FeedbackMeter error) where
    fmap function computation = FeedbackMeter $ \limits mapError accounting -> do
        (value, next) <- unFeedbackMeter computation limits mapError accounting
        Right (function value, next)

instance Applicative (FeedbackMeter error) where
    pure value = FeedbackMeter $ \_ _ accounting -> Right (value, accounting)
    function <*> argument = FeedbackMeter $ \limits mapError accounting -> do
        (apply, afterFunction) <- unFeedbackMeter function limits mapError accounting
        (value, afterArgument) <- unFeedbackMeter argument limits mapError afterFunction
        Right (apply value, afterArgument)

instance Monad (FeedbackMeter error) where
    computation >>= continue = FeedbackMeter $ \limits mapError accounting -> do
        (value, next) <- unFeedbackMeter computation limits mapError accounting
        unFeedbackMeter (continue value) limits mapError next

-- | Execute one atomic accounting computation.
runFeedbackMeter ::
    FeedbackLimits ->
    (FeedbackLimitError -> error) ->
    FeedbackMeter error value ->
    Either error (value, FeedbackAccounting)
runFeedbackMeter limits mapError computation =
    unFeedbackMeter computation limits mapError emptyFeedbackAccounting

-- | Stop an accounting computation with a semantic error.
throwFeedback :: error -> FeedbackMeter error value
throwFeedback failure = FeedbackMeter $ \_ _ _ -> Left failure

{- | Charge deterministic traversal work before visiting a branch or
descending recursively.
-}
chargeFeedbackWork :: Natural -> FeedbackMeter error ()
chargeFeedbackWork amount = FeedbackMeter $ \limits mapError accounting -> do
    required <- either (Left . mapError) Right (checkedSum FeedbackArithmeticWork [feedbackArithmeticWork accounting, amount])
    if required <= maximumFeedbackArithmeticWork limits
        then Right ((), accounting{feedbackArithmeticWork = required})
        else Left (mapError (FeedbackLimitExceeded FeedbackArithmeticWork (maximumFeedbackArithmeticWork limits) required))

{- | Observe a rational in one phase. Operations must charge work separately;
input and retained-result observations intentionally do not.
-}
recordFeedbackRational :: FeedbackPhase -> String -> Rational -> FeedbackMeter error Rational
recordFeedbackRational phase label value = FeedbackMeter $ \limits mapError accounting ->
    let bits = rationalBits value
        next = updateMaximum phase bits accounting
     in if bits <= maximumFeedbackRationalBits limits
            then Right (value, next)
            else Left (mapError (FeedbackRationalBitsExceeded label (maximumFeedbackRationalBits limits) bits))

feedbackAdd :: FeedbackPhase -> String -> Rational -> Rational -> FeedbackMeter error Rational
feedbackAdd phase label left right = do
    chargeFeedbackWork 1
    recordFeedbackRational phase label (left + right)

feedbackSubtract :: FeedbackPhase -> String -> Rational -> Rational -> FeedbackMeter error Rational
feedbackSubtract phase label left right = do
    chargeFeedbackWork 1
    recordFeedbackRational phase label (left - right)

feedbackMultiply :: FeedbackPhase -> String -> Rational -> Rational -> FeedbackMeter error Rational
feedbackMultiply phase label left right = do
    chargeFeedbackWork 1
    recordFeedbackRational phase label (left * right)

feedbackDivide :: FeedbackPhase -> String -> Rational -> Rational -> FeedbackMeter error Rational
feedbackDivide phase label numeratorValue denominatorValue = do
    -- The pivot is discarded even when the quotient later simplifies.
    _ <- recordFeedbackRational phase (label ++ " pivot") denominatorValue
    chargeFeedbackWork 1
    recordFeedbackRational phase label (numeratorValue / denominatorValue)

feedbackPower :: FeedbackPhase -> String -> Rational -> Natural -> FeedbackMeter error Rational
feedbackPower phase label base = go 1
  where
    go value 0 = recordFeedbackRational phase label value
    go value remaining = do
        next <- feedbackMultiply phase label value base
        go next (remaining - 1)

updateMaximum :: FeedbackPhase -> Natural -> FeedbackAccounting -> FeedbackAccounting
updateMaximum phase bits accounting =
    let withPhase = case phase of
            FeedbackInputPhase -> accounting{feedbackMaximumInputBits = max bits (feedbackMaximumInputBits accounting)}
            FeedbackMatrixPowerPhase -> accounting{feedbackMaximumMatrixPowerBits = max bits (feedbackMaximumMatrixPowerBits accounting)}
            FeedbackGaussianPhase -> accounting{feedbackMaximumGaussianIntermediateBits = max bits (feedbackMaximumGaussianIntermediateBits accounting)}
            FeedbackDelayedPathPhase -> accounting{feedbackMaximumDelayedPathIntermediateBits = max bits (feedbackMaximumDelayedPathIntermediateBits accounting)}
            FeedbackTimedPathPhase -> accounting{feedbackMaximumTimedPathIntermediateBits = max bits (feedbackMaximumTimedPathIntermediateBits accounting)}
            FeedbackOtherIntermediatePhase -> accounting{feedbackMaximumOtherIntermediateBits = max bits (feedbackMaximumOtherIntermediateBits accounting)}
            FeedbackRetainedResultPhase -> accounting{feedbackMaximumRetainedResultBits = max bits (feedbackMaximumRetainedResultBits accounting)}
     in withPhase{feedbackMaximumObservedBits = max bits (feedbackMaximumObservedBits withPhase)}

-- | An owner-labelled loop layout. The constructor is not public.
type role LoopLayout nominal nominal

data LoopLayout owner loop where
    UnsafeLoopLayout :: (Eq loop) => !owner -> !(FiniteSet loop) -> LoopLayout owner loop

-- | Attach a semantic owner to a duplicate-free loop layout.
loopLayout :: (Eq loop) => owner -> FiniteSet loop -> LoopLayout owner loop
loopLayout = UnsafeLoopLayout

checkLimit :: FeedbackLimitDimension -> Natural -> Natural -> Either FeedbackLimitError ()
checkLimit dimension limit required
    | required <= limit = Right ()
    | otherwise = Left (FeedbackLimitExceeded dimension limit required)

checkedProduct :: FeedbackLimitDimension -> [Natural] -> Either FeedbackLimitError Natural
checkedProduct dimension = foldl' step (Right 1)
  where
    step failure@(Left _) _ = failure
    step (Right left) right
        | left == 0 || right == 0 = Right 0
        | left > maxBoundNatural `div` right = Left (FeedbackNaturalOverflow dimension)
        | otherwise = Right (left * right)

checkedSum :: FeedbackLimitDimension -> [Natural] -> Either FeedbackLimitError Natural
checkedSum dimension = foldl' step (Right 0)
  where
    step failure@(Left _) _ = failure
    step (Right left) right
        | left > maxBoundNatural - right = Left (FeedbackNaturalOverflow dimension)
        | otherwise = Right (left + right)

checkedPower :: FeedbackLimitDimension -> Natural -> Natural -> Either FeedbackLimitError Natural
checkedPower dimension base = go 1
  where
    go accumulator 0 = Right accumulator
    go accumulator remaining = do
        next <- checkedProduct dimension [accumulator, base]
        go next (remaining - 1)

-- Natural has no finite max. Use the machine-sized bound because all current
-- represented layouts are backed by lists and Int cardinalities.
maxBoundNatural :: Natural
maxBoundNatural = fromIntegral (maxBound :: Int)

rationalBits :: Rational -> Natural
rationalBits value = max (integerBits (abs (numerator value))) (integerBits (denominator value))

integerBits :: Integer -> Natural
integerBits 0 = 1
integerBits value = go 0 value
  where
    go count 0 = count
    go count remaining = go (count + 1) (remaining `shiftR` 1)

checkRational :: Natural -> String -> Rational -> Either FeedbackLimitError Rational
checkRational limit phase value
    | bits <= limit = Right value
    | otherwise = Left (FeedbackRationalBitsExceeded phase limit bits)
  where
    bits = rationalBits value

checkedRationalPower :: Natural -> String -> Rational -> Natural -> Either FeedbackLimitError Rational
checkedRationalPower bitLimit phase base = go 1
  where
    go value 0 = Right value
    go value remaining = do
        next <- checkRational bitLimit phase (value * base)
        go next (remaining - 1)

sumFiniteSet :: FiniteSet left -> FiniteSet right -> FiniteSet (Either left right)
sumFiniteSet (UnsafeFiniteSet left) (UnsafeFiniteSet right) =
    UnsafeFiniteSet (map Left left ++ map Right right)

productFiniteSet :: FiniteSet left -> FiniteSet right -> FiniteSet (left, right)
productFiniteSet (UnsafeFiniteSet left) (UnsafeFiniteSet right) =
    UnsafeFiniteSet [(leftValue, rightValue) | leftValue <- left, rightValue <- right]

matrixRationalEntry :: Matrix NonNegativeRational source target -> source -> target -> Rational
matrixRationalEntry matrix source target =
    maybe 0 getNonNegativeRational (matrixEntry matrix source target)

-- | The least positive @k@ with @D^k = 0@, if it exists within the finite dimension.
nilpotenceIndex :: Natural -> [[Rational]] -> Either FeedbackLimitError (Maybe Natural)
nilpotenceIndex bitLimit rows
    | null rows = Right (Just 0)
    | otherwise = go 1 rows
  where
    dimension = fromIntegral (length rows)
    go currentPower power
        | all (all (== 0)) power = Right (Just currentPower)
        | currentPower >= dimension = Right Nothing
        | otherwise = do
            next <- matrixMultiply bitLimit "nilpotence power" power rows
            go (currentPower + 1) next

matrixMultiply :: Natural -> String -> [[Rational]] -> [[Rational]] -> Either FeedbackLimitError [[Rational]]
matrixMultiply bitLimit phase left right =
    traverse (\row -> traverse (\column -> entry (row, column)) (columns right)) left
  where
    entry (row, column) = foldProducts (zip row column)
    foldProducts = foldl' addTerm (Right 0)
    addTerm failure@(Left _) _ = failure
    addTerm (Right total) (x, y) = do
        productValue <- checkRational bitLimit phase (x * y)
        checkRational bitLimit phase (total + productValue)
    columns [] = []
    columns matrixRows@(firstRow : _) = [map (!! column) matrixRows | column <- [0 .. length firstRow - 1]]

matrixPower :: Natural -> String -> Natural -> [[Rational]] -> Either FeedbackLimitError [[Rational]]
matrixPower bitLimit phase powerValue rows
    | powerValue == 0 = Right (identity (length rows))
    | otherwise = go 1 rows
  where
    go current power
        | current == powerValue = Right power
        | otherwise = matrixMultiply bitLimit phase power rows >>= go (current + 1)
    identity size = [[if row == column then 1 else 0 | column <- [0 .. size - 1]] | row <- [0 .. size - 1]]

-- | Compute @D, D^2, ...@ once, stopping at the finite dimension.
matrixPowerSequence :: Natural -> String -> [[Rational]] -> Either FeedbackLimitError [(Natural, [[Rational]])]
matrixPowerSequence _ _ [] = Right [(0, [])]
matrixPowerSequence bitLimit phase rows = go 1 rows [(1, rows)]
  where
    dimension = fromIntegral (length rows)
    go current power reversed
        | current >= dimension = Right (reverse reversed)
        | otherwise = do
            next <- matrixMultiply bitLimit phase power rows
            go (current + 1) next ((current + 1, next) : reversed)

{- | Account every matrix-power product and sum, including values discarded
after the properness witness is selected.
-}
matrixPowerSequenceAccounted :: [[Rational]] -> FeedbackMeter error [(Natural, [[Rational]])]
matrixPowerSequenceAccounted [] = pure [(0, [])]
matrixPowerSequenceAccounted rows = do
    mapM_ (mapM_ (recordFeedbackRational FeedbackMatrixPowerPhase "matrix power input")) rows
    go 1 rows [(1, rows)]
  where
    dimension = fromIntegral (length rows)
    go current power reversed
        | current >= dimension = pure (reverse reversed)
        | otherwise = do
            next <- matrixMultiplyAccounted power rows
            go (current + 1) next ((current + 1, next) : reversed)
    matrixMultiplyAccounted left right =
        traverse (\row -> traverse (entry row) (columns right)) left
    entry row column = foldlM addTerm 0 (zip row column)
    addTerm total (left, right) = do
        productValue <- feedbackMultiply FeedbackMatrixPowerPhase "matrix power product" left right
        feedbackAdd FeedbackMatrixPowerPhase "matrix power sum" total productValue
    columns [] = []
    columns matrixRows@(firstRow : _) = [map (!! column) matrixRows | column <- [0 .. length firstRow - 1]]

maximumRowMass :: [[Rational]] -> Rational
maximumRowMass [] = 0
maximumRowMass rows = maximum (map sum rows)

maximumRowMassChecked :: Natural -> String -> [[Rational]] -> Either FeedbackLimitError Rational
maximumRowMassChecked _ _ [] = Right 0
maximumRowMassChecked bitLimit phase rows = maximum <$> traverse rowMass rows
  where
    rowMass = foldl' add (Right 0)
    add failure@(Left _) _ = failure
    add (Right total) value = checkRational bitLimit phase (total + value)

maximumRowMassAccounted :: FeedbackPhase -> String -> [[Rational]] -> FeedbackMeter error Rational
maximumRowMassAccounted _ _ [] = pure 0
maximumRowMassAccounted phase label rows = maximum <$> traverse rowMass rows
  where
    rowMass = foldlM (feedbackAdd phase label) 0

foldlM :: (accumulator -> value -> FeedbackMeter error accumulator) -> accumulator -> [value] -> FeedbackMeter error accumulator
foldlM _ initial [] = pure initial
foldlM step initial (value : remaining) = do
    next <- step initial value
    foldlM step next remaining
