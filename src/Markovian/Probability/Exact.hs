{-# LANGUAGE TupleSections #-}

{- | Bounded exact finite probabilities for structural interpreters and laws.

This module uses 'Rational'. It does not share representation or equality with
floating probability values. Checked sequencing is deliberately not a
'Monad': admission and resource reports can depend on association.
-}
module Markovian.Probability.Exact (
    ExactProb,
    ExactProbabilityError (..),
    mkExactProb,
    exactProbability,
    ExactWeight,
    ExactWeightError (..),
    mkExactWeight,
    exactWeight,
    ExactFiniteDist,
    ExactDistributionError (..),
    ExactDistributionLimits,
    ExactDistributionLimitError (..),
    ExactDistributionReport (..),
    maximumExactSupportEntries,
    exactDistributionLimits,
    defaultExactDistributionLimits,
    exactFiniteDistChecked,
    exactFiniteDist,
    exactDirac,
    ExactBindLimits,
    ExactBindLimitError (..),
    exactBindLimits,
    defaultExactBindLimits,
    ExactBindError (..),
    ExactBindReport (..),
    bindExactFiniteDistChecked,
    exactOutcomes,
) where

import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio (denominator, numerator)
import Numeric.Natural (Natural)

-- Preserve tuple-pattern strictness at validated representation boundaries.
{-# ANN module ("HLint: ignore Use first" :: String) #-}

-- | An exact probability in the closed interval @[0, 1]@.
newtype ExactProb = ExactProb Rational
    deriving (Eq, Ord, Show)

-- | The supplied rational is outside the closed unit interval.
newtype ExactProbabilityError = ExactProbabilityOutOfRange Rational
    deriving (Eq, Show)

-- | Validate an exact probability.
mkExactProb :: Rational -> Either ExactProbabilityError ExactProb
mkExactProb value
    | value < 0 || value > 1 = Left (ExactProbabilityOutOfRange value)
    | otherwise = Right (ExactProb value)

-- | Read the validated exact probability.
exactProbability :: ExactProb -> Rational
exactProbability (ExactProb value) = value

-- | An exact, nonnegative, unnormalized weight.
newtype ExactWeight = ExactWeight Rational
    deriving (Eq, Ord, Show)

-- | The supplied rational is negative.
newtype ExactWeightError = NegativeExactWeight Rational
    deriving (Eq, Show)

-- | Validate an exact weight.
mkExactWeight :: Rational -> Either ExactWeightError ExactWeight
mkExactWeight value
    | value < 0 = Left (NegativeExactWeight value)
    | otherwise = Right (ExactWeight value)

-- | Read the validated exact weight.
exactWeight :: ExactWeight -> Rational
exactWeight (ExactWeight value) = value

{- | A nonempty finite distribution with exact normalized probabilities.

Distinct labeled entries remain distinct. Zero-weight entries are removed by
the checked constructor.
-}
newtype ExactFiniteDist a = UnsafeExactFiniteDist (NonEmpty (a, ExactProb))
    deriving (Eq, Show)

instance Functor ExactFiniteDist where
    fmap function (UnsafeExactFiniteDist entries) =
        UnsafeExactFiniteDist (fmap (\(value, mass) -> (function value, mass)) entries)

instance Foldable ExactFiniteDist where
    foldMap function (UnsafeExactFiniteDist entries) =
        foldMap (function . fst) entries

instance Traversable ExactFiniteDist where
    traverse function (UnsafeExactFiniteDist entries) =
        UnsafeExactFiniteDist
            <$> traverse (\(value, mass) -> (,mass) <$> function value) entries

-- | Errors returned by 'exactFiniteDist'.
data ExactDistributionError
    = EmptyExactSupport
    | InvalidExactWeight !Integer !ExactWeightError
    | ExactSupportLimitExceeded !Natural
    | ExactDistributionWorkLimitExceeded !Natural
    | ExactDistributionNumeratorBitsExceeded !Natural
    | ExactDistributionDenominatorBitsExceeded !Natural
    | ZeroExactTotalWeight
    deriving (Eq, Show)

-- | Maximum raw or admitted result support for exact distributions.
maximumExactSupportEntries :: Natural
maximumExactSupportEntries = 4096

-- | Operation-wide limits for validation and normalization.
data ExactDistributionLimits = ExactDistributionLimits
    { distributionSupportLimit :: !Natural
    , distributionWorkLimit :: !Natural
    , distributionNumeratorBitLimit :: !Natural
    , distributionDenominatorBitLimit :: !Natural
    }
    deriving (Eq, Show)

-- | Invalid exact-constructor limits.
data ExactDistributionLimitError
    = ExactDistributionZeroSupportLimit
    | ExactDistributionSupportLimitAboveGlobalMaximum !Natural
    | ExactDistributionZeroWorkLimit
    | ExactDistributionZeroNumeratorBitLimit
    | ExactDistributionZeroDenominatorBitLimit
    deriving (Eq, Show)

-- | Deterministic, timing-free constructor receipt.
data ExactDistributionReport = ExactDistributionReport
    { exactDistributionRawEntries :: !Natural
    , exactDistributionPositiveEntries :: !Natural
    , exactDistributionWork :: !Natural
    , exactDistributionMaximumNumeratorBits :: !Natural
    , exactDistributionMaximumDenominatorBits :: !Natural
    }
    deriving (Eq, Show)

-- | Validate exact-constructor support, work, and rational-size limits.
exactDistributionLimits :: Natural -> Natural -> Natural -> Natural -> Either ExactDistributionLimitError ExactDistributionLimits
exactDistributionLimits support work numeratorBits denominatorBits
    | support == 0 = Left ExactDistributionZeroSupportLimit
    | support > maximumExactSupportEntries = Left (ExactDistributionSupportLimitAboveGlobalMaximum support)
    | work == 0 = Left ExactDistributionZeroWorkLimit
    | numeratorBits == 0 = Left ExactDistributionZeroNumeratorBitLimit
    | denominatorBits == 0 = Left ExactDistributionZeroDenominatorBitLimit
    | otherwise = Right (ExactDistributionLimits support work numeratorBits denominatorBits)

-- | Conservative package-wide limits used by the compatibility constructor.
defaultExactDistributionLimits :: ExactDistributionLimits
defaultExactDistributionLimits = ExactDistributionLimits maximumExactSupportEntries 16384 4096 4096

{- | Construct an exact distribution under one operation-wide budget.

The raw spine is stopped at the support limit plus one before any weight is
validated. Work charges validation, positive-entry selection, total addition,
and normalization. Every supplied, intermediate-total, and normalized rational
is checked. Failure returns neither a distribution nor a report.
-}
exactFiniteDistChecked :: ExactDistributionLimits -> [(a, Rational)] -> Either ExactDistributionError (ExactFiniteDist a, ExactDistributionReport)
exactFiniteDistChecked limits rawEntries = do
    entries <- boundedSupport (distributionSupportLimit limits) rawEntries
    when (null entries) (Left EmptyExactSupport)
    (validated, afterValidation) <- validateAll 0 initial entries
    (positive, afterSelection) <- selectPositive afterValidation validated
    when (null positive) (Left ZeroExactTotalWeight)
    (total, afterTotal) <- sumWeights afterSelection positive
    (normalized, final) <- normalizeAll afterTotal total positive
    case NonEmpty.nonEmpty normalized of
        Nothing -> Left ZeroExactTotalWeight
        Just admitted ->
            Right
                ( UnsafeExactFiniteDist admitted
                , ExactDistributionReport
                    { exactDistributionRawEntries = fromIntegral (length entries)
                    , exactDistributionPositiveEntries = fromIntegral (length positive)
                    , exactDistributionWork = constructorWork final
                    , exactDistributionMaximumNumeratorBits = constructorNumeratorBits final
                    , exactDistributionMaximumDenominatorBits = constructorDenominatorBits final
                    }
                )
  where
    initial = ConstructorState 0 0 0
    charge state =
        let observed = constructorWork state + 1
         in if observed > distributionWorkLimit limits
                then Left (ExactDistributionWorkLimitExceeded observed)
                else Right state{constructorWork = observed}
    checkRational state value = do
        let numeratorBits = integerBits (numerator value)
            denominatorBits = integerBits (denominator value)
        when (numeratorBits > distributionNumeratorBitLimit limits) (Left (ExactDistributionNumeratorBitsExceeded numeratorBits))
        when (denominatorBits > distributionDenominatorBitLimit limits) (Left (ExactDistributionDenominatorBitsExceeded denominatorBits))
        Right state{constructorNumeratorBits = max (constructorNumeratorBits state) numeratorBits, constructorDenominatorBits = max (constructorDenominatorBits state) denominatorBits}
    validateAll _ state [] = Right ([], state)
    validateAll index state ((value, rawWeight) : remaining) = do
        charged <- charge state
        checked <- checkRational charged rawWeight
        weight <- case mkExactWeight rawWeight of
            Left problem -> Left (InvalidExactWeight index problem)
            Right valid -> Right valid
        (rest, final) <- validateAll (index + 1) checked remaining
        Right ((value, weight) : rest, final)
    selectPositive state [] = Right ([], state)
    selectPositive state (entry@(_, weight) : remaining) = do
        charged <- charge state
        (rest, final) <- selectPositive charged remaining
        Right (if exactWeight weight > 0 then entry : rest else rest, final)
    sumWeights state = go state 0
      where
        go current total [] = Right (total, current)
        go current total ((_, weight) : remaining) = do
            charged <- charge current
            let next = total + exactWeight weight
            checked <- checkRational charged next
            go checked next remaining
    normalizeAll state _ [] = Right ([], state)
    normalizeAll state total ((value, weight) : remaining) = do
        charged <- charge state
        let normalized = exactWeight weight / total
        checked <- checkRational charged normalized
        (rest, final) <- normalizeAll checked total remaining
        Right ((value, ExactProb normalized) : rest, final)

-- | Bounded compatibility constructor using 'defaultExactDistributionLimits'.
exactFiniteDist :: [(a, Rational)] -> Either ExactDistributionError (ExactFiniteDist a)
exactFiniteDist = fmap fst . exactFiniteDistChecked defaultExactDistributionLimits

data ConstructorState = ConstructorState
    { constructorWork :: !Natural
    , constructorNumeratorBits :: !Natural
    , constructorDenominatorBits :: !Natural
    }

boundedSupport :: Natural -> [a] -> Either ExactDistributionError [a]
boundedSupport limit = go limit []
  where
    go _ reversed [] = Right (reverse reversed)
    go 0 _ (_ : _) = Left (ExactSupportLimitExceeded limit)
    go remaining reversed (entry : entries) =
        go (remaining - 1) (entry : reversed) entries

-- | Construct an exact distribution with one certain outcome.
exactDirac :: a -> ExactFiniteDist a
exactDirac value = UnsafeExactFiniteDist ((value, ExactProb 1) NonEmpty.:| [])

-- | Resource limits for one checked exact bind.
data ExactBindLimits = ExactBindLimits
    { limitResultEntries :: !Natural
    , limitWork :: !Natural
    , limitNumeratorBits :: !Natural
    , limitDenominatorBits :: !Natural
    }
    deriving (Eq, Show)

-- | Invalid checked-bind limit configuration.
data ExactBindLimitError
    = ExactBindZeroResultLimit
    | ExactBindResultLimitAboveGlobalMaximum !Natural
    | ExactBindZeroWorkLimit
    | ExactBindZeroNumeratorBitLimit
    | ExactBindZeroDenominatorBitLimit
    deriving (Eq, Show)

-- | Validate limits for one exact bind operation.
exactBindLimits :: Natural -> Natural -> Natural -> Natural -> Either ExactBindLimitError ExactBindLimits
exactBindLimits resultEntries work numeratorBits denominatorBits
    | resultEntries == 0 = Left ExactBindZeroResultLimit
    | resultEntries > maximumExactSupportEntries = Left (ExactBindResultLimitAboveGlobalMaximum resultEntries)
    | work == 0 = Left ExactBindZeroWorkLimit
    | numeratorBits == 0 = Left ExactBindZeroNumeratorBitLimit
    | denominatorBits == 0 = Left ExactBindZeroDenominatorBitLimit
    | otherwise = Right (ExactBindLimits resultEntries work numeratorBits denominatorBits)

-- | Conservative package-wide sequencing limits for compatibility APIs.
defaultExactBindLimits :: ExactBindLimits
defaultExactBindLimits = ExactBindLimits maximumExactSupportEntries 16384 4096 4096

-- | Atomic failure from checked exact sequencing.
data ExactBindError continuationError
    = ExactBindContinuationFailure !Natural !continuationError
    | ExactBindResultLimitExceeded !Natural
    | ExactBindWorkLimitExceeded !Natural
    | ExactBindNumeratorBitsExceeded !Natural
    | ExactBindDenominatorBitsExceeded !Natural
    deriving (Eq, Show)

-- | Deterministic resource receipt for an admitted exact bind.
data ExactBindReport = ExactBindReport
    { exactBindOuterEntries :: !Natural
    , exactBindContinuationCalls :: !Natural
    , exactBindResultEntries :: !Natural
    , exactBindMassMultiplications :: !Natural
    , exactBindMaximumNumeratorBits :: !Natural
    , exactBindMaximumDenominatorBits :: !Natural
    }
    deriving (Eq, Show)

{- | Internal successful accumulator. Work includes outer and inner support
traversal, continuation calls, and mass multiplications.
-}
data BindState value = BindState
    { bindReversedEntries :: ![(value, ExactProb)]
    , bindOuterCount :: !Natural
    , bindContinuationCount :: !Natural
    , bindResultCount :: !Natural
    , bindMultiplicationCount :: !Natural
    , bindWorkCount :: !Natural
    , bindNumeratorBits :: !Natural
    , bindDenominatorBits :: !Natural
    }

{- | Sequence exact choices under one operation-wide budget.

Continuation failure and every resource failure are atomic: no partial
result or report is returned. The result spine is stopped when the next entry
would exceed the configured support limit, so an oversized product is never
fully constructed.
-}
bindExactFiniteDistChecked ::
    ExactBindLimits ->
    ExactFiniteDist a ->
    (a -> Either continuationError (ExactFiniteDist b)) ->
    Either (ExactBindError continuationError) (ExactFiniteDist b, ExactBindReport)
bindExactFiniteDistChecked limits (UnsafeExactFiniteDist outerEntries) continuation = do
    final <- foldOuter emptyState (NonEmpty.toList outerEntries)
    case NonEmpty.nonEmpty (reverse (bindReversedEntries final)) of
        Nothing -> error "checked exact bind: impossible empty result"
        Just entries ->
            Right
                ( UnsafeExactFiniteDist entries
                , ExactBindReport
                    { exactBindOuterEntries = bindOuterCount final
                    , exactBindContinuationCalls = bindContinuationCount final
                    , exactBindResultEntries = bindResultCount final
                    , exactBindMassMultiplications = bindMultiplicationCount final
                    , exactBindMaximumNumeratorBits = bindNumeratorBits final
                    , exactBindMaximumDenominatorBits = bindDenominatorBits final
                    }
                )
  where
    emptyState = BindState [] 0 0 0 0 0 0 0

    charge amount state =
        let observed = bindWorkCount state + amount
         in if observed > limitWork limits
                then Left (ExactBindWorkLimitExceeded observed)
                else Right state{bindWorkCount = observed}

    foldOuter state [] = Right state
    foldOuter state ((value, ExactProb outerMass) : remaining) = do
        traversed <- charge 1 state
        called <- charge 1 traversed
        next <-
            case continuation value of
                Left problem -> Left (ExactBindContinuationFailure (bindContinuationCount called + 1) problem)
                Right distribution -> Right distribution
        let afterCall =
                called
                    { bindOuterCount = bindOuterCount called + 1
                    , bindContinuationCount = bindContinuationCount called + 1
                    }
        expanded <- foldInner outerMass afterCall (NonEmpty.toList (exactOutcomes next))
        foldOuter expanded remaining

    foldInner _ state [] = Right state
    foldInner outerMass state ((nextValue, ExactProb innerMass) : remaining) = do
        traversed <- charge 1 state
        let nextResultCount = bindResultCount traversed + 1
        when (nextResultCount > limitResultEntries limits) $
            Left (ExactBindResultLimitExceeded nextResultCount)
        multiplied <- charge 1 traversed
        let productMass = outerMass * innerMass
            productNumeratorBits = integerBits (numerator productMass)
            productDenominatorBits = integerBits (denominator productMass)
        when (productNumeratorBits > limitNumeratorBits limits) $
            Left (ExactBindNumeratorBitsExceeded productNumeratorBits)
        when (productDenominatorBits > limitDenominatorBits limits) $
            Left (ExactBindDenominatorBitsExceeded productDenominatorBits)
        let nextState =
                multiplied
                    { bindReversedEntries = (nextValue, ExactProb productMass) : bindReversedEntries multiplied
                    , bindResultCount = nextResultCount
                    , bindMultiplicationCount = bindMultiplicationCount multiplied + 1
                    , bindNumeratorBits = max (bindNumeratorBits multiplied) productNumeratorBits
                    , bindDenominatorBits = max (bindDenominatorBits multiplied) productDenominatorBits
                    }
        foldInner outerMass nextState remaining

integerBits :: Integer -> Natural
integerBits value = go 0 (abs value)
  where
    go bits 0 = max 1 bits
    go bits remaining = go (bits + 1) (remaining `quot` 2)

-- | Read all positive-mass exact outcomes in support order.
exactOutcomes :: ExactFiniteDist a -> NonEmpty (a, ExactProb)
exactOutcomes (UnsafeExactFiniteDist entries) = entries
