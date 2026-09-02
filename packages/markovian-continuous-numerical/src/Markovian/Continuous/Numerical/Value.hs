-- | Checked finite floating values and explicit rational conversion.
module Markovian.Continuous.Numerical.Value (
    FiniteDouble,
    finiteDouble,
    finiteValue,
    PositiveFinite,
    positiveFinite,
    positiveValue,
    CompactDoubleInterval,
    compactDoubleInterval,
    doubleIntervalBounds,
    RoundingReport,
    approximateRational,
    roundedValue,
    exactRoundingDifference,
    NumericalValueError (..),
) where

-- | A nonfinite, nonpositive, interval, or conversion failure.
data NumericalValueError
    = NonFiniteValue Double
    | NonPositiveValue Double
    | InvalidDoubleInterval Double Double
    | RationalOutsideFiniteDouble
    deriving stock (Eq, Show)

-- | An opaque finite floating value.
newtype FiniteDouble = FiniteDouble Double
    deriving stock (Eq, Ord, Show)

-- | An opaque positive finite floating value.
newtype PositiveFinite = PositiveFinite Double
    deriving stock (Eq, Ord, Show)

-- | A finite interval with strictly ordered endpoints.
data CompactDoubleInterval = CompactDoubleInterval FiniteDouble FiniteDouble
    deriving stock (Eq, Show)

-- | A rounded value and its exact rational difference.
data RoundingReport = RoundingReport Double Rational
    deriving stock (Eq, Show)

-- | Validate and canonicalize a finite floating value.
finiteDouble :: Double -> Either NumericalValueError FiniteDouble
finiteDouble value
    | isNaN value || isInfinite value = Left (NonFiniteValue value)
    | otherwise = Right (FiniteDouble (if value == 0 then 0 else value))

-- | Get the validated floating value.
finiteValue :: FiniteDouble -> Double
finiteValue (FiniteDouble value) = value

-- | Validate a positive finite floating value.
positiveFinite :: Double -> Either NumericalValueError PositiveFinite
positiveFinite value = do
    FiniteDouble checked <- finiteDouble value
    if checked > 0 then Right (PositiveFinite checked) else Left (NonPositiveValue checked)

-- | Get the positive floating value.
positiveValue :: PositiveFinite -> Double
positiveValue (PositiveFinite value) = value

-- | Validate finite strictly ordered endpoints.
compactDoubleInterval :: Double -> Double -> Either NumericalValueError CompactDoubleInterval
compactDoubleInterval lower upper = do
    checkedLower <- finiteDouble lower
    checkedUpper <- finiteDouble upper
    let width = upper - lower
    if lower < upper && not (isNaN width || isInfinite width) && width > 0
        then Right (CompactDoubleInterval checkedLower checkedUpper)
        else Left (InvalidDoubleInterval lower upper)

-- | Get the interval endpoints.
doubleIntervalBounds :: CompactDoubleInterval -> (Double, Double)
doubleIntervalBounds (CompactDoubleInterval lower upper) = (finiteValue lower, finiteValue upper)

-- | Convert a rational value and record the exact rounding difference.
approximateRational :: Rational -> Either NumericalValueError (FiniteDouble, RoundingReport)
approximateRational exact = do
    value <- finiteDouble (fromRational exact)
    let rounded = finiteValue value
    pure (value, RoundingReport rounded (exact - toRational rounded))

-- | Get the rounded floating value.
roundedValue :: RoundingReport -> Double
roundedValue (RoundingReport value _) = value

-- | Get @exact - toRational rounded@.
exactRoundingDifference :: RoundingReport -> Rational
exactRoundingDifference (RoundingReport _ difference) = difference
