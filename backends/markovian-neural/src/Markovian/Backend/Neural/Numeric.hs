{- | Checked 'Double' arithmetic used by the neural reference algorithms.

The functions in this module reject non-finite inputs and results. They do not
claim exact arithmetic: each successful result still has ordinary IEEE-754
rounding error.
-}
module Markovian.Backend.Neural.Numeric (
    NeuralNumericError (..),
    FiniteDouble,
    finiteDouble,
    finiteDoubleValue,
    addFiniteDouble,
    subtractFiniteDouble,
    multiplyFiniteDouble,
    NumericalTolerance,
    mkNumericalTolerance,
    absoluteTolerance,
    relativeTolerance,
    approximatelyEqual,
    validateFinite,
    validateFiniteVector,
    checkedAdd,
    checkedSubtract,
    checkedMultiply,
    checkedDivide,
    checkedSum,
) where

-- | A non-finite input or arithmetic result.
data NeuralNumericError
    = NonFiniteScalar !String !Double
    | NonFiniteVectorElement !String !Int !Double
    | NonFiniteArithmeticResult !String !Double
    | InvalidAbsoluteTolerance !Double
    | InvalidRelativeTolerance !Double
    | DivisionByZero !String
    deriving (Eq, Show)

-- | An opaque finite IEEE-754 value.
newtype FiniteDouble = FiniteDouble Double
    deriving (Eq, Ord, Show)

-- | Validate and wrap one finite value.
finiteDouble :: Double -> Either NeuralNumericError FiniteDouble
finiteDouble value = do
    validateFinite "finite Double" value
    Right (FiniteDouble value)

-- | Read an opaque finite value.
finiteDoubleValue :: FiniteDouble -> Double
finiteDoubleValue (FiniteDouble value) = value

-- | Checked addition of finite values.
addFiniteDouble :: FiniteDouble -> FiniteDouble -> Either NeuralNumericError FiniteDouble
addFiniteDouble (FiniteDouble left) (FiniteDouble right) =
    FiniteDouble <$> checkedAdd "finite Double addition" left right

-- | Checked subtraction of finite values.
subtractFiniteDouble :: FiniteDouble -> FiniteDouble -> Either NeuralNumericError FiniteDouble
subtractFiniteDouble (FiniteDouble left) (FiniteDouble right) =
    FiniteDouble <$> checkedSubtract "finite Double subtraction" left right

-- | Checked multiplication of finite values.
multiplyFiniteDouble :: FiniteDouble -> FiniteDouble -> Either NeuralNumericError FiniteDouble
multiplyFiniteDouble (FiniteDouble left) (FiniteDouble right) =
    FiniteDouble <$> checkedMultiply "finite Double multiplication" left right

-- | Explicit absolute-plus-relative comparison tolerances.
data NumericalTolerance = NumericalTolerance !Double !Double
    deriving (Eq, Show)

-- | Construct finite, nonnegative absolute and relative tolerances.
mkNumericalTolerance :: Double -> Double -> Either NeuralNumericError NumericalTolerance
mkNumericalTolerance absolute relative
    | not (finite absolute) || absolute < 0 = Left (InvalidAbsoluteTolerance absolute)
    | not (finite relative) || relative < 0 = Left (InvalidRelativeTolerance relative)
    | otherwise = Right (NumericalTolerance absolute relative)

-- | Absolute part of a numerical tolerance.
absoluteTolerance :: NumericalTolerance -> Double
absoluteTolerance (NumericalTolerance value _) = value

-- | Relative part of a numerical tolerance.
relativeTolerance :: NumericalTolerance -> Double
relativeTolerance (NumericalTolerance _ value) = value

-- | Compare finite values using @|x-y| <= atol + rtol * max(|x|,|y|)@.
approximatelyEqual :: NumericalTolerance -> Double -> Double -> Either NeuralNumericError Bool
approximatelyEqual (NumericalTolerance absolute relative) left right = do
    validateFinite "approximate comparison left operand" left
    validateFinite "approximate comparison right operand" right
    difference <- checkedSubtract "approximate comparison difference" left right
    scale <- checkedMultiply "approximate comparison relative scale" relative (max (abs left) (abs right))
    threshold <- checkedAdd "approximate comparison threshold" absolute scale
    Right (abs difference <= threshold)

-- | Validate one named scalar.
validateFinite :: String -> Double -> Either NeuralNumericError ()
validateFinite label value
    | finite value = Right ()
    | otherwise = Left (NonFiniteScalar label value)

-- | Validate all elements of a named vector.
validateFiniteVector :: String -> [Double] -> Either NeuralNumericError ()
validateFiniteVector label = go 0
  where
    go _ [] = Right ()
    go index (value : remaining)
        | finite value = go (index + 1) remaining
        | otherwise = Left (NonFiniteVectorElement label index value)

-- | Checked addition.
checkedAdd :: String -> Double -> Double -> Either NeuralNumericError Double
checkedAdd label left right = checkedResult label (left + right)

-- | Checked subtraction.
checkedSubtract :: String -> Double -> Double -> Either NeuralNumericError Double
checkedSubtract label left right = checkedResult label (left - right)

-- | Checked multiplication.
checkedMultiply :: String -> Double -> Double -> Either NeuralNumericError Double
checkedMultiply label left right = checkedResult label (left * right)

-- | Checked division with an explicit zero-denominator error.
checkedDivide :: String -> Double -> Double -> Either NeuralNumericError Double
checkedDivide label _ 0 = Left (DivisionByZero label)
checkedDivide label numerator denominator = checkedResult label (numerator / denominator)

-- | Sum a finite vector while checking each intermediate result.
checkedSum :: String -> [Double] -> Either NeuralNumericError Double
checkedSum label = go 0
  where
    go total [] = Right total
    go total (value : remaining) = do
        next <- checkedAdd label total value
        go next remaining

checkedResult :: String -> Double -> Either NeuralNumericError Double
checkedResult label result
    | finite result = Right result
    | otherwise = Left (NonFiniteArithmeticResult label result)

finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)
