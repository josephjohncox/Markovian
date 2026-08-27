{-# LANGUAGE RoleAnnotations #-}

{- | Proof-carrying deterministic finite matrices.

Only this refinement, rather than an arbitrary stochastic matrix whose current
denotation happens to be Dirac, authorizes deterministic copy-naturality
reasoning.
-}
module Markovian.Category.Matrix.Deterministic (
    DeterministicMatrix,
    DeterministicMatrixError (..),
    deterministicMatrix,
    deterministicFromFunction,
    forgetDeterministic,
    embedDeterministic,
    deterministicSource,
    deterministicTarget,
    deterministicEquivalent,
    identityDeterministic,
    composeDeterministic,
    tensorDeterministic,
) where

import Markovian.Algebra.Semiring
import Markovian.Category.Finite.Set
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Stochastic (StochasticMatrix)
import Markovian.Category.Matrix.Stochastic.Internal (StochasticMatrix (UnsafeStochasticMatrix))

-- | An opaque matrix carrying evidence that every row is exactly one-hot.
type role DeterministicMatrix nominal nominal nominal

newtype DeterministicMatrix scalar source target
    = UnsafeDeterministicMatrix (Matrix scalar source target)

-- | Deterministic validation failure.
data DeterministicMatrixError target
    = DeterministicOutputOutsideTarget !target
    | DeterministicRowNotOneHot !Int
    deriving (Eq, Show)

-- | Validate that each row is exactly one-hot.
deterministicMatrix ::
    (ExactNonNegativeSemifield scalar) =>
    Matrix scalar source target ->
    Either (DeterministicMatrixError target) (DeterministicMatrix scalar source target)
deterministicMatrix matrix =
    case firstInvalidRow 0 (matrixRows matrix) of
        Nothing -> Right (UnsafeDeterministicMatrix matrix)
        Just rowIndex -> Left (DeterministicRowNotOneHot rowIndex)
  where
    firstInvalidRow _ [] = Nothing
    firstInvalidRow rowIndex (row : remaining)
        | length (filter (== one) row) == 1 && all (\entry -> entry == one || isZero entry) row =
            firstInvalidRow (rowIndex + 1) remaining
        | otherwise = Just rowIndex

-- | Validate a represented total finite function and encode its graph.
deterministicFromFunction ::
    (ExactNonNegativeSemifield scalar) =>
    FiniteSet source ->
    FiniteSet target ->
    (source -> target) ->
    Either (DeterministicMatrixError target) (DeterministicMatrix scalar source target)
deterministicFromFunction source target@(UnsafeFiniteSet _) function =
    case firstOutside (finiteSetValues source) of
        Just value -> Left (DeterministicOutputOutsideTarget value)
        Nothing ->
            Right . UnsafeDeterministicMatrix $
                matrixFromFunction source target $ \sourceValue targetValue ->
                    if function sourceValue == targetValue then one else zero
  where
    firstOutside [] = Nothing
    firstOutside (sourceValue : remaining)
        | function sourceValue `elem` finiteSetValues target = firstOutside remaining
        | otherwise = Just (function sourceValue)

-- | Forget the one-hot proof.
forgetDeterministic :: DeterministicMatrix scalar source target -> Matrix scalar source target
forgetDeterministic (UnsafeDeterministicMatrix matrix) = matrix

-- | Total forgetful embedding into normalized stochastic matrices.
embedDeterministic ::
    DeterministicMatrix scalar source target ->
    StochasticMatrix scalar source target
embedDeterministic = UnsafeStochasticMatrix . forgetDeterministic

-- | Read the source witness.
deterministicSource :: DeterministicMatrix scalar source target -> FiniteSet source
deterministicSource = matrixSource . forgetDeterministic

-- | Read the target witness.
deterministicTarget :: DeterministicMatrix scalar source target -> FiniteSet target
deterministicTarget = matrixTarget . forgetDeterministic

-- | Extensional equality of deterministic maps.
deterministicEquivalent ::
    (Eq scalar) =>
    DeterministicMatrix scalar source target ->
    DeterministicMatrix scalar source target ->
    Bool
deterministicEquivalent left right =
    matrixEquivalent (forgetDeterministic left) (forgetDeterministic right)

-- | Deterministic identity.
identityDeterministic ::
    (ExactNonNegativeSemifield scalar) =>
    FiniteSet value ->
    DeterministicMatrix scalar value value
identityDeterministic = UnsafeDeterministicMatrix . identityMatrix

-- | Composition in the deterministic subcategory.
composeDeterministic ::
    (ExactNonNegativeSemifield scalar) =>
    DeterministicMatrix scalar source middle ->
    DeterministicMatrix scalar middle target ->
    Either MatrixError (DeterministicMatrix scalar source target)
composeDeterministic left right =
    UnsafeDeterministicMatrix
        <$> composeMatrix (forgetDeterministic left) (forgetDeterministic right)

-- | Tensor in the deterministic subcategory.
tensorDeterministic ::
    (ExactNonNegativeSemifield scalar) =>
    DeterministicMatrix scalar leftSource leftTarget ->
    DeterministicMatrix scalar rightSource rightTarget ->
    DeterministicMatrix scalar (leftSource, rightSource) (leftTarget, rightTarget)
tensorDeterministic left right =
    UnsafeDeterministicMatrix (tensorMatrix (forgetDeterministic left) (forgetDeterministic right))
