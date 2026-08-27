{- | Normalized exact nonnegative finite matrices.

The constructor is exposed only for 'NonNegativeRational', the currently
implemented exact nonnegative semifield. Transpose, compact structure, trace,
and raw addition are intentionally absent because they do not generally
preserve row normalization.
-}
module Markovian.Category.Matrix.Stochastic (
    StochasticMatrix,
    StochasticMatrixError (..),
    stochasticMatrix,
    stochasticFromFunction,
    forgetStochastic,
    stochasticSource,
    stochasticTarget,
    stochasticEquivalent,
    identityStochastic,
    composeStochastic,
    tensorStochastic,
    copyStochastic,
    discardStochastic,
) where

import Data.Foldable (foldl')
import Markovian.Algebra.NonNegativeRational (NonNegativeRational)
import Markovian.Algebra.Semiring
import Markovian.Category.Finite.Set
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Stochastic.Internal (StochasticMatrix (..))

-- | A row whose exact mass does not equal one.
data StochasticMatrixError scalar = StochasticRowNotNormalized !Int !scalar
    deriving (Eq, Show)

-- | Validate a nonnegative rational matrix row by row.
stochasticMatrix ::
    Matrix NonNegativeRational source target ->
    Either (StochasticMatrixError NonNegativeRational) (StochasticMatrix NonNegativeRational source target)
stochasticMatrix matrix =
    case firstInvalidRow 0 (matrixRows matrix) of
        Nothing -> Right (UnsafeStochasticMatrix matrix)
        Just failure -> Left failure
  where
    firstInvalidRow _ [] = Nothing
    firstInvalidRow rowIndex (row : remaining) =
        let total = foldl' plus zero row
         in if total == one
                then firstInvalidRow (rowIndex + 1) remaining
                else Just (StochasticRowNotNormalized rowIndex total)

-- | Construct and validate a nonnegative rational kernel from a total function.
stochasticFromFunction ::
    FiniteSet source ->
    FiniteSet target ->
    (source -> target -> NonNegativeRational) ->
    Either (StochasticMatrixError NonNegativeRational) (StochasticMatrix NonNegativeRational source target)
stochasticFromFunction source target = stochasticMatrix . matrixFromFunction source target

-- | Forget normalization while preserving entries and layout.
forgetStochastic :: StochasticMatrix scalar source target -> Matrix scalar source target
forgetStochastic (UnsafeStochasticMatrix matrix) = matrix

-- | Read the source witness.
stochasticSource :: StochasticMatrix scalar source target -> FiniteSet source
stochasticSource = matrixSource . forgetStochastic

-- | Read the target witness.
stochasticTarget :: StochasticMatrix scalar source target -> FiniteSet target
stochasticTarget = matrixTarget . forgetStochastic

-- | Extensional equality of stochastic matrices.
stochasticEquivalent ::
    (Eq scalar) =>
    StochasticMatrix scalar source target ->
    StochasticMatrix scalar source target ->
    Bool
stochasticEquivalent left right =
    matrixEquivalent (forgetStochastic left) (forgetStochastic right)

-- | Stochastic identity.
identityStochastic ::
    (ExactNonNegativeSemifield scalar) =>
    FiniteSet value ->
    StochasticMatrix scalar value value
identityStochastic = UnsafeStochasticMatrix . identityMatrix

-- | Stochastic composition, with checked middle-object alignment.
composeStochastic ::
    (ExactNonNegativeSemifield scalar) =>
    StochasticMatrix scalar source middle ->
    StochasticMatrix scalar middle target ->
    Either MatrixError (StochasticMatrix scalar source target)
composeStochastic left right =
    UnsafeStochasticMatrix
        <$> composeMatrix (forgetStochastic left) (forgetStochastic right)

-- | Independent stochastic tensor.
tensorStochastic ::
    (ExactNonNegativeSemifield scalar) =>
    StochasticMatrix scalar leftSource leftTarget ->
    StochasticMatrix scalar rightSource rightTarget ->
    StochasticMatrix scalar (leftSource, rightSource) (leftTarget, rightTarget)
tensorStochastic left right =
    UnsafeStochasticMatrix (tensorMatrix (forgetStochastic left) (forgetStochastic right))

-- | Deterministic diagonal copy into the full tensor square.
copyStochastic ::
    (ExactNonNegativeSemifield scalar) =>
    FiniteSet value ->
    StochasticMatrix scalar value (value, value)
copyStochastic object@(UnsafeFiniteSet _) =
    UnsafeStochasticMatrix $
        matrixFromFunction object (productSet object object) $ \sourceValue (left, right) ->
            if sourceValue == left && sourceValue == right then one else zero

-- | Natural stochastic discard into the singleton object.
discardStochastic ::
    (ExactNonNegativeSemifield scalar) =>
    FiniteSet value ->
    StochasticMatrix scalar value ()
discardStochastic object =
    UnsafeStochasticMatrix (matrixFromFunction object unitSet (\_ () -> one))

productSet :: FiniteSet left -> FiniteSet right -> FiniteSet (left, right)
productSet (UnsafeFiniteSet left) (UnsafeFiniteSet right) =
    UnsafeFiniteSet
        [ (leftValue, rightValue)
        | leftValue <- left
        , rightValue <- right
        ]

unitSet :: FiniteSet ()
unitSet = UnsafeFiniteSet [()]
