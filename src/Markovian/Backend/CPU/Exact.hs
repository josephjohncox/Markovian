-- | Dense rational CPU lowering for typed exact finite IR.
module Markovian.Backend.CPU.Exact (
    DenseExactKernel,
    denseExactSource,
    denseExactTarget,
    denseExactShape,
    denseExactRows,
    DenseExactLoweringError (..),
    lowerExactIR,
    runDenseExactKernel,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Category.Finite.Exact (
    ExactIR,
    ExactIRExecutionError,
    FiniteObject,
    denoteExactIR,
    exactIRSource,
    exactIRTarget,
    finiteObjectValues,
 )
import Markovian.Probability.Exact (
    ExactDistributionError,
    ExactFiniteDist,
    exactFiniteDist,
    exactOutcomes,
    exactProbability,
 )
import Numeric.Natural (Natural)

-- | A row-major dense exact stochastic matrix.
data DenseExactKernel source target
    = DenseExactKernel
        !(FiniteObject source)
        !(FiniteObject target)
        !(NonEmpty (NonEmpty Rational))
    deriving (Eq, Show)

-- | Read the dense source object.
denseExactSource :: DenseExactKernel source target -> FiniteObject source
denseExactSource (DenseExactKernel source _ _) = source

-- | Read the dense target object.
denseExactTarget :: DenseExactKernel source target -> FiniteObject target
denseExactTarget (DenseExactKernel _ target _) = target

-- | Read row and column counts.
denseExactShape :: DenseExactKernel source target -> (Natural, Natural)
denseExactShape (DenseExactKernel source target _) =
    ( supportLength source
    , supportLength target
    )

-- | Read row-major exact probabilities.
denseExactRows :: DenseExactKernel source target -> NonEmpty (NonEmpty Rational)
denseExactRows (DenseExactKernel _ _ rows) = rows

-- | Dense lowering and execution failures.
data DenseExactLoweringError
    = DenseExactDenotationError !ExactIRExecutionError
    | DenseExactDistributionError !ExactDistributionError
    | DenseExactInputOutsideSource
    deriving (Eq, Show)

{- | Lower exact finite syntax to a dense row-major rational matrix.

Source and target support order defines row and column indexing. Precision is
literal 'Rational'. This denotational backend performs no random draws.
-}
lowerExactIR ::
    (Eq source, Eq target) =>
    ExactIR source target ->
    Either DenseExactLoweringError (DenseExactKernel source target)
lowerExactIR expression = do
    rows <-
        traverse
            ( \input -> do
                distribution <- mapDenotationError (denoteExactIR expression input)
                Right (rowFor target distribution)
            )
            (finiteObjectValues source)
    Right (DenseExactKernel source target rows)
  where
    source = exactIRSource expression
    target = exactIRTarget expression

-- | Execute one dense row and reconstruct its exact finite distribution.
runDenseExactKernel ::
    (Eq source) =>
    DenseExactKernel source target ->
    source ->
    Either DenseExactLoweringError (ExactFiniteDist target)
runDenseExactKernel (DenseExactKernel source target rows) requested =
    case lookup requested (zip (NonEmpty.toList (finiteObjectValues source)) (NonEmpty.toList rows)) of
        Nothing -> Left DenseExactInputOutsideSource
        Just row ->
            mapDistributionError
                ( exactFiniteDist
                    ( zip
                        (NonEmpty.toList (finiteObjectValues target))
                        (NonEmpty.toList row)
                    )
                )

rowFor :: (Eq target) => FiniteObject target -> ExactFiniteDist target -> NonEmpty Rational
rowFor target distribution =
    fmap massFor (finiteObjectValues target)
  where
    massFor requested =
        sum
            [ exactProbability mass
            | (value, mass) <- NonEmpty.toList (exactOutcomes distribution)
            , value == requested
            ]

supportLength :: FiniteObject value -> Natural
supportLength = fromIntegral . NonEmpty.length . finiteObjectValues

mapDenotationError ::
    Either ExactIRExecutionError value ->
    Either DenseExactLoweringError value
mapDenotationError = either (Left . DenseExactDenotationError) Right

mapDistributionError ::
    Either ExactDistributionError value ->
    Either DenseExactLoweringError value
mapDistributionError = either (Left . DenseExactDistributionError) Right
