{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{- | Exact interpretation of raw stochastic-circuit syntax.

Deterministic primitives must return proof-carrying deterministic matrices.
Stochastic primitives return normalized stochastic matrices. Selected fixtures
exact-law-test the stated operation-preservation and coherence equations.
-}
module Markovian.Circuit.Interpret.Exact (
    ExactPrimitiveInterpreter (..),
    ExactCircuitInterpretationError (..),
    ExactArrow,
    exactCircuitAlgebra,
    interpretExactCircuit,
    interpretDeterministicCircuit,
    interpretDeterministicCircuitWithNodeLimit,
    interpretExactMatrix,
    runExactCircuit,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Algebra.NonNegativeRational
import Markovian.Category.Convex.Exact
import Markovian.Category.Finite.Set
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Deterministic
import Markovian.Category.Matrix.Stochastic
import Markovian.Circuit
import Markovian.Probability.Exact
import Numeric.Natural (Natural)

{- | Primitive denotations for the exact algebra. The separate fields make
a dishonest deterministic tag impossible without forging a deterministic
matrix proof.
-}
data ExactPrimitiveInterpreter primitive primitiveError = ExactPrimitiveInterpreter
    { interpretDeterministicPrimitive ::
        forall source target.
        FiniteSet source ->
        FiniteSet target ->
        primitive 'Deterministic source target ->
        Either primitiveError (DeterministicMatrix NonNegativeRational source target)
    , interpretStochasticPrimitive ::
        forall source target.
        FiniteSet source ->
        FiniteSet target ->
        primitive 'Stochastic source target ->
        Either primitiveError (StochasticMatrix NonNegativeRational source target)
    }

-- | Exact structural-fold or execution failure.
data ExactCircuitInterpretationError primitiveError
    = ExactCircuitPrimitiveError !primitiveError
    | ExactCircuitPrimitiveSourceMismatch
    | ExactCircuitPrimitiveTargetMismatch
    | ExactCircuitMatrixError !MatrixError
    | ExactCircuitConvexError !ConvexFamilyError
    | ExactCircuitStructuralInvariantFailure
    | ExactCircuitInputOutsideSource
    | ExactCircuitDistributionError !ExactDistributionError
    deriving (Eq, Show)

-- | Purity-indexed exact target of the structural fold.
data ExactArrow purity source target where
    ExactDeterministicArrow ::
        !(DeterministicMatrix NonNegativeRational source target) ->
        ExactArrow 'Deterministic source target
    ExactStochasticArrow ::
        !(StochasticMatrix NonNegativeRational source target) ->
        ExactArrow 'Stochastic source target

{- | Exact circuit algebra into deterministic and stochastic matrix refinements.

The generic @CircuitAlgebra@ type does not carry laws. Selected exact fixtures
test this implementation's operation-preservation and coherence equations.
-}
exactCircuitAlgebra ::
    ExactPrimitiveInterpreter primitive primitiveError ->
    CircuitAlgebra
        primitive
        ExactArrow
        (ExactCircuitInterpretationError primitiveError)
exactCircuitAlgebra primitives =
    CircuitAlgebra
        { algebraPrimitive = interpretExactPrimitive primitives
        , algebraDeterministicTable = Right . ExactDeterministicArrow
        , algebraIdentity = Right . ExactDeterministicArrow . identityDeterministic
        , algebraCompose = composeArrow
        , algebraTensor = tensorArrow
        , algebraSymmetry = \left right -> deterministicStructural (productSet left right) (productSet right left) (\(leftValue, rightValue) -> (rightValue, leftValue))
        , algebraAssociate = \first second third -> deterministicStructural (productSet (productSet first second) third) (productSet first (productSet second third)) associateValue
        , algebraUnassociate = \first second third -> deterministicStructural (productSet first (productSet second third)) (productSet (productSet first second) third) unassociateValue
        , algebraLeftUnitor = \object -> deterministicStructural (productSet unitSet object) object snd
        , algebraLeftUnitorInverse = \object -> deterministicStructural object (productSet unitSet object) ((),)
        , algebraRightUnitor = \object -> deterministicStructural (productSet object unitSet) object fst
        , algebraRightUnitorInverse = \object -> deterministicStructural object (productSet object unitSet) (,())
        , algebraCopy = \object -> deterministicStructural object (productSet object object) (\value -> (value, value))
        , algebraDiscard = \object -> deterministicStructural object unitSet (const ())
        , algebraConvexChoice = convexArrow
        , algebraWeaken = \(ExactDeterministicArrow arrow) -> Right (ExactStochasticArrow (embedDeterministic arrow))
        }

-- | Fold any circuit to a normalized exact stochastic matrix.
interpretExactCircuit ::
    ExactPrimitiveInterpreter primitive primitiveError ->
    Circuit primitive purity source target ->
    Either
        (ExactCircuitInterpretationError primitiveError)
        (StochasticMatrix NonNegativeRational source target)
interpretExactCircuit primitives circuit =
    exactArrowStochastic <$> foldCircuit (exactCircuitAlgebra primitives) circuit

-- | Fold deterministic syntax while retaining deterministic evidence.
interpretDeterministicCircuit ::
    ExactPrimitiveInterpreter primitive primitiveError ->
    Circuit primitive 'Deterministic source target ->
    Either
        (ExactCircuitInterpretationError primitiveError)
        (DeterministicMatrix NonNegativeRational source target)
interpretDeterministicCircuit primitives circuit = do
    interpreted <- foldCircuit (exactCircuitAlgebra primitives) circuit
    case interpreted of
        ExactDeterministicArrow arrow -> Right arrow

{- | Bounded deterministic interpretation. Raw syntax constructors are charged
before descent; primitive callbacks retain ownership of their own resource
use and termination.
-}
interpretDeterministicCircuitWithNodeLimit ::
    Natural ->
    ExactPrimitiveInterpreter primitive primitiveError ->
    Circuit primitive 'Deterministic source target ->
    Either
        (BoundedCircuitFoldError (ExactCircuitInterpretationError primitiveError))
        (DeterministicMatrix NonNegativeRational source target)
interpretDeterministicCircuitWithNodeLimit limit primitives circuit = do
    (_, interpreted) <- foldCircuitWithNodeLimit limit (exactCircuitAlgebra primitives) circuit
    case interpreted of
        ExactDeterministicArrow arrow -> Right arrow

-- | Fold a circuit to its raw exact semiring matrix denotation.
interpretExactMatrix ::
    ExactPrimitiveInterpreter primitive primitiveError ->
    Circuit primitive purity source target ->
    Either
        (ExactCircuitInterpretationError primitiveError)
        (Matrix NonNegativeRational source target)
interpretExactMatrix primitives circuit =
    forgetStochastic <$> interpretExactCircuit primitives circuit

{- | Apply the exact matrix denotation and reconstruct an exact finite kernel
result. Duplicate labels are aggregated extensionally by matrix entries.
-}
runExactCircuit ::
    (Eq source) =>
    ExactPrimitiveInterpreter primitive primitiveError ->
    Circuit primitive purity source target ->
    source ->
    Either
        (ExactCircuitInterpretationError primitiveError)
        (ExactFiniteDist target)
runExactCircuit primitives circuit input = do
    arrow <- interpretExactCircuit primitives circuit
    if input `elem` finiteSetValues (stochasticSource arrow)
        then
            mapDistributionError . exactFiniteDist $
                [ (targetValue, getNonNegativeRational entry)
                | targetValue <- finiteSetValues (stochasticTarget arrow)
                , Just entry <- [matrixEntry (forgetStochastic arrow) input targetValue]
                ]
        else Left ExactCircuitInputOutsideSource

interpretExactPrimitive ::
    ExactPrimitiveInterpreter primitive primitiveError ->
    SPurity purity ->
    FiniteSet source ->
    FiniteSet target ->
    primitive purity source target ->
    Either
        (ExactCircuitInterpretationError primitiveError)
        (ExactArrow purity source target)
interpretExactPrimitive primitives purity source target primitive =
    case purity of
        SDeterministic -> do
            arrow <- mapPrimitiveError (interpretDeterministicPrimitive primitives source target primitive)
            validateDeterministicEndpoints source target arrow
            Right (ExactDeterministicArrow arrow)
        SStochastic -> do
            arrow <- mapPrimitiveError (interpretStochasticPrimitive primitives source target primitive)
            validateStochasticEndpoints source target arrow
            Right (ExactStochasticArrow arrow)

composeArrow ::
    ExactArrow leftPurity source middle ->
    ExactArrow rightPurity middle target ->
    Either
        (ExactCircuitInterpretationError primitiveError)
        (ExactArrow (JoinPurity leftPurity rightPurity) source target)
composeArrow left right =
    case (left, right) of
        (ExactDeterministicArrow first, ExactDeterministicArrow second) ->
            ExactDeterministicArrow <$> mapMatrixError (composeDeterministic first second)
        (ExactDeterministicArrow first, ExactStochasticArrow second) ->
            ExactStochasticArrow <$> mapMatrixError (composeStochastic (embedDeterministic first) second)
        (ExactStochasticArrow first, ExactDeterministicArrow second) ->
            ExactStochasticArrow <$> mapMatrixError (composeStochastic first (embedDeterministic second))
        (ExactStochasticArrow first, ExactStochasticArrow second) ->
            ExactStochasticArrow <$> mapMatrixError (composeStochastic first second)

tensorArrow ::
    ExactArrow leftPurity leftSource leftTarget ->
    ExactArrow rightPurity rightSource rightTarget ->
    Either
        (ExactCircuitInterpretationError primitiveError)
        ( ExactArrow
            (JoinPurity leftPurity rightPurity)
            (leftSource, rightSource)
            (leftTarget, rightTarget)
        )
tensorArrow left right =
    Right $
        case (left, right) of
            (ExactDeterministicArrow first, ExactDeterministicArrow second) ->
                ExactDeterministicArrow (tensorDeterministic first second)
            (ExactDeterministicArrow first, ExactStochasticArrow second) ->
                ExactStochasticArrow (tensorStochastic (embedDeterministic first) second)
            (ExactStochasticArrow first, ExactDeterministicArrow second) ->
                ExactStochasticArrow (tensorStochastic first (embedDeterministic second))
            (ExactStochasticArrow first, ExactStochasticArrow second) ->
                ExactStochasticArrow (tensorStochastic first second)

convexArrow ::
    NonEmpty.NonEmpty
        (NonNegativeRational, ExactArrow 'Stochastic source target) ->
    Either
        (ExactCircuitInterpretationError primitiveError)
        (ExactArrow 'Stochastic source target)
convexArrow terms = do
    family <-
        mapConvexError . convexFamily $
            fmap (\(coefficient, ExactStochasticArrow arrow) -> (coefficient, arrow)) terms
    Right (ExactStochasticArrow (convexMixture family))

deterministicStructural ::
    FiniteSet source ->
    FiniteSet target ->
    (source -> target) ->
    Either
        (ExactCircuitInterpretationError primitiveError)
        (ExactArrow 'Deterministic source target)
deterministicStructural source target function =
    case deterministicFromFunction source target function of
        Left _ -> Left ExactCircuitStructuralInvariantFailure
        Right arrow -> Right (ExactDeterministicArrow arrow)

exactArrowStochastic ::
    ExactArrow purity source target ->
    StochasticMatrix NonNegativeRational source target
exactArrowStochastic (ExactDeterministicArrow arrow) = embedDeterministic arrow
exactArrowStochastic (ExactStochasticArrow arrow) = arrow

validateDeterministicEndpoints ::
    FiniteSet source ->
    FiniteSet target ->
    DeterministicMatrix scalar source target ->
    Either (ExactCircuitInterpretationError primitiveError) ()
validateDeterministicEndpoints source target arrow
    | not (sameFiniteSetLayout source (deterministicSource arrow)) = Left ExactCircuitPrimitiveSourceMismatch
    | not (sameFiniteSetLayout target (deterministicTarget arrow)) = Left ExactCircuitPrimitiveTargetMismatch
    | otherwise = Right ()

validateStochasticEndpoints ::
    FiniteSet source ->
    FiniteSet target ->
    StochasticMatrix scalar source target ->
    Either (ExactCircuitInterpretationError primitiveError) ()
validateStochasticEndpoints source target arrow
    | not (sameFiniteSetLayout source (stochasticSource arrow)) = Left ExactCircuitPrimitiveSourceMismatch
    | not (sameFiniteSetLayout target (stochasticTarget arrow)) = Left ExactCircuitPrimitiveTargetMismatch
    | otherwise = Right ()

associateValue :: ((first, second), third) -> (first, (second, third))
associateValue ((first, second), third) = (first, (second, third))

unassociateValue :: (first, (second, third)) -> ((first, second), third)
unassociateValue (first, (second, third)) = ((first, second), third)

productSet :: FiniteSet left -> FiniteSet right -> FiniteSet (left, right)
productSet (UnsafeFiniteSet left) (UnsafeFiniteSet right) =
    UnsafeFiniteSet
        [ (leftValue, rightValue)
        | leftValue <- left
        , rightValue <- right
        ]

unitSet :: FiniteSet ()
unitSet = UnsafeFiniteSet [()]

mapPrimitiveError ::
    Either primitiveError value ->
    Either (ExactCircuitInterpretationError primitiveError) value
mapPrimitiveError = either (Left . ExactCircuitPrimitiveError) Right

mapMatrixError ::
    Either MatrixError value ->
    Either (ExactCircuitInterpretationError primitiveError) value
mapMatrixError = either (Left . ExactCircuitMatrixError) Right

mapConvexError ::
    Either ConvexFamilyError value ->
    Either (ExactCircuitInterpretationError primitiveError) value
mapConvexError = either (Left . ExactCircuitConvexError) Right

mapDistributionError ::
    Either ExactDistributionError value ->
    Either (ExactCircuitInterpretationError primitiveError) value
mapDistributionError = either (Left . ExactCircuitDistributionError) Right
