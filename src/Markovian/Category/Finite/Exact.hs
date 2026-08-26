{-# LANGUAGE GADTs #-}

{- | Typed finite categorical syntax with exact stochastic denotation.

Objects are nonempty duplicate-free finite supports. Morphisms preserve typed
source and target objects. Tensor denotes independent draws; copy denotes one
shared value used twice.
-}
module Markovian.Category.Finite.Exact (
    FiniteObject,
    FiniteObjectError (..),
    finiteObject,
    finiteObjectValues,
    ExactIR,
    ExactIRValidationError (..),
    primitiveExactIR,
    deterministicExactIR,
    identityExactIR,
    composeExactIR,
    tensorExactIR,
    swapExactIR,
    associateExactIR,
    unassociateExactIR,
    leftUnitorExactIR,
    leftUnitorInverseExactIR,
    rightUnitorExactIR,
    rightUnitorInverseExactIR,
    copyExactIR,
    fanoutExactIR,
    discardExactIR,
    exactIRSource,
    exactIRTarget,
    ExactIRExecutionError (..),
    denoteExactIR,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Probability.Exact (
    ExactDistributionError,
    ExactFiniteDist,
    exactDirac,
    exactFiniteDist,
    exactOutcomes,
    exactProbability,
 )

-- | Errors from finite object construction.
data FiniteObjectError value
    = EmptyFiniteObject
    | DuplicateFiniteObjectValue !value
    deriving (Eq, Show)

-- | A nonempty duplicate-free finite categorical object.
newtype FiniteObject value = FiniteObject (NonEmpty value)
    deriving (Eq, Show)

-- | Validate one finite object support.
finiteObject :: (Eq value) => [value] -> Either (FiniteObjectError value) (FiniteObject value)
finiteObject [] = Left EmptyFiniteObject
finiteObject values@(first : remaining) =
    case firstDuplicate values of
        Just duplicate -> Left (DuplicateFiniteObjectValue duplicate)
        Nothing -> Right (FiniteObject (first :| remaining))

-- | Read object values in representation order.
finiteObjectValues :: FiniteObject value -> NonEmpty value
finiteObjectValues (FiniteObject values) = values

-- | Typed exact finite stochastic syntax.
data ExactIR source target where
    IdentityIR :: FiniteObject value -> ExactIR value value
    PrimitiveIR ::
        (Eq source) =>
        FiniteObject source ->
        FiniteObject target ->
        [(source, ExactFiniteDist target)] ->
        ExactIR source target
    ComposeIR ::
        (Eq middle) =>
        FiniteObject source ->
        FiniteObject target ->
        ExactIR source middle ->
        ExactIR middle target ->
        ExactIR source target
    TensorIR ::
        (Eq leftSource, Eq rightSource) =>
        FiniteObject (leftSource, rightSource) ->
        FiniteObject (leftTarget, rightTarget) ->
        ExactIR leftSource leftTarget ->
        ExactIR rightSource rightTarget ->
        ExactIR (leftSource, rightSource) (leftTarget, rightTarget)
    CopyIR :: FiniteObject value -> FiniteObject (value, value) -> ExactIR value (value, value)
    DiscardIR :: FiniteObject value -> FiniteObject () -> ExactIR value ()

-- | Validation failures from primitive or composite IR construction.
data ExactIRValidationError value
    = ExactIROutputOutsideTarget !value
    | ExactIRCompositionObjectMismatch
    | ExactIRFanoutSourceObjectMismatch
    deriving (Eq, Show)

-- | Validate one exact primitive against explicit source and target objects.
primitiveExactIR ::
    (Eq source, Eq target) =>
    FiniteObject source ->
    FiniteObject target ->
    (source -> ExactFiniteDist target) ->
    Either (ExactIRValidationError target) (ExactIR source target)
primitiveExactIR source target kernel = do
    rows <-
        traverse
            ( \input -> do
                let distribution = kernel input
                case firstOutside target distribution of
                    Just outside -> Left (ExactIROutputOutsideTarget outside)
                    Nothing -> Right (input, distribution)
            )
            (NonEmpty.toList (finiteObjectValues source))
    Right (PrimitiveIR source target rows)

-- | Lift a deterministic function after validating its target object.
deterministicExactIR ::
    (Eq source, Eq target) =>
    FiniteObject source ->
    FiniteObject target ->
    (source -> target) ->
    Either (ExactIRValidationError target) (ExactIR source target)
deterministicExactIR source target function =
    primitiveExactIR source target (exactDirac . function)

-- | Categorical identity.
identityExactIR :: FiniteObject value -> ExactIR value value
identityExactIR = IdentityIR

-- | Compose morphisms after checking the represented middle object.
composeExactIR ::
    (Eq middle) =>
    ExactIR source middle ->
    ExactIR middle target ->
    Either (ExactIRValidationError middle) (ExactIR source target)
composeExactIR first second
    | exactIRTarget first == exactIRSource second =
        Right (ComposeIR (exactIRSource first) (exactIRTarget second) first second)
    | otherwise = Left ExactIRCompositionObjectMismatch

-- | Tensor two morphisms. The denotation samples each side independently.
tensorExactIR ::
    (Eq leftSource, Eq rightSource) =>
    ExactIR leftSource leftTarget ->
    ExactIR rightSource rightTarget ->
    ExactIR (leftSource, rightSource) (leftTarget, rightTarget)
tensorExactIR left right =
    TensorIR
        (tensorObject (exactIRSource left) (exactIRSource right))
        (tensorObject (exactIRTarget left) (exactIRTarget right))
        left
        right

-- | Swap two finite factors.
swapExactIR ::
    (Eq left, Eq right) =>
    FiniteObject left ->
    FiniteObject right ->
    ExactIR (left, right) (right, left)
swapExactIR left right =
    PrimitiveIR source target rows
  where
    source = tensorObject left right
    target = tensorObject right left
    rows =
        [ (pair, exactDirac (snd pair, fst pair))
        | pair <- NonEmpty.toList (finiteObjectValues source)
        ]

-- | Reassociate a left-nested finite product.
associateExactIR ::
    (Eq first, Eq second, Eq third) =>
    FiniteObject first ->
    FiniteObject second ->
    FiniteObject third ->
    ExactIR ((first, second), third) (first, (second, third))
associateExactIR first second third =
    PrimitiveIR source target rows
  where
    source = tensorObject (tensorObject first second) third
    target = tensorObject first (tensorObject second third)
    rows =
        [ (nested, exactDirac (associateValue nested))
        | nested <- NonEmpty.toList (finiteObjectValues source)
        ]

-- | Reassociate a right-nested finite product.
unassociateExactIR ::
    (Eq first, Eq second, Eq third) =>
    FiniteObject first ->
    FiniteObject second ->
    FiniteObject third ->
    ExactIR (first, (second, third)) ((first, second), third)
unassociateExactIR first second third =
    PrimitiveIR source target rows
  where
    source = tensorObject first (tensorObject second third)
    target = tensorObject (tensorObject first second) third
    rows =
        [ (nested, exactDirac (unassociateValue nested))
        | nested <- NonEmpty.toList (finiteObjectValues source)
        ]

-- | Remove a unit factor on the left.
leftUnitorExactIR :: (Eq value) => FiniteObject value -> ExactIR ((), value) value
leftUnitorExactIR object =
    PrimitiveIR source object rows
  where
    source = tensorObject unitObject object
    rows =
        [ (pair, exactDirac (snd pair))
        | pair <- NonEmpty.toList (finiteObjectValues source)
        ]

-- | Add a unit factor on the left.
leftUnitorInverseExactIR :: (Eq value) => FiniteObject value -> ExactIR value ((), value)
leftUnitorInverseExactIR object =
    PrimitiveIR object target rows
  where
    target = tensorObject unitObject object
    rows =
        [ (value, exactDirac ((), value))
        | value <- NonEmpty.toList (finiteObjectValues object)
        ]

-- | Remove a unit factor on the right.
rightUnitorExactIR :: (Eq value) => FiniteObject value -> ExactIR (value, ()) value
rightUnitorExactIR object =
    PrimitiveIR source object rows
  where
    source = tensorObject object unitObject
    rows =
        [ (pair, exactDirac (fst pair))
        | pair <- NonEmpty.toList (finiteObjectValues source)
        ]

-- | Add a unit factor on the right.
rightUnitorInverseExactIR :: (Eq value) => FiniteObject value -> ExactIR value (value, ())
rightUnitorInverseExactIR object =
    PrimitiveIR object target rows
  where
    target = tensorObject object unitObject
    rows =
        [ (value, exactDirac (value, ()))
        | value <- NonEmpty.toList (finiteObjectValues object)
        ]

-- | Copy one value into the full tensor object. Its denotation has diagonal support.
copyExactIR :: FiniteObject value -> ExactIR value (value, value)
copyExactIR source = CopyIR source (tensorObject source source)

-- | Run two morphisms from one shared input with conditionally independent output draws.
fanoutExactIR ::
    (Eq source) =>
    ExactIR source leftTarget ->
    ExactIR source rightTarget ->
    Either
        (ExactIRValidationError source)
        (ExactIR source (leftTarget, rightTarget))
fanoutExactIR left right
    | source == exactIRSource right =
        Right (ComposeIR source target (copyExactIR source) parallel)
    | otherwise = Left ExactIRFanoutSourceObjectMismatch
  where
    source = exactIRSource left
    parallel = tensorExactIR left right
    target = exactIRTarget parallel

-- | Discard one value into the unit object.
discardExactIR :: FiniteObject value -> ExactIR value ()
discardExactIR source = DiscardIR source unitObject

-- | Read a morphism's source object.
exactIRSource :: ExactIR source target -> FiniteObject source
exactIRSource expression =
    case expression of
        IdentityIR source -> source
        PrimitiveIR source _ _ -> source
        ComposeIR source _ _ _ -> source
        TensorIR source _ _ _ -> source
        CopyIR source _ -> source
        DiscardIR source _ -> source

-- | Read a morphism's target object.
exactIRTarget :: ExactIR source target -> FiniteObject target
exactIRTarget expression =
    case expression of
        IdentityIR target -> target
        PrimitiveIR _ target _ -> target
        ComposeIR _ target _ _ -> target
        TensorIR _ target _ _ -> target
        CopyIR _ target -> target
        DiscardIR _ target -> target

-- | Exact denotation failures.
data ExactIRExecutionError
    = ExactIRInputOutsideSource
    | ExactIRMissingPrimitiveRow
    | ExactIRDenotationDistributionError !ExactDistributionError
    deriving (Eq, Show)

-- | Interpret typed syntax as one exact finite kernel application.
denoteExactIR ::
    (Eq source, Eq target) =>
    ExactIR source target ->
    source ->
    Either ExactIRExecutionError (ExactFiniteDist target)
denoteExactIR expression input
    | input `notElem` finiteObjectValues (exactIRSource expression) =
        Left ExactIRInputOutsideSource
    | otherwise = do
        raw <- denote expression input
        canonicalize (exactIRTarget expression) raw
  where
    denote :: (Eq a) => ExactIR a b -> a -> Either ExactIRExecutionError (ExactFiniteDist b)
    denote current value =
        case current of
            IdentityIR _ -> Right (exactDirac value)
            PrimitiveIR _ _ rows ->
                case lookup value rows of
                    Nothing -> Left ExactIRMissingPrimitiveRow
                    Just distribution -> Right distribution
            ComposeIR _ _ first second -> do
                intermediate <- denote first value
                branches <-
                    fmap
                        concat
                        ( traverse
                            ( \(middle, firstMass) -> do
                                result <- denote second middle
                                Right
                                    [ (output, exactProbability firstMass * exactProbability secondMass)
                                    | (output, secondMass) <- NonEmpty.toList (exactOutcomes result)
                                    ]
                            )
                            (NonEmpty.toList (exactOutcomes intermediate))
                        )
                mapDistributionError (exactFiniteDist branches)
            TensorIR _ _ left right -> do
                leftResult <- denote left (fst value)
                rightResult <- denote right (snd value)
                mapDistributionError
                    ( exactFiniteDist
                        [ ( (leftOutput, rightOutput)
                          , exactProbability leftMass * exactProbability rightMass
                          )
                        | (leftOutput, leftMass) <- NonEmpty.toList (exactOutcomes leftResult)
                        , (rightOutput, rightMass) <- NonEmpty.toList (exactOutcomes rightResult)
                        ]
                    )
            CopyIR _ _ -> Right (exactDirac (value, value))
            DiscardIR _ _ -> Right (exactDirac ())

canonicalize ::
    (Eq value) =>
    FiniteObject value ->
    ExactFiniteDist value ->
    Either ExactIRExecutionError (ExactFiniteDist value)
canonicalize target distribution =
    mapDistributionError
        ( exactFiniteDist
            [ (requested, massFor requested)
            | requested <- NonEmpty.toList (finiteObjectValues target)
            ]
        )
  where
    massFor requested =
        sum
            [ exactProbability mass
            | (value, mass) <- NonEmpty.toList (exactOutcomes distribution)
            , value == requested
            ]

firstOutside ::
    (Eq target) =>
    FiniteObject target ->
    ExactFiniteDist target ->
    Maybe target
firstOutside target distribution = go (NonEmpty.toList (exactOutcomes distribution))
  where
    go [] = Nothing
    go ((value, _) : remaining)
        | value `elem` finiteObjectValues target = go remaining
        | otherwise = Just value

tensorObject :: FiniteObject left -> FiniteObject right -> FiniteObject (left, right)
tensorObject left right =
    FiniteObject
        ( (firstLeft, firstRight)
            :| ( [(firstLeft, rightValue) | rightValue <- remainingRight]
                    ++ [ (leftValue, rightValue)
                       | leftValue <- remainingLeft
                       , rightValue <- firstRight : remainingRight
                       ]
               )
        )
  where
    firstLeft :| remainingLeft = finiteObjectValues left
    firstRight :| remainingRight = finiteObjectValues right

associateValue :: ((first, second), third) -> (first, (second, third))
associateValue ((first, second), third) = (first, (second, third))

unassociateValue :: (first, (second, third)) -> ((first, second), third)
unassociateValue (first, (second, third)) = ((first, second), third)

unitObject :: FiniteObject ()
unitObject = FiniteObject (() :| [])

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate [] = Nothing
firstDuplicate (value : remaining)
    | value `elem` remaining = Just value
    | otherwise = firstDuplicate remaining

mapDistributionError ::
    Either ExactDistributionError value ->
    Either ExactIRExecutionError value
mapDistributionError = either (Left . ExactIRDenotationDistributionError) Right
