{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}

{- | Reified first-order deterministic categorical translation.

The source fragment contains identities, composition, products, pairing,
projections, and finite quoted tables. It contains no Haskell function values,
function equality, recursion, exceptions, bottoms, or higher-order terms.
-}
module Markovian.Circuit.Compile.Deterministic (
    DeterministicTerm,
    EmptyPrimitive,
    DeterministicTermError (..),
    identityTerm,
    quotedTableTerm,
    composeTerm,
    productTerm,
    pairTerm,
    firstProjectionTerm,
    secondProjectionTerm,
    termSource,
    termTarget,
    denoteDeterministicTerm,
    compileDeterministicTerm,
) where

import Data.Kind (Type)
import Markovian.Algebra.NonNegativeRational
import Markovian.Algebra.Semiring
import Markovian.Category.Finite.Set
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Deterministic
import Markovian.Circuit

{- | Empty primitive signature used because supported source terms compile only
to structural nodes and validated tables.
-}
data EmptyPrimitive (purity :: Purity) (source :: Type) (target :: Type)

-- | Opaque first-order deterministic source terms.
type role DeterministicTerm nominal nominal

data DeterministicTerm source target where
    IdentityTerm :: !(FiniteSet value) -> DeterministicTerm value value
    QuotedTableTerm ::
        !(DeterministicMatrix NonNegativeRational source target) ->
        DeterministicTerm source target
    ComposeTerm ::
        !(FiniteSet source) ->
        !(FiniteSet target) ->
        !(DeterministicTerm source middle) ->
        !(DeterministicTerm middle target) ->
        DeterministicTerm source target
    ProductTerm ::
        !(FiniteSet (leftSource, rightSource)) ->
        !(FiniteSet (leftTarget, rightTarget)) ->
        !(DeterministicTerm leftSource leftTarget) ->
        !(DeterministicTerm rightSource rightTarget) ->
        DeterministicTerm (leftSource, rightSource) (leftTarget, rightTarget)
    PairTerm ::
        !(FiniteSet source) ->
        !(FiniteSet (leftTarget, rightTarget)) ->
        !(DeterministicTerm source leftTarget) ->
        !(DeterministicTerm source rightTarget) ->
        DeterministicTerm source (leftTarget, rightTarget)
    FirstProjectionTerm ::
        !(FiniteSet left) ->
        !(FiniteSet right) ->
        DeterministicTerm (left, right) left
    SecondProjectionTerm ::
        !(FiniteSet left) ->
        !(FiniteSet right) ->
        DeterministicTerm (left, right) right

-- | Source construction, compilation, or matrix-denotation failure.
data DeterministicTermError
    = DeterministicTermCompositionObjectMismatch
    | DeterministicTermPairSourceObjectMismatch
    | DeterministicTermTableDomainMismatch
    | DeterministicTermTableTargetMismatch !Int
    | DeterministicTermTableInvariantFailure
    | DeterministicTermCircuitError !CircuitConstructionError
    | DeterministicTermMatrixError !MatrixError
    | DeterministicTermStructuralInvariantFailure
    deriving (Eq, Show)

-- | Reified source identity.
identityTerm :: FiniteSet value -> DeterministicTerm value value
identityTerm = IdentityTerm

{- | Quote a finite total function table. Validation occurs before construction;
the resulting term stores a one-hot matrix, not a Haskell function.
-}
quotedTableTerm ::
    (Eq source, Eq target) =>
    FiniteSet source ->
    FiniteSet target ->
    [(source, target)] ->
    Either DeterministicTermError (DeterministicTerm source target)
quotedTableTerm source target entries
    | not (validDomain source entries) = Left DeterministicTermTableDomainMismatch
    | otherwise =
        case firstTargetMismatch 0 entries of
            Just index -> Left (DeterministicTermTableTargetMismatch index)
            Nothing ->
                case deterministicMatrix raw of
                    Left _ -> Left DeterministicTermTableInvariantFailure
                    Right table -> Right (QuotedTableTerm table)
  where
    raw =
        matrixFromFunction source target $ \sourceValue targetValue ->
            case lookup sourceValue entries of
                Just selected | selected == targetValue -> one
                _ -> zero
    firstTargetMismatch _ [] = Nothing
    firstTargetMismatch index ((_, value) : remaining)
        | value `elem` finiteSetValues target = firstTargetMismatch (index + 1) remaining
        | otherwise = Just index

-- | Checked source-language composition.
composeTerm ::
    DeterministicTerm source middle ->
    DeterministicTerm middle target ->
    Either DeterministicTermError (DeterministicTerm source target)
composeTerm first second
    | sameFiniteSet (termTarget first) (termSource second) =
        Right (ComposeTerm (termSource first) (termTarget second) first second)
    | otherwise = Left DeterministicTermCompositionObjectMismatch

-- | Source-language product.
productTerm ::
    DeterministicTerm leftSource leftTarget ->
    DeterministicTerm rightSource rightTarget ->
    DeterministicTerm (leftSource, rightSource) (leftTarget, rightTarget)
productTerm left right =
    ProductTerm
        (productSet (termSource left) (termSource right))
        (productSet (termTarget left) (termTarget right))
        left
        right

-- | Checked source-language pairing.
pairTerm ::
    DeterministicTerm source leftTarget ->
    DeterministicTerm source rightTarget ->
    Either
        DeterministicTermError
        (DeterministicTerm source (leftTarget, rightTarget))
pairTerm left right
    | sameFiniteSet (termSource left) (termSource right) =
        Right
            ( PairTerm
                (termSource left)
                (productSet (termTarget left) (termTarget right))
                left
                right
            )
    | otherwise = Left DeterministicTermPairSourceObjectMismatch

-- | First product projection.
firstProjectionTerm ::
    FiniteSet left ->
    FiniteSet right ->
    DeterministicTerm (left, right) left
firstProjectionTerm = FirstProjectionTerm

-- | Second product projection.
secondProjectionTerm ::
    FiniteSet left ->
    FiniteSet right ->
    DeterministicTerm (left, right) right
secondProjectionTerm = SecondProjectionTerm

-- | Read a term's finite source object.
termSource :: DeterministicTerm source target -> FiniteSet source
termSource term =
    case term of
        IdentityTerm source -> source
        QuotedTableTerm table -> deterministicSource table
        ComposeTerm source _ _ _ -> source
        ProductTerm source _ _ _ -> source
        PairTerm source _ _ _ -> source
        FirstProjectionTerm left right -> productSet left right
        SecondProjectionTerm left right -> productSet left right

-- | Read a term's finite target object.
termTarget :: DeterministicTerm source target -> FiniteSet target
termTarget term =
    case term of
        IdentityTerm target -> target
        QuotedTableTerm table -> deterministicTarget table
        ComposeTerm _ target _ _ -> target
        ProductTerm _ target _ _ -> target
        PairTerm _ target _ _ -> target
        FirstProjectionTerm left _ -> left
        SecondProjectionTerm _ right -> right

-- | Independent finite-table denotation of the source language.
denoteDeterministicTerm ::
    DeterministicTerm source target ->
    Either
        DeterministicTermError
        (DeterministicMatrix NonNegativeRational source target)
denoteDeterministicTerm term =
    case term of
        IdentityTerm object -> Right (identityDeterministic object)
        QuotedTableTerm table -> Right table
        ComposeTerm _ _ first second -> do
            firstArrow <- denoteDeterministicTerm first
            secondArrow <- denoteDeterministicTerm second
            mapMatrixError (composeDeterministic firstArrow secondArrow)
        ProductTerm _ _ left right ->
            tensorDeterministic
                <$> denoteDeterministicTerm left
                <*> denoteDeterministicTerm right
        PairTerm source _ left right -> do
            copied <- copyMatrix source
            leftArrow <- denoteDeterministicTerm left
            rightArrow <- denoteDeterministicTerm right
            mapMatrixError
                ( composeDeterministic
                    copied
                    (tensorDeterministic leftArrow rightArrow)
                )
        FirstProjectionTerm left right -> structuralProjection (productSet left right) left fst
        SecondProjectionTerm left right -> structuralProjection (productSet left right) right snd

{- | Compile source terms to raw deterministic circuit syntax.

Projection compilation explicitly uses discard and a unitor. Any returned error
is a checked object-invariant failure, never a partial evaluation.
-}
compileDeterministicTerm ::
    DeterministicTerm source target ->
    Either
        DeterministicTermError
        (Circuit EmptyPrimitive 'Deterministic source target)
compileDeterministicTerm term =
    case term of
        IdentityTerm object -> Right (identityCircuit object)
        QuotedTableTerm table -> Right (deterministicMatrixCircuit table)
        ComposeTerm _ _ first second -> do
            firstCircuit <- compileDeterministicTerm first
            secondCircuit <- compileDeterministicTerm second
            mapCircuitError (composeCircuit firstCircuit secondCircuit)
        ProductTerm _ _ left right ->
            tensorCircuit
                <$> compileDeterministicTerm left
                <*> compileDeterministicTerm right
        PairTerm _ _ left right -> do
            leftCircuit <- compileDeterministicTerm left
            rightCircuit <- compileDeterministicTerm right
            mapCircuitError (fanoutCircuit leftCircuit rightCircuit)
        FirstProjectionTerm left right -> do
            let parallel = tensorCircuit (identityCircuit left) (discardCircuit right)
            mapCircuitError (composeCircuit parallel (rightUnitorCircuit left))
        SecondProjectionTerm left right -> do
            let parallel = tensorCircuit (discardCircuit left) (identityCircuit right)
            mapCircuitError (composeCircuit parallel (leftUnitorCircuit right))

structuralProjection ::
    FiniteSet source ->
    FiniteSet target ->
    (source -> target) ->
    Either
        DeterministicTermError
        (DeterministicMatrix NonNegativeRational source target)
structuralProjection source target function =
    case deterministicFromFunction source target function of
        Left _ -> Left DeterministicTermStructuralInvariantFailure
        Right arrow -> Right arrow

copyMatrix ::
    FiniteSet value ->
    Either
        DeterministicTermError
        (DeterministicMatrix NonNegativeRational value (value, value))
copyMatrix object =
    case deterministicFromFunction object (productSet object object) (\value -> (value, value)) of
        Left _ -> Left DeterministicTermStructuralInvariantFailure
        Right arrow -> Right arrow

validDomain :: (Eq source) => FiniteSet source -> [(source, target)] -> Bool
validDomain source entries =
    length entries == finiteSetCardinality source
        && all (\sourceValue -> count sourceValue == 1) (finiteSetValues source)
        && all (\(sourceValue, _) -> sourceValue `elem` finiteSetValues source) entries
  where
    count requested = length [() | (sourceValue, _) <- entries, sourceValue == requested]

productSet :: FiniteSet left -> FiniteSet right -> FiniteSet (left, right)
productSet (UnsafeFiniteSet left) (UnsafeFiniteSet right) =
    UnsafeFiniteSet
        [ (leftValue, rightValue)
        | leftValue <- left
        , rightValue <- right
        ]

mapCircuitError ::
    Either CircuitConstructionError value ->
    Either DeterministicTermError value
mapCircuitError = either (Left . DeterministicTermCircuitError) Right

mapMatrixError ::
    Either MatrixError value ->
    Either DeterministicTermError value
mapMatrixError = either (Left . DeterministicTermMatrixError) Right
