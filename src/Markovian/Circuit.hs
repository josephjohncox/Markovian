{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

{- | Raw typed finite stochastic-circuit syntax.

The purity index records construction provenance. It is not inferred from a
circuit's denotation. Structural nodes remain reified, and stochastic sharing
is distinct from independent fanout. This recursive AST has no quotient,
normal form, or claimed universal property.
-}
module Markovian.Circuit (
    Purity (..),
    SPurity (..),
    JoinPurity,
    Circuit,
    CircuitConstructionError (..),
    deterministicPrimitive,
    stochasticPrimitive,
    deterministicTable,
    deterministicMatrixCircuit,
    identityCircuit,
    composeCircuit,
    tensorCircuit,
    symmetryCircuit,
    associateCircuit,
    unassociateCircuit,
    leftUnitorCircuit,
    leftUnitorInverseCircuit,
    rightUnitorCircuit,
    rightUnitorInverseCircuit,
    copyCircuit,
    discardCircuit,
    convexChoice,
    weakenPurity,
    shareCircuit,
    fanoutCircuit,
    copyNaturalDeterministic,
    circuitSource,
    circuitTarget,
    circuitPurity,
    CircuitAlgebra (..),
    foldCircuit,
) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Markovian.Algebra.NonNegativeRational (NonNegativeRational)
import Markovian.Algebra.Semiring
import Markovian.Category.Finite.Set
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Deterministic

-- | Construction provenance for a circuit.
data Purity = Deterministic | Stochastic

-- | Singleton evidence for circuit purity.
data SPurity (purity :: Purity) where
    SDeterministic :: SPurity 'Deterministic
    SStochastic :: SPurity 'Stochastic

{- | Composition and tensor retain deterministic provenance only when both
operands are deterministic.
-}
type JoinPurity :: Purity -> Purity -> Purity
type family JoinPurity left right where
    JoinPurity 'Deterministic 'Deterministic = 'Deterministic
    JoinPurity 'Deterministic 'Stochastic = 'Stochastic
    JoinPurity 'Stochastic 'Deterministic = 'Stochastic
    JoinPurity 'Stochastic 'Stochastic = 'Stochastic

-- | Opaque recursive circuit syntax over a purity-indexed primitive signature.
type role Circuit nominal nominal nominal nominal

data
    Circuit
        (primitive :: Purity -> Type -> Type -> Type)
        (purity :: Purity)
        source
        target
    where
    IdentityCircuit :: !(FiniteSet value) -> Circuit primitive 'Deterministic value value
    PrimitiveCircuit ::
        !(SPurity purity) ->
        !(FiniteSet source) ->
        !(FiniteSet target) ->
        !(primitive purity source target) ->
        Circuit primitive purity source target
    TableCircuit ::
        !(DeterministicMatrix NonNegativeRational source target) ->
        Circuit primitive 'Deterministic source target
    ComposeCircuit ::
        !(FiniteSet source) ->
        !(FiniteSet target) ->
        !(Circuit primitive leftPurity source middle) ->
        !(Circuit primitive rightPurity middle target) ->
        Circuit primitive (JoinPurity leftPurity rightPurity) source target
    TensorCircuit ::
        !(FiniteSet (leftSource, rightSource)) ->
        !(FiniteSet (leftTarget, rightTarget)) ->
        !(Circuit primitive leftPurity leftSource leftTarget) ->
        !(Circuit primitive rightPurity rightSource rightTarget) ->
        Circuit
            primitive
            (JoinPurity leftPurity rightPurity)
            (leftSource, rightSource)
            (leftTarget, rightTarget)
    SymmetryCircuit ::
        !(FiniteSet left) ->
        !(FiniteSet right) ->
        !(FiniteSet (left, right)) ->
        !(FiniteSet (right, left)) ->
        Circuit primitive 'Deterministic (left, right) (right, left)
    AssociateCircuit ::
        !(FiniteSet first) ->
        !(FiniteSet second) ->
        !(FiniteSet third) ->
        !(FiniteSet ((first, second), third)) ->
        !(FiniteSet (first, (second, third))) ->
        Circuit primitive 'Deterministic ((first, second), third) (first, (second, third))
    UnassociateCircuit ::
        !(FiniteSet first) ->
        !(FiniteSet second) ->
        !(FiniteSet third) ->
        !(FiniteSet (first, (second, third))) ->
        !(FiniteSet ((first, second), third)) ->
        Circuit primitive 'Deterministic (first, (second, third)) ((first, second), third)
    LeftUnitorCircuit ::
        !(FiniteSet value) ->
        !(FiniteSet ((), value)) ->
        Circuit primitive 'Deterministic ((), value) value
    LeftUnitorInverseCircuit ::
        !(FiniteSet value) ->
        !(FiniteSet ((), value)) ->
        Circuit primitive 'Deterministic value ((), value)
    RightUnitorCircuit ::
        !(FiniteSet value) ->
        !(FiniteSet (value, ())) ->
        Circuit primitive 'Deterministic (value, ()) value
    RightUnitorInverseCircuit ::
        !(FiniteSet value) ->
        !(FiniteSet (value, ())) ->
        Circuit primitive 'Deterministic value (value, ())
    CopyCircuit ::
        !(FiniteSet value) ->
        !(FiniteSet (value, value)) ->
        Circuit primitive 'Deterministic value (value, value)
    DiscardCircuit ::
        !(FiniteSet value) ->
        Circuit primitive 'Deterministic value ()
    ConvexCircuit ::
        !(FiniteSet source) ->
        !(FiniteSet target) ->
        !(NonEmpty (NonNegativeRational, Circuit primitive 'Stochastic source target)) ->
        Circuit primitive 'Stochastic source target
    WeakenCircuit ::
        !(Circuit primitive 'Deterministic source target) ->
        Circuit primitive 'Stochastic source target
    ShareCircuit ::
        !(FiniteSet source) ->
        !(FiniteSet (target, target)) ->
        !(Circuit primitive purity source target) ->
        Circuit primitive purity source (target, target)
    FanoutCircuit ::
        !(FiniteSet source) ->
        !(FiniteSet (leftTarget, rightTarget)) ->
        !(Circuit primitive leftPurity source leftTarget) ->
        !(Circuit primitive rightPurity source rightTarget) ->
        Circuit primitive (JoinPurity leftPurity rightPurity) source (leftTarget, rightTarget)

-- | Checked circuit-construction failure.
data CircuitConstructionError
    = CircuitCompositionObjectMismatch
    | CircuitFanoutSourceObjectMismatch
    | CircuitInvalidConvexCoefficients
    | CircuitConvexSourceObjectMismatch !Int
    | CircuitConvexTargetObjectMismatch !Int
    | CircuitDeterministicTableDomainMismatch
    | CircuitDeterministicTableTargetMismatch !Int
    | CircuitDeterministicTableInvariantFailure
    deriving (Eq, Show)

-- | Introduce a primitive whose interpreter must return deterministic evidence.
deterministicPrimitive ::
    FiniteSet source ->
    FiniteSet target ->
    primitive 'Deterministic source target ->
    Circuit primitive 'Deterministic source target
deterministicPrimitive = PrimitiveCircuit SDeterministic

-- | Introduce a primitive with stochastic construction provenance.
stochasticPrimitive ::
    FiniteSet source ->
    FiniteSet target ->
    primitive 'Stochastic source target ->
    Circuit primitive 'Stochastic source target
stochasticPrimitive = PrimitiveCircuit SStochastic

{- | Introduce a validated finite table. The table is reified as one-hot matrix
syntax; no Haskell function is retained or compared.
-}
deterministicTable ::
    (Eq source, Eq target) =>
    FiniteSet source ->
    FiniteSet target ->
    [(source, target)] ->
    Either CircuitConstructionError (Circuit primitive 'Deterministic source target)
deterministicTable source target entries
    | not (validDomain source entries) = Left CircuitDeterministicTableDomainMismatch
    | otherwise =
        case firstTargetMismatch 0 entries of
            Just index -> Left (CircuitDeterministicTableTargetMismatch index)
            Nothing ->
                case deterministicMatrix raw of
                    Left _ -> Left CircuitDeterministicTableInvariantFailure
                    Right table -> Right (TableCircuit table)
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

-- | Reify an already validated finite deterministic table matrix.
deterministicMatrixCircuit ::
    DeterministicMatrix NonNegativeRational source target ->
    Circuit primitive 'Deterministic source target
deterministicMatrixCircuit = TableCircuit

-- | Reified categorical identity.
identityCircuit :: FiniteSet value -> Circuit primitive 'Deterministic value value
identityCircuit = IdentityCircuit

-- | Checked left-to-right circuit composition.
composeCircuit ::
    Circuit primitive leftPurity source middle ->
    Circuit primitive rightPurity middle target ->
    Either
        CircuitConstructionError
        (Circuit primitive (JoinPurity leftPurity rightPurity) source target)
composeCircuit first second
    | sameFiniteSet (circuitTarget first) (circuitSource second) =
        Right (ComposeCircuit (circuitSource first) (circuitTarget second) first second)
    | otherwise = Left CircuitCompositionObjectMismatch

-- | Independent parallel tensor.
tensorCircuit ::
    Circuit primitive leftPurity leftSource leftTarget ->
    Circuit primitive rightPurity rightSource rightTarget ->
    Circuit
        primitive
        (JoinPurity leftPurity rightPurity)
        (leftSource, rightSource)
        (leftTarget, rightTarget)
tensorCircuit left right =
    TensorCircuit
        (productSet (circuitSource left) (circuitSource right))
        (productSet (circuitTarget left) (circuitTarget right))
        left
        right

-- | Reified tensor symmetry.
symmetryCircuit ::
    FiniteSet left ->
    FiniteSet right ->
    Circuit primitive 'Deterministic (left, right) (right, left)
symmetryCircuit left right =
    SymmetryCircuit left right (productSet left right) (productSet right left)

-- | Reified forward tensor associator.
associateCircuit ::
    FiniteSet first ->
    FiniteSet second ->
    FiniteSet third ->
    Circuit primitive 'Deterministic ((first, second), third) (first, (second, third))
associateCircuit first second third =
    AssociateCircuit first second third source target
  where
    source = productSet (productSet first second) third
    target = productSet first (productSet second third)

-- | Reified inverse tensor associator.
unassociateCircuit ::
    FiniteSet first ->
    FiniteSet second ->
    FiniteSet third ->
    Circuit primitive 'Deterministic (first, (second, third)) ((first, second), third)
unassociateCircuit first second third =
    UnassociateCircuit first second third source target
  where
    source = productSet first (productSet second third)
    target = productSet (productSet first second) third

-- | Reified left unitor.
leftUnitorCircuit ::
    FiniteSet value ->
    Circuit primitive 'Deterministic ((), value) value
leftUnitorCircuit object = LeftUnitorCircuit object (productSet unitSet object)

-- | Reified inverse left unitor.
leftUnitorInverseCircuit ::
    FiniteSet value ->
    Circuit primitive 'Deterministic value ((), value)
leftUnitorInverseCircuit object = LeftUnitorInverseCircuit object (productSet unitSet object)

-- | Reified right unitor.
rightUnitorCircuit ::
    FiniteSet value ->
    Circuit primitive 'Deterministic (value, ()) value
rightUnitorCircuit object = RightUnitorCircuit object (productSet object unitSet)

-- | Reified inverse right unitor.
rightUnitorInverseCircuit ::
    FiniteSet value ->
    Circuit primitive 'Deterministic value (value, ())
rightUnitorInverseCircuit object = RightUnitorInverseCircuit object (productSet object unitSet)

-- | Reified diagonal copy.
copyCircuit :: FiniteSet value -> Circuit primitive 'Deterministic value (value, value)
copyCircuit object = CopyCircuit object (productSet object object)

-- | Reified discard into the tensor unit.
discardCircuit :: FiniteSet value -> Circuit primitive 'Deterministic value ()
discardCircuit = DiscardCircuit

{- | Construct a checked finite convex choice. Terms are conservatively
stochastic, including a literal singleton family.
-}
convexChoice ::
    NonEmpty (NonNegativeRational, Circuit primitive 'Stochastic source target) ->
    Either CircuitConstructionError (Circuit primitive 'Stochastic source target)
convexChoice terms@((_, first) :| remaining)
    | not (validConvexCoefficients (fmap fst terms)) = Left CircuitInvalidConvexCoefficients
    | otherwise = checkObjects 1 remaining
  where
    checkObjects _ [] = Right (ConvexCircuit (circuitSource first) (circuitTarget first) terms)
    checkObjects index ((_, circuit) : rest)
        | not (sameFiniteSet (circuitSource first) (circuitSource circuit)) =
            Left (CircuitConvexSourceObjectMismatch index)
        | not (sameFiniteSet (circuitTarget first) (circuitTarget circuit)) =
            Left (CircuitConvexTargetObjectMismatch index)
        | otherwise = checkObjects (index + 1) rest

-- | The only purity cast: deterministic provenance can be forgotten.
weakenPurity ::
    Circuit primitive 'Deterministic source target ->
    Circuit primitive 'Stochastic source target
weakenPurity = WeakenCircuit

{- | Execute once, then copy the one result. A stochastic circuit therefore
performs one draw and returns correlated outputs.
-}
shareCircuit ::
    Circuit primitive purity source target ->
    Circuit primitive purity source (target, target)
shareCircuit circuit =
    ShareCircuit (circuitSource circuit) (productSet target target) circuit
  where
    target = circuitTarget circuit

{- | Copy the input, then execute both branches independently conditional on
that input.
-}
fanoutCircuit ::
    Circuit primitive leftPurity source leftTarget ->
    Circuit primitive rightPurity source rightTarget ->
    Either
        CircuitConstructionError
        ( Circuit
            primitive
            (JoinPurity leftPurity rightPurity)
            source
            (leftTarget, rightTarget)
        )
fanoutCircuit left right
    | sameFiniteSet (circuitSource left) (circuitSource right) =
        Right
            ( FanoutCircuit
                (circuitSource left)
                (productSet (circuitTarget left) (circuitTarget right))
                left
                right
            )
    | otherwise = Left CircuitFanoutSourceObjectMismatch

{- | Copy-naturality rewrite available only with deterministic syntax evidence.
It rewrites @circuit ; copy@ to @copy ; (circuit tensor circuit)@. Semantic
soundness additionally requires the chosen algebra to satisfy deterministic
copy naturality; the exact algebra has direct law tests for this obligation.
-}
copyNaturalDeterministic ::
    Circuit primitive 'Deterministic source target ->
    Circuit primitive 'Deterministic source (target, target)
copyNaturalDeterministic circuit =
    FanoutCircuit
        (circuitSource circuit)
        (productSet target target)
        circuit
        circuit
  where
    target = circuitTarget circuit

-- | Read the represented source object.
circuitSource :: Circuit primitive purity source target -> FiniteSet source
circuitSource circuit =
    case circuit of
        IdentityCircuit source -> source
        PrimitiveCircuit _ source _ _ -> source
        TableCircuit table -> deterministicSource table
        ComposeCircuit source _ _ _ -> source
        TensorCircuit source _ _ _ -> source
        SymmetryCircuit _ _ source _ -> source
        AssociateCircuit _ _ _ source _ -> source
        UnassociateCircuit _ _ _ source _ -> source
        LeftUnitorCircuit _ source -> source
        LeftUnitorInverseCircuit source _ -> source
        RightUnitorCircuit _ source -> source
        RightUnitorInverseCircuit source _ -> source
        CopyCircuit source _ -> source
        DiscardCircuit source -> source
        ConvexCircuit source _ _ -> source
        WeakenCircuit inner -> circuitSource inner
        ShareCircuit source _ _ -> source
        FanoutCircuit source _ _ _ -> source

-- | Read the represented target object.
circuitTarget :: Circuit primitive purity source target -> FiniteSet target
circuitTarget circuit =
    case circuit of
        IdentityCircuit target -> target
        PrimitiveCircuit _ _ target _ -> target
        TableCircuit table -> deterministicTarget table
        ComposeCircuit _ target _ _ -> target
        TensorCircuit _ target _ _ -> target
        SymmetryCircuit _ _ _ target -> target
        AssociateCircuit _ _ _ _ target -> target
        UnassociateCircuit _ _ _ _ target -> target
        LeftUnitorCircuit target _ -> target
        LeftUnitorInverseCircuit _ target -> target
        RightUnitorCircuit target _ -> target
        RightUnitorInverseCircuit _ target -> target
        CopyCircuit _ target -> target
        DiscardCircuit _ -> unitSet
        ConvexCircuit _ target _ -> target
        WeakenCircuit inner -> circuitTarget inner
        ShareCircuit _ target _ -> target
        FanoutCircuit _ target _ _ -> target

-- | Read singleton purity evidence.
circuitPurity :: Circuit primitive purity source target -> SPurity purity
circuitPurity circuit =
    case circuit of
        IdentityCircuit _ -> SDeterministic
        PrimitiveCircuit purity _ _ _ -> purity
        TableCircuit _ -> SDeterministic
        ComposeCircuit _ _ left right -> joinSPurity (circuitPurity left) (circuitPurity right)
        TensorCircuit _ _ left right -> joinSPurity (circuitPurity left) (circuitPurity right)
        SymmetryCircuit{} -> SDeterministic
        AssociateCircuit{} -> SDeterministic
        UnassociateCircuit{} -> SDeterministic
        LeftUnitorCircuit{} -> SDeterministic
        LeftUnitorInverseCircuit{} -> SDeterministic
        RightUnitorCircuit{} -> SDeterministic
        RightUnitorInverseCircuit{} -> SDeterministic
        CopyCircuit{} -> SDeterministic
        DiscardCircuit{} -> SDeterministic
        ConvexCircuit{} -> SStochastic
        WeakenCircuit{} -> SStochastic
        ShareCircuit _ _ inner -> circuitPurity inner
        FanoutCircuit _ _ left right -> joinSPurity (circuitPurity left) (circuitPurity right)

{- | Operations for folding the raw circuit AST.

This record is an unchecked syntax algebra, not evidence of categorical laws.
An implementation that claims homomorphism laws must separately prove or test
identity, composition, monoidal, comonoid, convex, and weakening obligations.
Sharing and fanout are deliberately absent as independent handlers: 'foldCircuit'
derives them from composition, tensor, and copy.
-}
data CircuitAlgebra primitive target error = CircuitAlgebra
    { algebraPrimitive ::
        forall purity source result.
        SPurity purity ->
        FiniteSet source ->
        FiniteSet result ->
        primitive purity source result ->
        Either error (target purity source result)
    , algebraDeterministicTable ::
        forall source result.
        DeterministicMatrix NonNegativeRational source result ->
        Either error (target 'Deterministic source result)
    , algebraIdentity ::
        forall value.
        FiniteSet value ->
        Either error (target 'Deterministic value value)
    , algebraCompose ::
        forall leftPurity rightPurity source middle result.
        target leftPurity source middle ->
        target rightPurity middle result ->
        Either error (target (JoinPurity leftPurity rightPurity) source result)
    , algebraTensor ::
        forall leftPurity rightPurity leftSource leftTarget rightSource rightTarget.
        target leftPurity leftSource leftTarget ->
        target rightPurity rightSource rightTarget ->
        Either
            error
            ( target
                (JoinPurity leftPurity rightPurity)
                (leftSource, rightSource)
                (leftTarget, rightTarget)
            )
    , algebraSymmetry ::
        forall left right.
        FiniteSet left ->
        FiniteSet right ->
        Either error (target 'Deterministic (left, right) (right, left))
    , algebraAssociate ::
        forall first second third.
        FiniteSet first ->
        FiniteSet second ->
        FiniteSet third ->
        Either error (target 'Deterministic ((first, second), third) (first, (second, third)))
    , algebraUnassociate ::
        forall first second third.
        FiniteSet first ->
        FiniteSet second ->
        FiniteSet third ->
        Either error (target 'Deterministic (first, (second, third)) ((first, second), third))
    , algebraLeftUnitor ::
        forall value.
        FiniteSet value ->
        Either error (target 'Deterministic ((), value) value)
    , algebraLeftUnitorInverse ::
        forall value.
        FiniteSet value ->
        Either error (target 'Deterministic value ((), value))
    , algebraRightUnitor ::
        forall value.
        FiniteSet value ->
        Either error (target 'Deterministic (value, ()) value)
    , algebraRightUnitorInverse ::
        forall value.
        FiniteSet value ->
        Either error (target 'Deterministic value (value, ()))
    , algebraCopy ::
        forall value.
        FiniteSet value ->
        Either error (target 'Deterministic value (value, value))
    , algebraDiscard ::
        forall value.
        FiniteSet value ->
        Either error (target 'Deterministic value ())
    , algebraConvexChoice ::
        forall source result.
        NonEmpty (NonNegativeRational, target 'Stochastic source result) ->
        Either error (target 'Stochastic source result)
    , algebraWeaken ::
        forall source result.
        target 'Deterministic source result ->
        Either error (target 'Stochastic source result)
    }

{- | Fold recursive syntax. Sharing is interpreted as @circuit ; copy@, and
fanout as @copy ; (left tensor right)@ for every algebra.
-}
foldCircuit ::
    CircuitAlgebra primitive target error ->
    Circuit primitive purity source result ->
    Either error (target purity source result)
foldCircuit algebra circuit =
    case circuit of
        IdentityCircuit object -> algebraIdentity algebra object
        PrimitiveCircuit purity source target primitive ->
            algebraPrimitive algebra purity source target primitive
        TableCircuit table -> algebraDeterministicTable algebra table
        ComposeCircuit _ _ first second -> do
            interpretedFirst <- foldCircuit algebra first
            interpretedSecond <- foldCircuit algebra second
            algebraCompose algebra interpretedFirst interpretedSecond
        TensorCircuit _ _ left right -> do
            interpretedLeft <- foldCircuit algebra left
            interpretedRight <- foldCircuit algebra right
            algebraTensor algebra interpretedLeft interpretedRight
        SymmetryCircuit left right _ _ -> algebraSymmetry algebra left right
        AssociateCircuit first second third _ _ -> algebraAssociate algebra first second third
        UnassociateCircuit first second third _ _ -> algebraUnassociate algebra first second third
        LeftUnitorCircuit object _ -> algebraLeftUnitor algebra object
        LeftUnitorInverseCircuit object _ -> algebraLeftUnitorInverse algebra object
        RightUnitorCircuit object _ -> algebraRightUnitor algebra object
        RightUnitorInverseCircuit object _ -> algebraRightUnitorInverse algebra object
        CopyCircuit object _ -> algebraCopy algebra object
        DiscardCircuit object -> algebraDiscard algebra object
        ConvexCircuit _ _ terms -> do
            interpreted <- traverse (traverse (foldCircuit algebra)) terms
            algebraConvexChoice algebra interpreted
        WeakenCircuit inner -> foldCircuit algebra inner >>= algebraWeaken algebra
        ShareCircuit _ _ inner ->
            case circuitPurity inner of
                SDeterministic -> do
                    interpreted <- foldCircuit algebra inner
                    copied <- algebraCopy algebra (circuitTarget inner)
                    algebraCompose algebra interpreted copied
                SStochastic -> do
                    interpreted <- foldCircuit algebra inner
                    copied <- algebraCopy algebra (circuitTarget inner)
                    algebraCompose algebra interpreted copied
        FanoutCircuit source _ left right ->
            case (circuitPurity left, circuitPurity right) of
                (SDeterministic, SDeterministic) -> do
                    copied <- algebraCopy algebra source
                    interpretedLeft <- foldCircuit algebra left
                    interpretedRight <- foldCircuit algebra right
                    parallel <- algebraTensor algebra interpretedLeft interpretedRight
                    algebraCompose algebra copied parallel
                (SDeterministic, SStochastic) -> do
                    copied <- algebraCopy algebra source
                    interpretedLeft <- foldCircuit algebra left
                    interpretedRight <- foldCircuit algebra right
                    parallel <- algebraTensor algebra interpretedLeft interpretedRight
                    algebraCompose algebra copied parallel
                (SStochastic, SDeterministic) -> do
                    copied <- algebraCopy algebra source
                    interpretedLeft <- foldCircuit algebra left
                    interpretedRight <- foldCircuit algebra right
                    parallel <- algebraTensor algebra interpretedLeft interpretedRight
                    algebraCompose algebra copied parallel
                (SStochastic, SStochastic) -> do
                    copied <- algebraCopy algebra source
                    interpretedLeft <- foldCircuit algebra left
                    interpretedRight <- foldCircuit algebra right
                    parallel <- algebraTensor algebra interpretedLeft interpretedRight
                    algebraCompose algebra copied parallel

validDomain :: (Eq source) => FiniteSet source -> [(source, target)] -> Bool
validDomain source entries =
    length entries == finiteSetCardinality source
        && all (\sourceValue -> count sourceValue == 1) (finiteSetValues source)
        && all (\(sourceValue, _) -> sourceValue `elem` finiteSetValues source) entries
  where
    count requested = length [() | (sourceValue, _) <- entries, sourceValue == requested]

joinSPurity :: SPurity left -> SPurity right -> SPurity (JoinPurity left right)
joinSPurity SDeterministic SDeterministic = SDeterministic
joinSPurity SDeterministic SStochastic = SStochastic
joinSPurity SStochastic SDeterministic = SStochastic
joinSPurity SStochastic SStochastic = SStochastic

productSet :: FiniteSet left -> FiniteSet right -> FiniteSet (left, right)
productSet (UnsafeFiniteSet left) (UnsafeFiniteSet right) =
    UnsafeFiniteSet
        [ (leftValue, rightValue)
        | leftValue <- left
        , rightValue <- right
        ]

unitSet :: FiniteSet ()
unitSet = UnsafeFiniteSet [()]
