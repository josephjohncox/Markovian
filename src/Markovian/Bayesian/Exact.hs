{-# LANGUAGE RoleAnnotations #-}

{- | Exact finite Bayesian operations over normalized nonnegative matrices.

Bayesian inversion is indexed by a prior and restricts both endpoints to their
positive supports. It is not matrix conjugate transpose and is not a dagger.
The distribution bridge at the end of this module owns the generic exact
normalization used by the POMDP compatibility surface.
-}
module Markovian.Bayesian.Exact (
    Prior,
    PriorError (..),
    prior,
    priorObject,
    priorMass,
    priorStochastic,
    priorEquivalent,
    Support,
    supportObject,
    supportValues,
    priorSupport,
    supportedPrior,
    Posterior,
    posteriorPrior,
    posteriorObject,
    posteriorSupport,
    posteriorMass,
    BayesianError (..),
    ConditioningError (..),
    pushforward,
    joint,
    tensorPrior,
    observationEvidence,
    condition,
    BayesianInverse,
    bayesianInverse,
    disintegrate,
    inverseMatrix,
    inverseInputPrior,
    inverseOutputPrior,
    inverseForwardRestriction,
    almostSureEqual,
    ExactDistributionBayesianError (..),
    ExactConditioningError (..),
    canonicalExactDistribution,
    pushforwardExactDistribution,
    conditionExactDistribution,
) where

import Data.Foldable (foldl', traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Markovian.Algebra.NonNegativeRational
import Markovian.Algebra.Semiring
import Markovian.Category.Finite.Object
import Markovian.Category.Finite.Set
import Markovian.Category.Finite.Set.Internal (FiniteSet (UnsafeFiniteSet))
import Markovian.Category.Matrix
import Markovian.Category.Matrix.Stochastic
import Markovian.Category.Matrix.Stochastic.Internal (StochasticMatrix (UnsafeStochasticMatrix))
import Markovian.Probability.Exact

-- | A normalized exact state on one explicit nonempty finite object.
type role Prior nominal

data Prior value
    = Prior
        !(FiniteObject value)
        !(Support value)
        !(StochasticMatrix NonNegativeRational () value)

-- | Exact prior validation failure.
data PriorError value
    = NegativePriorMass !Int !Rational
    | PriorValueOutsideObject !value
    | PriorMassNotOne !Rational
    | PriorInternalInvariantFailure
    deriving (Eq, Show)

-- | A positive finite support. A normalized prior always has nonempty support.
type role Support nominal

newtype Support value = Support (FiniteObject value)

-- | A conditioned normalized state.
type role Posterior nominal

newtype Posterior value = Posterior (Prior value)

-- | Structural Bayesian-operation failure.
data BayesianError
    = BayesianSourceObjectMismatch
    | BayesianTargetObjectMismatch
    | BayesianMatrixError !MatrixError
    | BayesianNormalizationError !(StochasticMatrixError NonNegativeRational)
    | BayesianDivisionByZero
    | BayesianInternalInvariantFailure
    deriving (Eq, Show)

-- | Observation-specific conditioning failure.
data ConditioningError observation
    = ConditioningBayesianError !BayesianError
    | ObservationOutsideTarget !observation
    | ZeroEvidence !observation
    | ConditioningInternalInvariantFailure
    deriving (Eq, Show)

-- | A support-restricted prior-indexed Bayesian inverse.
type role BayesianInverse nominal nominal

data BayesianInverse source target
    = BayesianInverse
        !(Prior source)
        !(Prior target)
        !(StochasticMatrix NonNegativeRational target source)
        !(StochasticMatrix NonNegativeRational source target)

{- | Construct an already-normalized prior. Duplicate labels are aggregated
extensionally in object-layout order.
-}
prior ::
    (Eq value) =>
    FiniteObject value ->
    [(value, Rational)] ->
    Either (PriorError value) (Prior value)
prior object entries = do
    traverse_Indexed validateMass entries
    case firstOutside entries of
        Just outside -> Left (PriorValueOutsideObject outside)
        Nothing -> do
            let masses = aggregate entries
                total = sum (map snd masses)
            if total /= 1
                then Left (PriorMassNotOne total)
                else buildPrior object masses
  where
    validateMass index (_, mass)
        | mass < 0 = Left (NegativePriorMass index mass)
        | otherwise = Right ()
    traverse_Indexed function = traverse_ (uncurry function) . zip [0 ..]
    firstOutside [] = Nothing
    firstOutside ((value, _) : remaining)
        | value `elem` finiteObjectValues object = firstOutside remaining
        | otherwise = Just value

-- | Read the represented prior object.
priorObject :: Prior value -> FiniteObject value
priorObject (Prior object _ _) = object

{- | Read one represented mass. Values outside the prior object return
'Nothing'.
-}
priorMass :: Prior value -> value -> Maybe NonNegativeRational
priorMass (Prior _ _ state) = matrixEntry (forgetStochastic state) ()

-- | Read the normalized state matrix.
priorStochastic :: Prior value -> StochasticMatrix NonNegativeRational () value
priorStochastic (Prior _ _ state) = state

-- | Extensional equality of prior objects and labelled masses.
priorEquivalent :: Prior value -> Prior value -> Bool
priorEquivalent left right =
    sameFiniteSupport (priorObject left) (priorObject right)
        && all
            (\value -> priorMass left value == priorMass right value)
            (NonEmpty.toList (finiteObjectValues (priorObject left)))

-- | Read the support as a nonempty finite object.
supportObject :: Support value -> FiniteObject value
supportObject (Support object) = object

-- | Read positive support values in parent-object layout order.
supportValues :: Support value -> NonEmpty value
supportValues = finiteObjectValues . supportObject

-- | Extract the positive support carried by a prior.
priorSupport :: Prior value -> Support value
priorSupport (Prior _ support _) = support

{- | Restrict a prior to its positive support. This changes only its explicit
object witness; all positive masses remain unchanged.
-}
supportedPrior :: Prior value -> Prior value
supportedPrior original@(Prior _ support _) =
    Prior object support state
  where
    object = supportObject support
    state =
        UnsafeStochasticMatrix $
            matrixFromFunction unitSet (forgetNonempty object) $ \() value ->
                massOrZero original value

-- | Forget the posterior marker while retaining normalization and support.
posteriorPrior :: Posterior value -> Prior value
posteriorPrior (Posterior result) = result

-- | Read the posterior's full represented object.
posteriorObject :: Posterior value -> FiniteObject value
posteriorObject = priorObject . posteriorPrior

-- | Read the posterior's positive support.
posteriorSupport :: Posterior value -> Support value
posteriorSupport = priorSupport . posteriorPrior

-- | Read one posterior mass.
posteriorMass :: Posterior value -> value -> Maybe NonNegativeRational
posteriorMass = priorMass . posteriorPrior

-- | Push a prior through an exact stochastic channel.
pushforward ::
    (Eq target) =>
    Prior source ->
    StochasticMatrix NonNegativeRational source target ->
    Either BayesianError (Prior target)
pushforward sourcePrior channel
    | not (sameFiniteSet (forgetNonempty (priorObject sourcePrior)) (stochasticSource channel)) =
        Left BayesianSourceObjectMismatch
    | otherwise = do
        state <- mapMatrix (composeStochastic (priorStochastic sourcePrior) channel)
        priorFromState state

-- | Construct the normalized joint state @p(x) K(y|x)@.
joint ::
    (Eq source, Eq target) =>
    Prior source ->
    StochasticMatrix NonNegativeRational source target ->
    Either BayesianError (Prior (source, target))
joint sourcePrior channel
    | not (sameFiniteSet sourceSet (stochasticSource channel)) = Left BayesianSourceObjectMismatch
    | otherwise = do
        copied <- mapMatrix (composeStochastic (priorStochastic sourcePrior) (copyStochastic sourceSet))
        let paired = tensorStochastic (identityStochastic sourceSet) channel
        jointState <- mapMatrix (composeStochastic copied paired)
        priorFromState jointState
  where
    sourceSet = forgetNonempty (priorObject sourcePrior)

-- | Independent tensor of two exact priors.
tensorPrior ::
    (Eq left, Eq right) =>
    Prior left ->
    Prior right ->
    Either BayesianError (Prior (left, right))
tensorPrior left right = do
    let productState = tensorStochastic (priorStochastic left) (priorStochastic right)
        productSource = stochasticSource productState
        unitor = matrixFromFunction unitSet productSource (\() _ -> one)
    unitorStochastic <- mapNormalization (stochasticMatrix unitor)
    state <- mapMatrix (composeStochastic unitorStochastic productState)
    priorFromState state

{- | Compute one output's exact evidence. An output outside the represented
target is distinct from represented zero evidence.
-}
observationEvidence ::
    (Eq observation) =>
    Prior source ->
    StochasticMatrix NonNegativeRational source observation ->
    observation ->
    Either (ConditioningError observation) NonNegativeRational
observationEvidence sourcePrior channel observed = do
    output <- mapConditioningBayesian (pushforward sourcePrior channel)
    case priorMass output observed of
        Nothing -> Left (ObservationOutsideTarget observed)
        Just mass -> Right mass

{- | Condition on one represented observation. Zero evidence is returned as a
structured error and no arbitrary posterior is selected.
-}
condition ::
    (Eq source, Eq observation) =>
    Prior source ->
    StochasticMatrix NonNegativeRational source observation ->
    observation ->
    Either (ConditioningError observation) (Posterior source)
condition sourcePrior channel observed = do
    evidence <- observationEvidence sourcePrior channel observed
    if isZero evidence
        then Left (ZeroEvidence observed)
        else do
            entries <- traverse (posteriorEntry evidence) (NonEmpty.toList (finiteObjectValues (priorObject sourcePrior)))
            case prior (priorObject sourcePrior) entries of
                Left _ -> Left ConditioningInternalInvariantFailure
                Right result -> Right (Posterior result)
  where
    posteriorEntry evidence sourceValue = do
        let numerator = massOrZero sourcePrior sourceValue `times` channelMass channel sourceValue observed
        case divideNonZero numerator evidence of
            Nothing -> Left ConditioningInternalInvariantFailure
            Just posteriorValue -> Right (sourceValue, getNonNegativeRational posteriorValue)

-- | Construct the support-restricted prior-indexed Bayesian inverse.
bayesianInverse ::
    (Eq source, Eq target) =>
    Prior source ->
    StochasticMatrix NonNegativeRational source target ->
    Either BayesianError (BayesianInverse source target)
bayesianInverse sourcePrior channel = do
    outputPrior <- pushforward sourcePrior channel
    let supportedSource = supportedPrior sourcePrior
        supportedOutput = supportedPrior outputPrior
        sourceSupportSet = forgetNonempty (priorObject supportedSource)
        outputSupportSet = forgetNonempty (priorObject supportedOutput)
    inverseRows <-
        traverse
            (\targetValue -> traverse (inverseEntry outputPrior targetValue) (finiteSetValues sourceSupportSet))
            (finiteSetValues outputSupportSet)
    inverseRaw <- mapMatrix (matrixFromRows outputSupportSet sourceSupportSet inverseRows)
    inverse <- mapNormalization (stochasticMatrix inverseRaw)
    let restrictedRaw =
            matrixFromFunction sourceSupportSet outputSupportSet $ \sourceValue targetValue ->
                channelMass channel sourceValue targetValue
    restricted <- mapNormalization (stochasticMatrix restrictedRaw)
    Right (BayesianInverse supportedSource supportedOutput inverse restricted)
  where
    inverseEntry outputPrior targetValue sourceValue =
        let numerator = massOrZero sourcePrior sourceValue `times` channelMass channel sourceValue targetValue
            denominator = massOrZero outputPrior targetValue
         in case divideNonZero numerator denominator of
                Nothing -> Left BayesianDivisionByZero
                Just result -> Right result

-- | Support-restricted disintegration is prior-indexed Bayesian inversion.
disintegrate ::
    (Eq source, Eq target) =>
    Prior source ->
    StochasticMatrix NonNegativeRational source target ->
    Either BayesianError (BayesianInverse source target)
disintegrate = bayesianInverse

-- | Read the normalized inverse @Y_q -> X_p@.
inverseMatrix :: BayesianInverse source target -> StochasticMatrix NonNegativeRational target source
inverseMatrix (BayesianInverse _ _ inverse _) = inverse

-- | Read the source prior restricted to positive support.
inverseInputPrior :: BayesianInverse source target -> Prior source
inverseInputPrior (BayesianInverse input _ _ _) = input

-- | Read the pushforward prior restricted to positive support.
inverseOutputPrior :: BayesianInverse source target -> Prior target
inverseOutputPrior (BayesianInverse _ output _ _) = output

-- | Read the original channel restricted to @X_p -> Y_q@.
inverseForwardRestriction :: BayesianInverse source target -> StochasticMatrix NonNegativeRational source target
inverseForwardRestriction (BayesianInverse _ _ _ forward) = forward

{- | Compare two channels under one prior. Zero-prior rows can differ. This is
an explicit equivalence predicate, not an 'Eq' instance.
-}
almostSureEqual ::
    Prior source ->
    StochasticMatrix NonNegativeRational source target ->
    StochasticMatrix NonNegativeRational source target ->
    Either BayesianError Bool
almostSureEqual sourcePrior left right
    | not (sameFiniteSet priorSet (stochasticSource left)) = Left BayesianSourceObjectMismatch
    | not (sameFiniteSet priorSet (stochasticSource right)) = Left BayesianSourceObjectMismatch
    | not (sameFiniteSet (stochasticTarget left) (stochasticTarget right)) = Left BayesianTargetObjectMismatch
    | otherwise =
        Right
            ( and
                [ massOrZero sourcePrior sourceValue `times` channelMass left sourceValue targetValue
                    == massOrZero sourcePrior sourceValue `times` channelMass right sourceValue targetValue
                | sourceValue <- finiteSetValues priorSet
                , targetValue <- finiteSetValues (stochasticTarget left)
                ]
            )
  where
    priorSet = forgetNonempty (priorObject sourcePrior)

-- | Failure from exact distribution pushforward.
data ExactDistributionBayesianError kernelError
    = ExactDistributionKernelError !kernelError
    | ExactDistributionNormalizationError !ExactDistributionError
    deriving (Eq, Show)

-- | Failure from exact distribution conditioning.
data ExactConditioningError observation
    = ExactZeroEvidence !observation
    | ExactConditioningNormalizationError !ExactDistributionError
    deriving (Eq, Show)

{- | Canonicalize exact labelled weights by first-occurrence support order.

Each supplied weight is validated before duplicate labels are aggregated. Thus
negative entries cannot be hidden by a positive duplicate at the same label.
-}
canonicalExactDistribution ::
    (Eq value) =>
    [(value, Rational)] ->
    Either ExactDistributionError (ExactFiniteDist value)
canonicalExactDistribution entries = do
    traverse_ validateWeight (zip [0 ..] entries)
    exactFiniteDist (aggregate entries)
  where
    validateWeight (index, (_, rawWeight)) =
        case mkExactWeight rawWeight of
            Left problem -> Left (InvalidExactWeight index problem)
            Right _ -> Right ()

-- | Push an exact finite distribution through a fallible exact kernel.
pushforwardExactDistribution ::
    (Eq target) =>
    ExactFiniteDist source ->
    (source -> Either kernelError (ExactFiniteDist target)) ->
    Either (ExactDistributionBayesianError kernelError) (ExactFiniteDist target)
pushforwardExactDistribution sourceDistribution channel = do
    branches <-
        fmap concat . traverse pushOne $
            NonEmpty.toList (exactOutcomes sourceDistribution)
    either (Left . ExactDistributionNormalizationError) Right (canonicalExactDistribution branches)
  where
    pushOne (sourceValue, sourceMass) = do
        targetDistribution <- either (Left . ExactDistributionKernelError) Right (channel sourceValue)
        Right
            [ (targetValue, exactProbability sourceMass * exactProbability targetMass)
            | (targetValue, targetMass) <- NonEmpty.toList (exactOutcomes targetDistribution)
            ]

-- | Condition an exact finite distribution by one exact likelihood kernel.
conditionExactDistribution ::
    (Eq source, Eq observation) =>
    observation ->
    ExactFiniteDist source ->
    (source -> ExactFiniteDist observation) ->
    Either (ExactConditioningError observation) (ExactFiniteDist source)
conditionExactDistribution observed sourceDistribution likelihood =
    case positive of
        [] -> Left (ExactZeroEvidence observed)
        entries ->
            either
                (Left . ExactConditioningNormalizationError)
                Right
                (canonicalExactDistribution entries)
  where
    positive =
        [ (sourceValue, exactProbability sourceMass * observationMass sourceValue)
        | (sourceValue, sourceMass) <- NonEmpty.toList (exactOutcomes sourceDistribution)
        , observationMass sourceValue > 0
        ]
    observationMass sourceValue =
        sum
            [ exactProbability mass
            | (candidate, mass) <- NonEmpty.toList (exactOutcomes (likelihood sourceValue))
            , candidate == observed
            ]

buildPrior ::
    (Eq value) =>
    FiniteObject value ->
    [(value, Rational)] ->
    Either (PriorError value) (Prior value)
buildPrior object masses = do
    scalarMasses <- traverse toScalar masses
    support <-
        case finiteObject
            [ value
            | value <- NonEmpty.toList (finiteObjectValues object)
            , isNotZero (scalarFor scalarMasses value)
            ] of
            Left _ -> Left PriorInternalInvariantFailure
            Right positive -> Right (Support positive)
    let raw =
            matrixFromFunction unitSet (forgetNonempty object) $ \() value ->
                scalarFor scalarMasses value
    case stochasticMatrix raw of
        Left _ -> Left PriorInternalInvariantFailure
        Right state -> Right (Prior object support state)
  where
    toScalar (value, mass) =
        case nonNegativeRational mass of
            Left _ -> Left PriorInternalInvariantFailure
            Right scalar -> Right (value, scalar)
    isNotZero scalar = not (isZero scalar)

priorFromState ::
    (Eq value) =>
    StochasticMatrix NonNegativeRational () value ->
    Either BayesianError (Prior value)
priorFromState state = do
    object <-
        case requireNonempty (stochasticTarget state) of
            Left _ -> Left BayesianInternalInvariantFailure
            Right nonempty -> Right nonempty
    let entries =
            [ (value, getNonNegativeRational (channelMass state () value))
            | value <- NonEmpty.toList (finiteObjectValues object)
            ]
    case prior object entries of
        Left _ -> Left BayesianInternalInvariantFailure
        Right result -> Right result

massOrZero :: Prior value -> value -> NonNegativeRational
massOrZero sourcePrior value = fromMaybe zero (priorMass sourcePrior value)

channelMass ::
    StochasticMatrix NonNegativeRational source target ->
    source ->
    target ->
    NonNegativeRational
channelMass channel sourceValue targetValue =
    fromMaybe zero (matrixEntry (forgetStochastic channel) sourceValue targetValue)

scalarFor :: (Eq value) => [(value, NonNegativeRational)] -> value -> NonNegativeRational
scalarFor masses value = fromMaybe zero (lookup value masses)

aggregate :: (Eq value) => [(value, Rational)] -> [(value, Rational)]
aggregate = foldl' insert []
  where
    insert accumulated (value, mass) = go accumulated
      where
        go [] = [(value, mass)]
        go ((existing, existingMass) : remaining)
            | existing == value = (existing, existingMass + mass) : remaining
            | otherwise = (existing, existingMass) : go remaining

mapMatrix :: Either MatrixError value -> Either BayesianError value
mapMatrix = either (Left . BayesianMatrixError) Right

mapNormalization ::
    Either (StochasticMatrixError NonNegativeRational) value ->
    Either BayesianError value
mapNormalization = either (Left . BayesianNormalizationError) Right

mapConditioningBayesian :: Either BayesianError value -> Either (ConditioningError observation) value
mapConditioningBayesian = either (Left . ConditioningBayesianError) Right

unitSet :: FiniteSet ()
unitSet = UnsafeFiniteSet [()]
